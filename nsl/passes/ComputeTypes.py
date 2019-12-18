from collections import OrderedDict
from nsl import ast, types, Errors, Visitor, LinearIR
from enum import Enum
from typing import List

def ParseSwizzleMask(mask):
	'''Parse a swizzle mask into a list of element indices, starting
	at 0.
	'''

	mapping = { 'x' : 0,
				'y' : 1,
				'z' : 2,
				'w' : 3,
				'r' : 0,
				'g' : 1,
				'b' : 2,
				'a' : 3
	}

	return [mapping [e] for e in mask]

def ComputeSwizzleType(inType, mask):
	'''Compute the resulting type of a swizzle operation.
	@param inType: Must be a PrimitiveType
	@param mask: A valid swizzle mask
	'''
	assert isinstance(inType, types.Type)
	outComponentCount = len (mask)

	swizzleType = inType.GetComponentType ()

	if outComponentCount == 1:
		return swizzleType
	else:
		result = types.VectorType (swizzleType, outComponentCount)

		return result
	
class FunctionVisitationPass(Enum):
	Register = 0
	Visit = 1

class ComputeTypeVisitor(Visitor.DefaultVisitor):
	def GetContext(self) -> List[types.Scope]:
		return [self.scope]

	def __init__(self):
		self.ok = True
		self.scope = types.Scope ()
		self.__loader = LinearIR.FilesystemModuleLoader()

	def v_StructureDefinition(self, decl, ctx):
		assert isinstance(decl, ast.StructureDefinition)
		
		scope = ctx[-1]
		fields = OrderedDict ()
		for field in decl.GetFields ():
			self.v_Visit(field, ctx)
			fields [field.GetName ()] = field.GetType ()
		structType = types.StructType(decl.GetName (), fields)
		scope.RegisterType (decl.GetName (), structType)
		decl.SetType (structType)

	def v_CompoundStatement(self, stmt, ctx):		
		ctx.append (types.Scope (ctx[-1]))
		stmt.AcceptVisitor(self, ctx)
		ctx.pop()

	def v_ForStatement(self, stmt, ctx):
		ctx.append (types.Scope (ctx[-1]))
		stmt.AcceptVisitor(self, ctx)
		ctx.pop()

	def v_DoStatement(self, stmt, ctx):
		ctx.append (types.Scope (ctx[-1]))
		stmt.AcceptVisitor(self, ctx)
		ctx.pop()

	def v_WhileStatement(self, stmt, ctx):
		ctx.append (types.Scope (ctx[-1]))
		stmt.AcceptVisitor(self, ctx)
		ctx.pop()

	def v_IfStatement(self, stmt, ctx):
		ctx.append (types.Scope (ctx[-1]))
		stmt.AcceptVisitor(self, ctx)
		ctx.pop()
		
	def _GetClassScopeForMemberAccess(self, expr, scope):
		return scope.GetFieldType(expr.GetMemberAccess().GetParent ().GetName())

	def _ProcessExpression(self, expr, scope):
		assert isinstance(expr, ast.Expression), 'Expression {1} has type {0} which is not an expression type'.format(type(expr), expr)
		# We type-cast here so we can process access trees separately
		if isinstance(expr, ast.VariableAccessExpression):
			p = expr.GetParent ()
			# Figure out the parent type
			self._ProcessExpression(p, scope)
			if isinstance (expr, ast.MemberAccessExpression):
				parentType = p.GetType()
				if parentType.IsPrimitive ():
					if parentType.IsVector () or parentType.IsScalar ():
						# We allow swizzling of vector and scalar types
						expr.SetType (ComputeSwizzleType(parentType, expr.GetMember ().GetName ()))
						expr.SetSwizzle(True)
					else:
						Errors.ERROR_CANNOT_SWIZZLE_PRIMITIVE_TYPE.Raise ()
				elif parentType.IsAggregate():
					expr.SetType (parentType.GetMembers ().GetFieldType (expr.GetMember ().GetName ()))
				else:
					Errors.ERROR_CANNOT_SWIZZLE_TYPE.Raise (parentType)
				
				expr.GetMember ().SetType (expr.GetType ())
			elif isinstance (expr, ast.ArrayExpression):
				self._ProcessExpression (expr.GetExpression (), scope)
				
				if not expr.GetExpression ().GetType ().IsScalar ():
					Errors.ERROR_ARRAY_ACCESS_WITH_NONSCALAR.Raise (expr.GetExpression ().GetType ())
				
				parentType = p.GetType()
				nestedSize = parentType.GetSize ()

				if isinstance(parentType, types.MatrixType):
					# Array access on matrix returns a vector
					arrayType = types.VectorType(parentType.GetComponentType (),
						parentType.GetColumnCount())
					expr.SetType(arrayType)
				elif len(nestedSize) > 1:
					# Drop one dimension from the array
					arrayType = types.ArrayType (parentType.GetComponentType (), nestedSize [1:])
					expr.SetType (arrayType)
				else:
					# We've reached the last dimension (array is 1D now), so
					# return the element type
					expr.SetType (p.GetType ().GetComponentType ())
		elif isinstance(expr, ast.PrimaryExpression):
			# Simply check the name
			expr.SetType (scope.GetFieldType (expr.GetName ()))
		else:
			# Walk through all children
			for c in expr:
				self._ProcessExpression(c, scope)

			# during the walking up, we can compute the expression
			# type as well

			if isinstance(expr, ast.CallExpression):
				# As we know the parameter types now, we can finally resolve
				# overloaded functions
				expr.ResolveType (scope)
				expr.SetType (expr.function.GetReturnType())
			elif isinstance (expr, ast.BinaryExpression):
				expr.ResolveType (expr.GetLeft().GetType(), expr.GetRight().GetType())
				expr.SetType (expr.GetOperator ().GetReturnType ())
			elif isinstance (expr, ast.AffixExpression):
				expr.SetType (expr.children[0].GetType ())

		return expr.GetType ()

	def v_VariableDeclaration(self, decl, ctx):
		assert isinstance(decl, ast.VariableDeclaration)
		
		scope = ctx[-1]
		scope.RegisterVariable (decl.GetName (),
						decl.ResolveType (scope))
		if decl.HasInitializerExpression():
			self._ProcessExpression(decl.GetInitializerExpression (),
									scope)

	def v_Expression(self, expr, ctx):
		self._ProcessExpression(expr, ctx[-1])

	def __RegisterFunction(self, func, ctx):
		assert isinstance(func, ast.Function)
				
		func.ResolveType (ctx [-1])
		func.GetType ().Resolve (ctx[-1])
		funcType = func.GetType()
		ctx[-1].RegisterFunction (funcType.GetName (), funcType)
			
	def v_Function(self, func, ctx):
		'''Computes the function type and processes all statements.'''
		assert isinstance(func, ast.Function)
				
		scope = types.Scope (ctx[-1])
		ctx.append (scope)
		for (name, argType) in func.GetType ().GetArgumentTypes().items ():
			scope.RegisterVariable (name, argType)

		self.v_Visit (func.GetBody(), ctx)
		ctx.pop ()

	def v_Module(self, module: ast.Module, ctx: List[types.Scope]):
		import pickle
		# Module imports are added first, so the symbols exported from a module
		# are available to everyone
		for importedModule in module.GetImports():
			irModule = self.__loader.Load(importedModule)

			for moduleType in irModule.Metadata['types']:
				assert isinstance(moduleType, types.Type)
				ctx[-1].RegisterType(moduleType.GetName(), moduleType)
			for func in irModule.Metadata['functions']:
				assert isinstance(func, types.Function)
				ctx[-1].RegisterFunction(func.GetName(), func)

		# Must visit types first
		for programType in module.GetTypes ():
			self.v_Visit (programType, ctx)
		for decl in module.GetDeclarations ():
			self.v_Visit (decl, ctx)
		
		for func in module.GetFunctions ():
			self.__RegisterFunction(func, ctx)

		for func in module.GetFunctions():
			self.v_Visit (func, ctx)

import nsl.Pass
class ComputeTypesPass(nsl.Pass.Pass):
	def __init__(self):
		import os, pickle, nsl.parser
		# register default functions and types
		self.visitor = ComputeTypeVisitor ()

	def GetName (self):
		return 'compute-types'

	def Process (self, root: ast.Module, ctx=None,output=None):
		self.visitor.Visit (root)

		return self.visitor.ok

def GetPass():
	return ComputeTypesPass ()
