from collections import OrderedDict
from nsl import ast, types, Errors
from enum import Enum

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

	swizzleType = inType.GetElementType ()

	if outComponentCount == 1:
		return swizzleType
	else:
		result = types.VectorType (swizzleType, outComponentCount)
		# Copy semantic if needed
		if inType.HasSemantic ():
			result.SetSemantic (inType.GetSemantic ())
		return result
	
class FunctionVisitationPass(Enum):
	Register = 0
	Visit = 1

class ComputeTypeVisitor(ast.DefaultVisitor):
	def GetContext(self):
		return [self.scope]

	def __init__(self):
		self.ok = True
		self.scope = types.Scope ()

	def v_StructureDefinition(self, decl, ctx):
		assert isinstance(decl, ast.StructureDefinition)
		
		scope = ctx[-1]
		fields = OrderedDict ()
		for field in decl.GetFields ():
			# Resolve here allows for nested types
			fields [field.GetName ()] = types.ResolveType (field.GetType (), scope)
		structType = types.StructType(decl.GetName (), fields)
		scope.RegisterType (decl.GetName (), structType)
		decl.SetType (structType)

	def v_InterfaceDefinition (self, decl, ctx):
		assert isinstance(decl, ast.InterfaceDefinition)
		
		scope = ctx[-1]
		methods = []
		for method in decl.GetMethods ():
			# Resolve here allows for nested types
			methodType = method.GetType ()
			methodType.Resolve (scope)
			methods.append (methodType)
		classType = types.ClassType(decl.GetName (), dict(), methods, isInterface=True)
		scope.RegisterType (decl.GetName (), classType)
		decl.SetType (classType)

	def v_CompoundStatement(self, stmt, ctx):
		assert isinstance(stmt, ast.CompoundStatement)
		
		ctx.append (types.Scope (ctx[-1]))
		for s in stmt:
			self.v_Visit (s, ctx)
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
				if p.GetType().IsPrimitive ():
					if p.GetType().IsVector () or p.GetType().IsScalar ():
						# We allow both swizzling of vector and scalar types
						expr.SetType (ComputeSwizzleType(p.GetType(), expr.GetMember ().GetName ()))
					else:
						Errors.ERROR_CANNOT_SWIZZLE_PRIMITIVE_TYPE.Raise ()
				elif isinstance (p.GetType(), types.StructType):
					expr.SetType (p.GetType().GetMembers ().GetFieldType (expr.GetMember ().GetName ()))
				else:
					Errors.ERROR_CANNOT_SWIZZLE_TYPE.Raise (p.GetType())
				
				expr.GetMember ().SetType (expr.GetType ())
			elif isinstance (expr, ast.ArrayExpression):
				self._ProcessExpression (expr.GetExpression (), scope)
				
				if not expr.GetExpression ().GetType ().IsScalar ():
					Errors.ERROR_ARRAY_ACCESS_WITH_NONSCALAR.Raise (expr.GetExpression ().GetType ())
				
				nestedSize = p.GetType().GetSize ()
				if len(nestedSize) > 1:
					# Drop one dimension from the array
					arrayType = types.ArrayType (p.GetType ().GetElementType (), nestedSize [1:])
					expr.SetType (arrayType)
				else:
					# We've reached the last dimension (array is 1D now), so
					# return the element type
					expr.SetType (p.GetType ().GetElementType ())
		elif isinstance(expr, ast.PrimaryExpression):
			# Simply check the name
			expr.SetType (scope.GetFieldType (expr.GetName ()))
		else:
			# Walk through all children
			for c in expr:
				self._ProcessExpression(c, scope)

			# during the walking up, we can compute the expression
			# type as well

			if isinstance(expr, ast.MethodCallExpression):
				# We resolve the method call with the scope of the class/interface
				# The arguments have been already resolved above
				expr.ResolveType(self._GetClassScopeForMemberAccess(expr, scope))
				expr.SetType(expr.function.GetReturnType ())
			elif isinstance(expr, ast.CallExpression):
				# As we know the parameter types now, we can finally resolve
				# overloaded functions
				expr.ResolveType (scope)
				expr.SetType (expr.function.GetReturnType())
			elif isinstance (expr, ast.BinaryExpression):
				expr.ResolveType (expr.GetLeft().GetType(), expr.GetRight().GetType())
				expr.SetType (expr.GetOperator ().GetReturnType ())

		return expr.GetType ()

	def v_DeclarationStatement(self, stmt, ctx):
		assert isinstance(stmt, ast.DeclarationStatement)
		
		scope = ctx[-1]
		for decl in stmt.GetDeclarations():
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
		if not isinstance (func, ast.Shader):
			ctx[-1].RegisterFunction (func.GetType ().GetName (), func.GetType ())
			
	def v_Function(self, func, ctx):
		'''Computes the function type and processes all statements.'''
		assert isinstance(func, ast.Function)
				
		scope = types.Scope (ctx[-1])
		ctx.append (scope)
		for (name, argType) in func.GetType ().GetArgumentTypes().items ():
			scope.RegisterVariable (name, argType)

		self.v_Visit (func.GetBody(), ctx)
		ctx.pop ()

	def v_Shader(self, shd, ctx=None):
		self.v_Function(shd, ctx)

	def v_Program(self, prog, ctx):
		# Must visit types first
		for programType in prog.GetTypes ():
			self.v_Visit (programType, ctx)
		for decl in prog.GetDeclarations ():
			self.v_Visit (decl, ctx)
		
		for func in prog.GetFunctions ():
			self.__RegisterFunction(func, ctx)

		for func in prog.GetFunctions():
			self.v_Visit (func, ctx)

import nsl.Pass
class ComputeTypesPass(nsl.Pass.Pass):
	def __init__(self):
		import os, pickle, nsl.parser
		# register default functions and types
		self.visitor = ComputeTypeVisitor ()
		stdlib = None
		if not os.path.exists ('stdlib.cache') or True:
			# generate the std lib first
			stdlib_vis = ComputeTypeVisitor ()
			p = nsl.parser.NslParser ()
			stdlib_ast = p.Parse (open ('nsl/stdlib.nsl').read ())
			stdlib_vis.Visit(stdlib_ast)
			stdlib = stdlib_vis.scope
			pickle.dump(stdlib, open ('stdlib.cache', 'wb'))
		else:
			stdlib = pickle.load(open ('stdlib.cache', 'rb'))
		self.visitor.scope = stdlib

	def GetName (self):
		return 'compute-types'

	def Process (self, ast, ctx=None,output=None):
		self.visitor.Visit (ast)

		return self.visitor.ok

def GetPass():
	return ComputeTypesPass ()
