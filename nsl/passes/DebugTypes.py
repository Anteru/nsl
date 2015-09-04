from nsl import ast

class DebugTypeVisitor(ast.DebugPrintVisitor):
	def GetContext (self):
		return 0

	def _p(self, ctx, s, **args):
		print (' ' * (ctx * 4), end = '')
		print (s, **args)

	def v_StructureDefinition(self, decl, ctx):
		self._p (ctx, 'struct ' + decl.GetName ())
		for t in decl.GetElements ():
			# Resolve here allows for nested types
			self._p (ctx + 1, t.GetName () + ':' + str(t.GetType ()))
		print()

	def _ProcessExpression(self, expr, ctx):
		self._p (ctx, str(expr) + ':' + str(expr.type))
		for e in expr:
			self._ProcessExpression (e, ctx + 1)

	def v_CompoundStatement(self, stmt, ctx):
		for s in stmt:
			self.v_Visit (s, ctx + 1)

	def v_PrimaryExpression(self, expr, ctx):
		self._p(ctx, str(expr) + ':' + str(expr.type))

	def v_ExpressionStatement(self, stmt, ctx):
		self._ProcessExpression(stmt.GetExpression(), ctx)

	def v_Function(self, func, ctx):
		'''Computes the function type and processes all statements.'''
		self._p(ctx, str(func.GetType ()))
		ctx += 1
		self._p (ctx, 'Arguments')
		for (name, argType) in func.GetType ().GetArguments().items ():
			self._p (ctx + 1, name + ':' + str(argType))

		print ()
		self._p (ctx, 'Body')
		self.v_Visit (func.GetBody(), ctx)
		print ()
		ctx -= 1

	def v_Shader(self, shd, ctx=None):
		self.v_Function(shd, ctx)

	def v_DeclarationStatement (self, stmt, ctx):
		for decl in stmt.GetDeclarations():
			self._p (ctx, decl.GetName () + ':' + str(decl.GetType()))
		print ()

	def v_Program(self, prog, ctx):
		# Must visit types first
		for programType in prog.GetTypes ():
			self.v_Visit (programType, ctx)
		for decl in prog.GetDeclarations ():
			self.v_Visit (decl, ctx)
		for func in prog.GetFunctions ():
			self.v_Visit (func, ctx)

	def v_Generic(self, node, ctx):
		ast.Visitor.v_Generic (self, node, ctx)

def GetPass():
	import nsl.Pass
	return nsl.Pass.MakePassFromVisitor (DebugTypeVisitor (), 'debug-print-types')
