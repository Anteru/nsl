from nsl import ast, Visitor

class PrettyPrintVisitor(Visitor.DefaultVisitor):
	def v_Module (self, module, ctx):
		for programType in module.GetTypes():
			self.v_Visit (programType, ctx)
		for decl in module.GetDeclarations():
			self.v_Visit (decl, ctx)
		for func in module.GetFunctions():
			self.v_Visit (func, ctx)

	def GetContext(self):
		return 0

	def __FormatArgumentList(self, args):
		def FormatArg(arg):
			if arg.HasName ():
				return str(arg)
			else:
				return '{} /* unnamed */'.format (arg.GetType ())

		return ', '.join([FormatArg(arg) for arg in args])

	def v_Function(self, func, ctx=None):
		if func.isForwardDeclaration:
			self.Print ('__declaration function {0} ({1}) -> {2};'.format(func.GetName (),
												  self.__FormatArgumentList(func.GetArguments()),
												  func.GetType ().GetReturnType().GetName ()))
		else:
			self.Print ('function {0} ({1}) -> {2}'.format(func.GetName (),
												  self.__FormatArgumentList(func.GetArguments()),
												  func.GetType ().GetReturnType().GetName ()))
			self.v_Visit (func.GetBody (), ctx)
		self.Print()

	def _p(self, ctx, s, **args):
		self.Print (' ' * (ctx * 4), end = '')
		self.Print (s, **args)

	def v_BreakStatement(self, s, c):
		self._p (c, 'break;')

	def v_ContinueStatement(self, s, c):
		self._p (c, 'continue;')

	def v_StructureDefinition(self, decl, ctx):
		for annotation in decl.GetAnnotations():
			self._p (ctx, annotation)
		self._p (ctx, 'struct {0}'.format (decl.GetName ()))
		self._p (ctx, '{')
		for field in decl.GetFields ():
			self.v_Visit (field, ctx + 1)
		self._p (ctx, '}')
		self.Print()

	def v_VariableDeclaration(self, decl, ctx):
		if decl.GetType().IsArray():
			self._p (ctx, '{0} {1}{2}'.format(decl.GetType().GetComponentType(), 
				decl.GetName(), ''.join(['[{}]'.format(s) for s in decl.GetType().GetSize()])), end = '')
		else:
			self._p (ctx, '{0} {1}'.format(decl.GetType().GetName (), decl.GetName()), end = '')

		self.Print (';')

	def v_DeclarationStatement(self, decl, ctx):
		for d in decl.GetDeclarations ():
			self.v_Visit (d, ctx)
		self.Print ()

	def v_CompoundStatement(self, cs, ctx):
		self._p(ctx, '{')
		for s in cs.GetStatements():
			self.v_Visit (s, ctx+1)
		self._p(ctx, '}')

	def v_ExpressionStatement(self, es, ctx):
		self._p(ctx, '', end = '')
		self.Print(str(es.GetExpression()), end=';\n')

	def v_IfStatement(self, stmt, ctx):
		self._p(ctx, 'if ({0})'.format(str(stmt.GetCondition ())))

		self.v_Visit (stmt.GetTruePath (), ctx)
		if (stmt.HasElsePath()):
			self._p(ctx, 'else')
			self.v_Visit (stmt.GetElsePath(), ctx)

	def v_ReturnStatement (self, stmt, ctx):
		self._p(ctx, 'return {0};'.format(str(stmt.GetExpression())))

	def v_ForStatement(self, stmt, ctx):
		self._p(ctx, 'for ({0}; {1}; {2})'.format (stmt.GetInitialization(),
			stmt.GetCondition(), stmt.GetNext()))
		self.v_Visit(stmt.GetBody(), ctx)


def GetPass():
	import nsl.Pass
	return nsl.Pass.MakePassFromVisitor(PrettyPrintVisitor (), 'pretty-print')
