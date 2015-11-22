from nsl import ast

class PrettyPrintVisitor(ast.Visitor):
    def v_Program (self, prog, ctx):
        for programType in prog.GetTypes():
            self.v_Visit (programType, ctx)
        for decl in prog.GetDeclarations():
            self.v_Visit (decl, ctx)
        for func in prog.GetFunctions():
            self.v_Visit (func, ctx)

    def v_Generic(self, node, ctx):
        ast.Visitor.v_Generic(self, node, ctx)

    def GetContext(self):
        return 0

    def _FormatArgumentList(self, args):
        def FormatArg(arg):
            if arg.HasName ():
                return str(arg)
            else:
                return '{} /* unnamed */'.format (arg.GetType ())

        return ', '.join([FormatArg(arg) for arg in args])

    def v_Function(self, func, ctx=None):
        if func.isForwardDeclaration:
            print ('__declaration function {0} ({1}) -> {2};'.format(func.GetName (),
                                                  self._FormatArgumentList(func.GetArguments()),
                                                  func.GetType ().GetReturnType().GetName ()))
        else:
            print ('function {0} ({1}) -> {2}'.format(func.GetName (),
                                                  self._FormatArgumentList(func.GetArguments()),
                                                  func.GetType ().GetReturnType().GetName ()))
            self.v_Visit (func.GetBody (), ctx)
        print()

    def v_Shader(self, shd, ctx=None):
        print ('shader({0}) ({1}) -> {2}'.format(shd.GetShaderType().name.lower(),
                                          self._FormatArgumentList(shd.GetArguments()),
                                          shd.GetType().GetReturnType().GetName ()))
        self.v_Visit (shd.GetBody (), ctx)
        print()

    def _p(self, ctx, s, **args):
        print (' ' * (ctx * 4), end = '')
        print (s, **args)

    def v_BreakStatement(self, s, c):
        self._p (c, 'break;')

    def v_ContinueStatement(self, s, c):
        self._p (c, 'continue;')

    def v_StructureDefinition(self, decl, ctx):
        for annotation in decl.GetAnnotations():
            self._p (ctx, annotation)
        self._p (ctx, 'struct {0}'.format (decl.GetName ()))
        self._p (ctx, '{')
        for e in decl.GetElements ():
            self.v_Visit (e, ctx + 1)
        self._p (ctx, '}')
        print()

    def v_VariableDeclaration(self, decl, ctx):
        self._p (ctx, '{0} {1}'.format(decl.GetType().GetName (), decl.GetName()), end = '')
        if (decl.HasSemantic ()):
            print (': {0};'.format(str(decl.GetSemantic ())))
        else:
            print (';')

    def v_DeclarationStatement(self, decl, ctx):
        for d in decl.GetDeclarations ():
            self.v_Visit (d, ctx)
        print ()

    def v_CompoundStatement(self, cs, ctx):
        self._p(ctx, '{')
        for s in cs.GetStatements():
            self.v_Visit (s, ctx+1)
        self._p(ctx, '}')

    def v_ExpressionStatement(self, es, ctx):
        self._p(ctx, '', end = '')
        print(str(es.GetExpression()), end=';\n')

    def v_IfStatement(self, stmt, ctx):
        self._p(ctx, 'if ({0})'.format(str(stmt.GetCondition ())))

        self.v_Visit (stmt.GetTruePath (), ctx)
        if (stmt.HasElsePath()):
            self._p(ctx, 'else')
            self.v_Visit (stmt.GetElsePath(), ctx)

    def v_ReturnStatement (self, stmt, ctx):
        self._p(ctx, 'return {0};'.format(str(stmt.GetExpression())))


def GetPass():
    import nsl.Pass
    return nsl.Pass.MakePassFromVisitor(PrettyPrintVisitor (), 'pretty-print')
