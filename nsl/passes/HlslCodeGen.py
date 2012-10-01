from nsl import ast

def NslToHlslSemantic(semantic):
    if semantic.GetName () == 'COLOR':
        return '{}{}'.format (semantic.GetName (), semantic.GetSlot (0))
    else:
        return semantic.GetName ()

class HlslCodeGen(ast.Visitor):
    def GetContext(self):
        return 0
    
    def v_Program (self, prog, ctx):
        for type in prog.GetTypes():
            type.Accept (self, ctx)
        for func in prog.GetFunctions():
            func.Accept (self, ctx)
    
    def _FormatArgumentList(self, args):
        return ', '.join(['{0} {1}'.format(arg.GetType().GetName(), arg.GetName()) for arg in args])
    
    def v_Function(self, func, ctx=None):
        print ('{2} {0} ({1})'.format(func.GetName (),
                                      self._FormatArgumentList(func.GetArguments()),
                                      func.GetType().GetReturnType().GetName ()))
        
        func.GetBody().Accept(self, ctx)
        print()
       
    def v_Shader(self, shd, ctx=None):
        print ('{2} {0} ({1})'.format(shd.GetName(),
                                      self._FormatArgumentList(shd.GetArguments()),
                                      shd.GetType().GetReturnType ().GetName()))
        shd.GetBody().Accept(self, ctx)
        print()
        
    def _p(self, ctx, s, **args):
        print (' ' * (ctx*4), end = '')
        print (s, **args)
    
    def v_StructureDeclaration(self, decl, ctx):
        self._p (ctx, 'struct {0}'.format (decl.GetName ()))
        self._p (ctx, '{')
        for e in decl.GetElements ():
            e.Accept (self, ctx + 1)
        self._p (ctx, '}')
        print()
        
    def v_VariableDeclaration(self, decl, ctx):
        self._p (ctx, '{0} {1}'.format(decl.GetType().GetName (), decl.GetName()), end = '')
        if (decl.HasSemantic ()):
            print (': {0};'.format(NslToHlslSemantic (decl.GetSemantic())))
        else:
            print (';')
            
    def v_CompundStatement(self, cs, ctx):
        self._p(ctx, '{')
        for s in cs.GetStatements():
            s.Accept(self, ctx+1)
        self._p(ctx, '}')
        
    def v_ExpressionStatement(self, es, ctx):
        self._p(ctx, '', end = '')
        print(str(es.GetExpression()), end=';\n')
            
    def v_IfStatement(self, stmt, ctx):
        self._p(ctx, 'if ({0})'.format(str(stmt.GetCondition ())))
        
        stmt.GetTruePath().Accept (self, ctx)
        if (stmt.HasElsePath()):
            self._p(ctx, 'else')
            stmt.GetElsePath().Accept(self, ctx)
        
    def v_ReturnStatement (self, stmt, ctx):
        self._p(ctx, 'return {0};'.format(str(stmt.GetExpression())))