from nsl import ast

class DebugAstVisitor(ast.DebugPrintVisitor):
    def GetContext (self):
        return 0

    def v_Generic(self, obj, ctx=None):
        ast.Visitor.v_Generic (self, obj, ctx)
        obj.AcceptVisitor(self, ctx + 1)

    def v_Default(self, obj, ctx):
        if obj.GetLocation ():
            self.Print (' '*ctx*2, obj.__class__.__name__, obj.GetLocation ())
        else:
            self.Print (' '*ctx*2, obj.__class__.__name__)
        self.Print (' '*(ctx*2 + 4), str (obj))

def GetPass():
    import nsl.Pass
    return nsl.Pass.MakePassFromVisitor (DebugAstVisitor (), 'debug-print-ast')
