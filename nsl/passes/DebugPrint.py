from nsl import ast

class DebugVisitor(ast.Visitor):
    def GetContext (self):
        return 0

    def v_Generic(self, obj, ctx=None):
        ast.Visitor.v_Generic (self, obj, ctx)
        if hasattr (obj, 'Traverse'):
            obj.Traverse (self, ctx + 1)

    def v_Default(self, obj, ctx):
        print (' '*ctx*2, obj.__class__.__name__)
        print (' '*(ctx*2 + 4), str (obj))

def GetPass():
    import nsl.Pass
    return nsl.Pass.MakePassFromVisitor (DebugVisitor (), 'debug-print')