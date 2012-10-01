import nsl.ast

def GetPass():
    import nsl.Pass
    return nsl.Pass.MakePassFromVisitor (nsl.ast.DebugVisitor (), 'debug-print')