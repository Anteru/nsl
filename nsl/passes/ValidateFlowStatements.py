from nsl import ast, Errors

class ValidateFlowStatementVisitor(ast.DefaultVisitor):
    def GetContext (self):
        return 0
    
    def __init__(self):
        self.valid = True
    
    def v_FlowStatement(self, stmt, ctx):
        ctx += 1
        stmt.AcceptVisitor(self, ctx)
        ctx -= 1

    def v_ForStatement(self, stmt, ctx):
        self.FlowStatement (stmt, ctx)
        
    def v_WhileStatement(self, stmt, ctx):
        self.FlowStatement (stmt, ctx)
        
    def v_DoStatement (self, stmt, ctx):
        self.FlowStatement (stmt, ctx)
        
    def v_ContinueStatement(self, stmt, ctx):
        Errors.ERROR_CONTINUE_OUTSIDE_FLOW.Raise ()
        self.valid = False
        
    def v_BreakStatement(self, stmt, ctx):
        Errors.ERROR_BREAK_OUTSIDE_FLOW_SWITCH.Raise ()
        self.valid = False
        
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateFlowStatementVisitor (),
                                    'validate-flow-statements',
                                    validator = IsValid)