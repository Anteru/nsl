from nsl import ast, Errors

class ValidateFlowStatementVisitor(ast.DefaultVisitor):
    def GetContext (self):
        return 0
    
    def __init__(self):
        super().__init__()
        self.valid = True
    
    def v_FlowStatement(self, stmt, ctx):
        ctx += 1
        with Errors.CompileExceptionToErrorHandler (self.errorHandler):
            stmt.AcceptVisitor(self, ctx)
        ctx -= 1

    def v_ForStatement(self, stmt, ctx):
        self.v_FlowStatement (stmt, ctx)
        
    def v_WhileStatement(self, stmt, ctx):
        self.v_FlowStatement (stmt, ctx)
        
    def v_DoStatement (self, stmt, ctx):
        self.v_FlowStatement (stmt, ctx)
        
    def v_ContinueStatement(self, stmt, ctx):
        self.valid = False
        Errors.ERROR_CONTINUE_OUTSIDE_FLOW.Raise ()
        
    def v_BreakStatement(self, stmt, ctx):
        self.valid = False
        Errors.ERROR_BREAK_OUTSIDE_FLOW_SWITCH.Raise ()
        
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateFlowStatementVisitor (),
                                    'validate-flow-statements',
                                    validator = IsValid)