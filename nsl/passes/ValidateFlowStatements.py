from nsl import Errors, Visitor

class ValidateFlowStatementVisitor(Visitor.DefaultVisitor):
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
        
    def v_ContinueStatement(self, stmt, ctx):
        if ctx == 0:
            self.valid = False
            Errors.ERROR_CONTINUE_OUTSIDE_FLOW.Raise ()
        
    def v_BreakStatement(self, stmt, ctx):
        if ctx == 0:
            self.valid = False
            Errors.ERROR_BREAK_OUTSIDE_FLOW_SWITCH.Raise ()
        
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateFlowStatementVisitor (),
                                    'validate-flow-statements',
                                    validator = IsValid)