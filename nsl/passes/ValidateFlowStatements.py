from nsl import Errors, Visitor

class ValidateFlowStatementVisitor(Visitor.DefaultVisitor):
    """Validate that continue/break statements only appear within loops."""
    def GetContext (self):
        return 0
    
    def __init__(self):
        super().__init__()
        self.valid = True

    def __VisitLoop(self, stmt, ctx):
        ctx +=  1
        with Errors.CompileExceptionToErrorHandler(self.errorHandler):
            stmt.AcceptVisitor(self, ctx)
        ctx -= 1

    def v_DoStatement(self, stmt, ctx):
        self.__VisitLoop(stmt, ctx)

    def v_ForStatement(self, stmt, ctx):
        self.__VisitLoop(stmt, ctx)

    def v_WhileStatement(self, stmt, ctx):
        self.__VisitLoop(stmt, ctx)
        
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