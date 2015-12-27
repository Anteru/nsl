from nsl import ast, Errors, types

class ValidateArrayAccessType(ast.DefaultVisitor):
    def __init__(self):
        self.valid = True
        
    def _ValidateArrayExpression(self, expr):
        rhs = expr.GetExpression ()
        
        if rhs.GetType() != types.Integer () and rhs.GetType() != types.UnsignedInteger ():
            Errors.ERROR_ARRAY_ACCESS_WITH_NONINTEGER.Raise (rhs.GetType ())
        
    def v_Expression(self, expr, ctx=None):
        if isinstance(expr, ast.ArrayExpression):
            with Errors.CompileExceptionToErrorHandler (self.errorHandler):
                self._ValidateArrayExpression(expr)

        expr.AcceptVisitor(self)
            
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateArrayAccessType(),
                                    'validate-array-access-type',
                                    validator = IsValid)