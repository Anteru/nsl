from nsl import ast, Errors, types, Visitor


class ValidateArrayAccessTypeVisitor(Visitor.DefaultVisitor):
    """Validate that array indices have integer type."""

    def __init__(self):
        super().__init__()
        self.valid = True

    def _ValidateArrayExpression(self, expr):
        rhsType = expr.GetExpression().GetType()

        if rhsType != types.Integer() and rhsType != types.UnsignedInteger():
            self.valid = False
            Errors.ERROR_ARRAY_ACCESS_WITH_NONINTEGER.Raise(rhsType)

    def v_Expression(self, expr, ctx=None):
        if isinstance(expr, ast.ArrayExpression):
            with Errors.CompileExceptionToErrorHandler(self.errorHandler):
                self._ValidateArrayExpression(expr)

        expr.AcceptVisitor(self)


def GetPass():
    from nsl import Pass

    def IsValid(visitor):
        return visitor.valid

    return Pass.MakePassFromVisitor(
        ValidateArrayAccessTypeVisitor(),
        "validate-array-access-type",
        validator=IsValid,
    )
