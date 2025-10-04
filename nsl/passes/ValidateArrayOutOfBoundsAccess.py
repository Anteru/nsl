from nsl import ast, Errors, Visitor


class ValidateArrayOutOfBoundsAccessVisitor(Visitor.DefaultVisitor):
    """Validate that accesses to an array using constants are within bounds."""

    def __init__(self):
        super().__init__()
        self.valid = True

    def _ValidateArrayExpression(self, expr):
        rhs = expr.GetExpression()

        if isinstance(rhs, ast.LiteralExpression):
            arrayType = expr.GetParent().GetType()
            lastDimensionSize = arrayType.GetSize()[-1]
            accessValue = rhs.GetValue()

            if lastDimensionSize <= accessValue:
                self.valid = False
                Errors.ERROR_ARRAY_ACCESS_OUT_OF_BOUNDS.Raise(
                    lastDimensionSize, accessValue
                )

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
        ValidateArrayOutOfBoundsAccessVisitor(),
        "validate-array-out-of-bounds-access",
        validator=IsValid,
    )
