from nsl import Visitor


def ValidateSwizzleMask(mask):
    from .. import Utility, Errors

    if any([m not in "xyzwrgba" for m in mask]):
        Errors.ERROR_INVALID_SWIZZLE_MASK.Raise()

    if Utility.ContainsAnyOf(mask, "xyzw") and Utility.ContainsAnyOf(
        mask, "rgba"
    ):
        Errors.ERROR_MIXED_SWIZZLE_MASK.Raise()
    elif Utility.ContainsAnyOf(mask, "rgba") and Utility.ContainsAnyOf(
        mask, "xyzw"
    ):
        Errors.ERROR_MIXED_SWIZZLE_MASK.Raise()


class ValidateSwizzleMaskVisitor(Visitor.DefaultVisitor):
    """Validate swizzle masks on vector types."""

    def __init__(self):
        self.valid = True

    def v_MemberAccessExpression(self, expr, ctx=None):
        import nsl.Errors

        t = expr.GetParent().GetType()

        with nsl.Errors.CompileExceptionToErrorHandler(self.errorHandler):
            if t.IsPrimitive() and (t.IsVector() or t.IsScalar()):
                ValidateSwizzleMask(expr.GetMember())


def GetPass():
    from nsl import Pass

    def IsValid(visitor):
        return visitor.valid

    return Pass.MakePassFromVisitor(
        ValidateSwizzleMaskVisitor(), "validate-swizzle-mask", validator=IsValid
    )
