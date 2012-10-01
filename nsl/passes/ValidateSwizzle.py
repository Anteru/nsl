from nsl import ast

def ValidateSwizzleMask(mask):
    from .. import Utility, Errors

    for m in mask:
        if m not in 'xyzwrgba':
            Errors.ERROR_INVALID_SWIZZLE_MASK.Raise ()

    if (Utility.ContainsAnyOf(mask, 'xyzw')):
        if (Utility.ContainsAnyOf (mask, 'rgba')):
            Errors.ERROR_MIXED_SWIZZLE_MASK.Raise ()
    elif (Utility.ContainsAnyOf(mask, 'rgba')):
        if (Utility.ContainsAnyOf (mask, 'xyzw')):
            Errors.ERROR_MIXED_SWIZZLE_MASK.Raise ()

class ValidateSwizzleMaskVisitor(ast.DefaultVisitor):
    def GetContext (self):
        return 0
    
    def __init__(self):
        self.valid = True
    
    def v_MemberAccessExpression (self, expr, ctx=None):
        import nsl.Errors
        t = expr.GetParent ().type
        
        with nsl.Errors.CompileExceptionToErrorHandler (self.errorHandler):
            if t.IsPrimitive () and (t.IsVector () or t.IsScalar ()):
                ValidateSwizzleMask (expr.GetMember ())
            
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateSwizzleMaskVisitor(),
                                    'validate-swizzle-mask',
                                    validator = IsValid)