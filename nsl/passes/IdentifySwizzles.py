from nsl import Visitor

class IdentifySwizzleVisitor(Visitor.DefaultVisitor):
    '''Mark all member access expressions that are swizzles.'''
    
    def v_MemberAccessExpression (self, expr, ctx=None):
        t = expr.GetParent ().GetType()
        
        if t.IsPrimitive () and (t.IsVector () or t.IsScalar ()):
            expr.SetSwizzle (True)
            
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(IdentifySwizzleVisitor(),
                                    'identify-swizzles')