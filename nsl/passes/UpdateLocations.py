from nsl import ast, Visitor

class UpdateLocationsVisitor(Visitor.DefaultVisitor):
    def v_Generic(self, obj, ctx=None):
        obj.AcceptVisitor(self)
        
        locations = []
        if obj.GetLocation ():
            locations.append(obj.GetLocation ())
        def GetLocation(c, ctx=None):
            l = c.GetLocation ()
            if l:
                locations.append (l)
    
        obj.ForEachChild(GetLocation)
        
        if locations:
            obj.SetLocation (ast.Location.Merge (*locations))

def GetPass():
    import nsl.Pass
    return nsl.Pass.MakePassFromVisitor (UpdateLocationsVisitor (), 'update-locations')
