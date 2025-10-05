from nsl import Visitor


class DebugAstVisitor(Visitor.DefaultVisitor):
    def GetContext(self) -> int:
        return 0

    def v_Generic(self, obj, ctx=0):
        self.v_Default(obj, ctx)
        obj.AcceptVisitor(self, ctx + 1)

    def v_Default(self, obj, ctx: int):
        if obj.GetLocation():
            self.Print(" " * ctx * 2, obj.__class__.__name__, obj.GetLocation())
        else:
            self.Print(" " * ctx * 2, obj.__class__.__name__)
        self.Print(" " * (ctx * 2 + 4), str(obj))


def GetPass():
    import nsl.Pass

    return nsl.Pass.MakePassFromVisitor(DebugAstVisitor(), "debug-print-ast")
