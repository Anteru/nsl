from nsl import ast, Visitor


class DebugTypeVisitor(Visitor.DefaultVisitor):
    def GetContext(self):
        return 0

    def _p(self, ctx, s, **args):
        self.Print(" " * (ctx * 4), end="")
        self.Print(s, **args)

    def v_StructureDefinition(self, decl, ctx):
        self._p(ctx, "struct " + decl.GetName())
        for field in decl.GetFields():
            # Resolve here allows for nested types
            self._p(ctx + 1, field.GetName() + ":" + str(field.GetType()))
        self.Print()

    def v_Expression(self, expr, ctx):
        self._p(ctx, str(expr) + ":" + str(expr.GetType()))
        expr.AcceptVisitor(self, ctx + 1)

    def v_CompoundStatement(self, stmt, ctx):
        for s in stmt:
            self.v_Visit(s, ctx + 1)

    def v_Function(self, func, ctx):
        self._p(ctx, str(func.GetType()))
        ctx += 1
        self._p(ctx, "Arguments")
        for name, argType in func.GetType().GetArgumentTypes().items():
            self._p(ctx + 1, name + ":" + str(argType))

        self.Print()
        self._p(ctx, "Body")
        self.v_Visit(func.GetBody(), ctx)
        self.Print()
        ctx -= 1

    def v_DeclarationStatement(self, stmt, ctx):
        for decl in stmt.GetDeclarations():
            self._p(ctx, decl.GetName() + ":" + str(decl.GetType()))
        self.Print()


def GetPass():
    import nsl.Pass

    return nsl.Pass.MakePassFromVisitor(DebugTypeVisitor(), "debug-print-types")
