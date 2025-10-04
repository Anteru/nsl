from nsl import Visitor
from .. import LinearIR


class RewriteFunctionArgAccessVisitor(Visitor.DefaultVisitor):
    """We want to store function arguments in a list for simple access instead
    of going through a dictionary lookup. In this pass, we will translate any
    load.arg <name> to load.arg <index> with index being the argument number."""

    def v_Function(self, function: LinearIR.Function, ctx=None):
        mapping = {arg: idx for idx, arg in enumerate(function.Type.Arguments)}

        function.AcceptVisitor(self, mapping)

    def v_VariableAccessInstruction(self, vai, ctx):
        if vai.Scope == LinearIR.VariableAccessScope.FUNCTION_ARGUMENT:
            instruction = vai.WithVariable(ctx[vai.Variable])

            return instruction


def GetPass():
    import nsl.Pass

    return nsl.Pass.MakePassFromVisitor(
        RewriteFunctionArgAccessVisitor(), "rewrite-function-arg-accessor"
    )
