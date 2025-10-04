from nsl import Errors, Visitor
from .. import LinearIR


class OptimizeConstantCastVisitor(Visitor.DefaultVisitor):
    """This visitor replaces all cast instructions which reference a constant
    that is equal to the cast result."""

    def v_CastInstruction(self, ci, ctx=None):
        value = ci.Value
        if isinstance(value, LinearIR.ConstantValue):
            constant = value.Value
            if isinstance(ci.Type, LinearIR.FloatType):
                constant = float(constant)
            else:
                Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                    f"Cannot cast constant {ci.Value} to type {ci.Type}"
                )
            # Parent is basic block, and the parent of the basic block is
            # a function
            cv = ci.Parent.Parent.CreateConstant(ci.Type, constant)
            ci.Parent.Replace(ci, cv)


def GetPass():
    import nsl.Pass

    return nsl.Pass.MakePassFromVisitor(
        OptimizeConstantCastVisitor(),
        "optimize-constant-cast",
        flags=nsl.Pass.PassFlags.IsOptimization,
    )
