from nsl import Errors, types, Visitor
from .. import LinearIR

class OptimizeConstantCastVisitor (Visitor.DefaultVisitor):
    def v_CastInstruction(self, ci, ctx=None):
        value = ci.Value
        if isinstance(value, LinearIR.ConstantValue):
            constant = value.Value
            print(constant, value.Type, ci.Type)
            if ci.Type == types.Float():
                constant = float(constant)
            else:
                Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise()
            # Parent is basic block, and the parent of the basic block is
            # a function
            cv = LinearIR.ConstantValue(ci.Type, constant)
            ci.Parent.Parent.RegisterConstant(cv)
            ci.Parent.Replace(ci, cv)
        pass
    
def GetPass():
	import nsl.Pass
	return nsl.Pass.MakePassFromVisitor(OptimizeConstantCastVisitor (),
        'optimize-constant-cast')
