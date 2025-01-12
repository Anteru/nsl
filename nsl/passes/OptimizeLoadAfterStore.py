from nsl import Errors, types, Visitor
from .. import LinearIR

class OptimizeLoadAfterStoreVisitor (Visitor.DefaultVisitor):
    '''This visitor will replace a load immediately after a store, loading the
    value that was stored. All references to the load will be replaced to
    references to the value that was actually stored.'''
    
    def v_VariableAccessInstruction(self, vai, ctx=None):
        if vai.Store is None:
            # This is a load. Try to get the previous instruction from the same
            # basic block
            previous = vai.Parent.GetPreviousInstruction(vai)

            if previous is None:
                return
            
            if not isinstance(previous, LinearIR.VariableAccessInstruction):
                return

            # The previous instruction is a store to the same variable. Replace
            # ourselves with the value that was stored
            if previous.Variable == vai.Variable and \
               previous.Store is not None:
                # Remove all uses of this instruction with the value that was
                # stored
                vai.Parent.ReplaceUses(vai, previous.Store)

                # Remove this instruction
                vai.Parent.Replace(vai, None)
    
def GetPass():
    import nsl.Pass
    return nsl.Pass.MakePassFromVisitor(OptimizeLoadAfterStoreVisitor (),
        'optimize-load-after-store',
        flags = nsl.Pass.PassFlags.IsOptimization)
