from nsl import ast
from .. import LinearIR

class PrintLinearIRVisitor (ast.Visitor):
    def __init__(self):
        def PrintCallback(*args, end='\n'):
            self.Print(*args, end=end)
        self.__printer = LinearIR.InstructionPrinter(PrintCallback)

    def v_Function(self, function, ctx=None):
        self.__printer.Print(function)

    def v_Program(self, program, ctx=None):
        for function in program.Functions.values():
            self.v_Visit(function, ctx)

def GetPass():
	import nsl.Pass
	return nsl.Pass.MakePassFromVisitor(PrintLinearIRVisitor (), 'print-linear-ir')
