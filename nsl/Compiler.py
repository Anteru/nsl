from nsl.parser import NslParser
from nsl.passes import ComputeTypes, ValidateSwizzle, ValidateFlowStatements, \
	AddImplicitCasts, DebugAst, DebugTypes, PrettyPrint, HlslCodeGen, ValidateArrayOutOfBoundsAccess

class Compiler:
    def __init__(self):
        self.parser = NslParser ()

        self.passes = [ComputeTypes.GetPass(),
			ValidateArrayOutOfBoundsAccess.GetPass (),
			ValidateFlowStatements.GetPass (),
			ValidateSwizzle.GetPass (),
			AddImplicitCasts.GetPass (),
			DebugAst.GetPass (),
			DebugTypes.GetPass (),
			PrettyPrint.GetPass (),
			HlslCodeGen.GetPass()]

    def Compile (self, source, debugParsing = False):
        ast = self.parser.Parse (source, debug = debugParsing)
        for p in self.passes:
            if not p.Process (ast):
                print ('Error in pass {}'.format (p.GetName ()))
                return False
        return True
