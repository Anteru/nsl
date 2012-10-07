from nsl.parser import NslParser

class Compiler:
    def __init__(self):
        import nsl.passes.ComputeTypes, nsl.passes.ValidateFlowStatements, nsl.passes.DebugPrint, nsl.passes.ValidateSwizzle, nsl.passes.PrettyPrint
        self.parser = NslParser ()

        self.passes = [nsl.passes.ComputeTypes.GetPass(),
                  nsl.passes.ValidateFlowStatements.GetPass (),
                  nsl.passes.ValidateSwizzle.GetPass (),
                  nsl.passes.DebugPrint.GetPass (),
                  nsl.passes.PrettyPrint.GetPass ()]

    def Compile (self, source, debugParsing = False):
        ast = self.parser.Parse (source, debug = debugParsing)
        for p in self.passes:
            if not p.Process (ast):
                print ('Error in pass {}'.format (p.GetName ()))

