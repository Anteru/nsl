from nsl.parser import NslParser
from nsl.passes import ComputeTypes, ValidateSwizzle, ValidateFlowStatements, \
	AddImplicitCasts, DebugAst, DebugTypes, PrettyPrint, HlslCodeGen, ValidateArrayOutOfBoundsAccess,\
	ValidateArrayAccessType, UpdateLocations
from io import StringIO

class Compiler:
	def __init__(self):
		self.parser = NslParser ()

		self.passes = [
			DebugAst.GetPass (),
			UpdateLocations.GetPass (),
			DebugAst.GetPass (),
			ComputeTypes.GetPass(),
			ValidateArrayAccessType.GetPass (),
			ValidateArrayOutOfBoundsAccess.GetPass (),
			ValidateFlowStatements.GetPass (),
			ValidateSwizzle.GetPass (),
			AddImplicitCasts.GetPass (),
			DebugAst.GetPass (),
			DebugTypes.GetPass (),
			PrettyPrint.GetPass (),
			HlslCodeGen.GetPass()]

	def Compile (self, source, options):
		ast = self.parser.Parse (source, debug = options ['debug-parsing'])
		for i,p in enumerate (self.passes):
			buffer = StringIO()
			if not p.Process (ast, output=buffer):
				print ('Error in pass {}'.format (p.GetName ()))
				return False
			
			if options ['debug-passes']:
				if buffer.getvalue ():
					with open('pass-{}-{}.txt'.format (i, p.GetName ()), 'w') as outputFile:
						outputFile.write (buffer.getvalue ())
		return True
