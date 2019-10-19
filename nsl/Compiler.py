from nsl.parser import NslParser
from nsl.passes import (
	ComputeTypes, 
	ValidateSwizzle, 
	ValidateFlowStatements,
	AddImplicitCasts, 
	DebugAst, 
	DebugTypes,
	PrettyPrint, 
	ValidateArrayOutOfBoundsAccess,
	ValidateArrayAccessType,
	UpdateLocations,
	RewriteAssignEqualOperations,
	LowerToIR,
	PrintLinearIR,
)
from io import StringIO

class Compiler:
	def __init__(self):
		self.parser = NslParser ()

		self.astPasses = [
			DebugAst.GetPass (),
			RewriteAssignEqualOperations.GetPass (),
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
			]

		self.irPasses = [
			PrintLinearIR.GetPass ()
		]

	def __RunPass(self, data, passIndex, p, kind, debug = False):
		buffer = StringIO()
		if not p.Process (data, output=buffer):
			print (f'Error in {kind} pass {p.GetName()}')
			return False

		if debug and buffer.getvalue ():
			outputFilename = f'{kind.lower()}-pass-{passIndex}-{p.GetName()}.txt'
			with open(outputFilename, 'w') as outputFile:
				outputFile.write (buffer.getvalue ())


	def Compile (self, source, options):
		ast = self.parser.Parse (source, debug = options ['debug-parsing'])
		for i,p in enumerate (self.astPasses):
			if not self.__RunPass(ast, i, p, 'AST', options ['debug-passes']):
				return False

		# Done with the AST, we need to lower to IR now
		lowerPass = LowerToIR.GetPass ()
		if not lowerPass.Process (ast):
			print (f'Failed to lower AST to IR')
			return False

		ir = lowerPass.visitor.Program

		for i, p in enumerate(self.irPasses):
			if not self.__RunPass(ir, i, p, 'IR', options ['debug-passes']):
				return False

		return True
