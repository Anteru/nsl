from nsl import LinearIR, Visitor, WebAssembly
import collections
from typing import Tuple

def _ConvertType(t: LinearIR.Type) -> WebAssembly.ValueType:
	if t.IsScalar():
		if isinstance(t, LinearIR.IntegerType):
			return WebAssembly.ValueType.i32
		elif isinstance(t, LinearIR.FloatType):
			return WebAssembly.ValueType.f32

def _ConvertFunctionType(ft: LinearIR.FunctionType) -> WebAssembly.FunctionType:
	argTypes = []
	resultTypes = []

	for argType in ft.Arguments.values():
		argTypes.append(_ConvertType(argType))

	resultTypes.append(_ConvertType(ft.ReturnType))

	return WebAssembly.FunctionType(argTypes, resultTypes)

def _GetTemporaryCount(ft: LinearIR.Type) -> Tuple[int, int]:
	if ft.IsScalar():
		if isinstance(ft, LinearIR.FloatType):
			return 0, 1
		elif isinstance(ft, LinearIR.IntegerType):
			return 1, 0

	return 0, 0

class GenerateWasmVisitor(Visitor.DefaultVisitor):
	def __init__(self, ctx):
		self.__ctx = ctx

	def Finalize(self) -> WebAssembly.Module:
		return self.__ctx.Finalize()

	class Context:
		def __init__(self):
			self.__module = WebAssembly.Module()
			self.__code = None
			self.__functionCount = 0

		@property
		def Module(self):
			return self.__module

		def Finalize(self):
			self.__module.AddTable(WebAssembly.Table(0))
			return self.__module

		@property
		def Code(self):
			return self.__code

		def OnEnterFunction(self, functionName: str):
			self.__code = WebAssembly.Code()

			self.__module.AddExport(WebAssembly.Export(
				self.__functionCount, functionName))
			self.__functionCount += 1

		def OnLeaveFunction(self):
			self.__module.AddCode(self.__code)
			self.__code = None

	def GetContext(self):
		return self.__ctx

	def v_VariableAccessInstruction(self, vai: LinearIR.VariableAccessInstruction,
									ctx: Context):
		if vai.Scope == LinearIR.VariableAccessScope.FUNCTION_ARGUMENT:
			index = vai.Variable
			ctx.Code.AddInstruction(WebAssembly.Instruction(
				WebAssembly.opcodes['local.get'],
				(index,)
			))

	def v_BinaryInstruction(self, bi: LinearIR.BinaryInstruction,
							ctx: Context):
		if isinstance(bi.Type, LinearIR.IntegerType):
			operationType = 'i32'
		elif isinstance(bi.Type, LinearIR.FloatType):
			operationType = 'f32'
		else:
			#TODO Handle error
			pass

		opCodeMap = {
			LinearIR.OpCode.ADD: 'add',
			LinearIR.OpCode.SUB: 'sub',
			LinearIR.OpCode.MUL: 'mul',
			LinearIR.OpCode.DIV: 'div'
		}

		opCode = f'{operationType}.{opCodeMap[bi.OpCode]}'

		ctx.Code.AddInstruction(WebAssembly.Instruction(
			WebAssembly.opcodes[opCode]
		))

	def v_Function(self, function: LinearIR.Function, ctx: Context = None):
		ctx.OnEnterFunction(function.Name)
		functionType = _ConvertFunctionType(function.Type)
		functionTypeIdx = ctx.Module.AddFunctionType(functionType)
		functionIdx = ctx.Module.AddFunction(functionTypeIdx)

		# Check if function is exported - for now assume yes

		c = ctx.Code

		for basicBlock in function.BasicBlocks:
			for instruction in basicBlock.Instructions:
				self.v_Visit(instruction, ctx)

		ctx.OnLeaveFunction()


def GetPass():
	import nsl.Pass

	ctx = GenerateWasmVisitor.Context()

	return nsl.Pass.MakePassFromVisitor(GenerateWasmVisitor (ctx),
										'generate-wasm')