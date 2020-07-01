from nsl import LinearIR, Visitor, WebAssembly
import collections
from typing import Tuple, Dict

def _ConvertType(t: LinearIR.Type) -> WebAssembly.ValueType:
	if t.IsScalar():
		if isinstance(t, LinearIR.IntegerType):
			return WebAssembly.ValueType.i32
		elif isinstance(t, LinearIR.FloatType):
			return WebAssembly.ValueType.f32

	raise Exception('Unsupported type')

def _ConvertFunctionType(ft: LinearIR.FunctionType) -> WebAssembly.FunctionType:
	argTypes = []
	resultTypes = []

	for argType in ft.Arguments.values():
		argTypes.append(_ConvertType(argType))

	resultTypes.append(_ConvertType(ft.ReturnType))

	return WebAssembly.FunctionType(argTypes, resultTypes)

def _GenerateConstant(cv: LinearIR.ConstantValue) -> WebAssembly.Instruction:
	t = cv.Type
	if t.IsScalar():
		if isinstance(t, LinearIR.IntegerType):
			return WebAssembly.Instruction(
				WebAssembly.opcodes['i32.const'],
				(cv.Value,)
			)
		elif isinstance(t, LinearIR.FloatType):
			return WebAssembly.Instruction(
				WebAssembly.opcodes['f32.const'],
				(cv.Value,)
			)

	raise Exception('Unsupported constant')


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
			self.__refToLocalMap = {}

		def SetReferenceToLocalMap(self, refToLocalMap: Dict[int, int]):
			self.__refToLocalMap = refToLocalMap

		def GetLocalForReference(self, ref):
			return self.__refToLocalMap [ref]

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

			self.__refToLocalMap = {}

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
			ctx.Code.AddInstruction(WebAssembly.Instruction(
				WebAssembly.opcodes['local.set'],
				(ctx.GetLocalForReference(vai.Reference),)
			))

	def __PushValueOntoStack(self, value: LinearIR.Value, ctx: Context):
		if isinstance(value, LinearIR.ConstantValue):
			ctx.Code.AddInstruction(_GenerateConstant(value))
		else:
			ctx.Code.AddInstruction(WebAssembly.Instruction(
				WebAssembly.opcodes['local.get'],
				(ctx.GetLocalForReference(value.Reference),)
			))

	def v_BinaryInstruction(self, bi: LinearIR.BinaryInstruction,
							ctx: Context):
		if isinstance(bi.Type, LinearIR.IntegerType):
			operationType = 'i32'
			unsigned = bi.Type.Unsigned
		elif isinstance(bi.Type, LinearIR.FloatType):
			operationType = 'f32'
		else:
			#TODO Handle error
			pass

		for value in bi.Values:
			self.__PushValueOntoStack(value, ctx)

		opCodeMap = {
			LinearIR.OpCode.ADD: 'add',
			LinearIR.OpCode.SUB: 'sub',
			LinearIR.OpCode.MUL: 'mul',
			LinearIR.OpCode.DIV: 'div',

			LinearIR.OpCode.CMP_EQ: 'eq',
		}

		opCode = f'{operationType}.{opCodeMap[bi.OpCode]}'

		if operationType == 'i32' and bi.OpCode in {LinearIR.OpCode.DIV}:
			if unsigned:
				opCode += '_u'
			else:
				opCode += '_s'

		ctx.Code.AddInstruction(WebAssembly.Instruction(
			WebAssembly.opcodes[opCode]
		))

		ctx.Code.AddInstruction(WebAssembly.Instruction(
			WebAssembly.opcodes['local.set'],
			(ctx.GetLocalForReference(bi.Reference),)
		))

	def v_ReturnInstruction(self, ri: LinearIR.ReturnInstruction, ctx: Context):
		if ri.Value:
			self.__PushValueOntoStack(ri.Value, ctx)

		ctx.Code.AddInstruction(WebAssembly.Instruction(
			WebAssembly.opcodes['return']
		))

	def v_Function(self, function: LinearIR.Function, ctx: Context = None):
		ctx.OnEnterFunction(function.Name)
		functionType = _ConvertFunctionType(function.Type)
		functionTypeIdx = ctx.Module.AddFunctionType(functionType)
		functionIdx = ctx.Module.AddFunction(functionTypeIdx)

		# Check if function is exported - for now assume yes

		c = ctx.Code

		# We create locals for every register we have
		valueReferenceTypes = {}

		for basicBlock in function.BasicBlocks:
			localValues = set()
			for instruction in basicBlock.Instructions:
				ref = instruction.Reference

				if instruction.Type.IsVoid():
					continue

				if instruction.OpCode == LinearIR.OpCode.RETURN:
					continue

				if ref not in valueReferenceTypes:
					valueReferenceTypes [ref] = instruction.Type

		valueReferenceToLocalMap = {}
		# Need to offset, as the first N locals are the function parameters
		argCount = len(functionType.Arguments)
		for ref, t in valueReferenceTypes.items ():
			valueReferenceToLocalMap [ref] = argCount + c.AddLocal (WebAssembly.Local(_ConvertType (t)))

		ctx.SetReferenceToLocalMap(valueReferenceToLocalMap)

		for basicBlock in function.BasicBlocks:
			for instruction in basicBlock.Instructions:
				self.v_Visit(instruction, ctx)

		ctx.OnLeaveFunction()


def GetPass():
	import nsl.Pass

	ctx = GenerateWasmVisitor.Context()

	return nsl.Pass.MakePassFromVisitor(GenerateWasmVisitor (ctx),
										'generate-wasm')