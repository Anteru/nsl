from nsl import LinearIR, Visitor, WebAssembly
import collections

def _ConvertType(t: LinearIR.Type) -> WebAssembly.Type:
	if t.IsScalar():
		if isinstance(t, LinearIR.IntegerType):
			return WebAssembly.Type.i32
		elif isinstance(t, LinearIR.FloatType):
			return WebAssembly.Type.f32

def _ConvertFunctionType(ft: LinearIR.FunctionType) -> WebAssembly.FunctionType:
	argTypes = []
	resultTypes = []

	for argType in ft.Arguments.values():
		argTypes.append(_ConvertType(argType))

	resultTypes.append(_ConvertType(ft.ReturnType))

	return WebAssembly.FunctionType(argTypes, resultTypes)

class GenerateWasmVisitor(Visitor.DefaultVisitor):
	def __init__(self, ctx):
		self.__ctx = ctx

	@property
	def Module(self):
		return self.__ctx.Module

	class Context:
		def __init__(self):
			self.__module = WebAssembly.Module()

		@property
		def Module(self):
			return self.__module

	def GetContext(self):
		return self.__ctx

	def v_Function(self, function: LinearIR.Function, ctx: Context =None):
		functionType = _ConvertFunctionType(function.Type)
		ctx.Module.AddFunctionType(functionType)

def GetPass():
	import nsl.Pass

	ctx = GenerateWasmVisitor.Context()

	return nsl.Pass.MakePassFromVisitor(GenerateWasmVisitor (ctx),
										'generate-wasm')