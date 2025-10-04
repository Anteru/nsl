from nsl.parser import NslParser
from nsl.passes import (
    AddImplicitCasts,
    ComputeTypes,
    DebugAst,
    DebugTypes,
    GenerateWasm,
    LowerToIR,
    OptimizeConstantCasts,
    OptimizeLoadAfterStore,
    PrettyPrint,
    PrintLinearIR,
    RewriteAssignEqualOperations,
    RewriteFunctionArgAccess,
    UpdateLocations,
    ValidateArrayAccessType,
    ValidateArrayOutOfBoundsAccess,
    ValidateExportedFunctions,
    ValidateFlowStatements,
    ValidateSwizzle,
    ValidateVariableNames,
)
from nsl import LinearIR, WebAssembly
from io import StringIO
from typing import Optional


class Compiler:
    class Result:
        def __init__(
            self,
            *,
            irModule: Optional[LinearIR.Module] = None,
            wasmModule: Optional[WebAssembly.Module] = None,
        ):
            self.__irModule = irModule
            self.__wasmModule = wasmModule

        @property
        def IRModule(self) -> Optional[LinearIR.Module]:
            return self.__irModule

        @property
        def WasmModule(self) -> Optional[WebAssembly.Module]:
            return self.__wasmModule

    def __init__(self):
        self.parser = NslParser()

        self.astPasses = [
            DebugAst.GetPass(),
            RewriteAssignEqualOperations.GetPass(),
            DebugAst.GetPass(),
            UpdateLocations.GetPass(),
            DebugAst.GetPass(),
            ComputeTypes.GetPass(),
            ValidateArrayAccessType.GetPass(),
            ValidateArrayOutOfBoundsAccess.GetPass(),
            ValidateExportedFunctions.GetPass(),
            ValidateFlowStatements.GetPass(),
            ValidateSwizzle.GetPass(),
            ValidateVariableNames.GetPass(),
            AddImplicitCasts.GetPass(),
            DebugAst.GetPass(),
            DebugTypes.GetPass(),
            PrettyPrint.GetPass(),
        ]

        self.irPasses = [
            RewriteFunctionArgAccess.GetPass(),
            OptimizeConstantCasts.GetPass(),
            OptimizeLoadAfterStore.GetPass(),
            PrintLinearIR.GetPass(),
        ]

    def __RunPass(self, data, passIndex, p, kind, debug=False):
        buffer = StringIO()
        if not p.Process(data, output=buffer):
            print(f"Error in {kind} pass {p.GetName()}")
            return False

        if debug and buffer.getvalue():
            outputFilename = f"{kind.lower()}-pass-{passIndex}-{p.Name}.txt"
            with open(outputFilename, "w", encoding="utf-8") as outputFile:
                outputFile.write(buffer.getvalue())

        return True

    def Compile(self, source, options={}) -> Optional[Result]:
        from nsl.Pass import PassFlags

        debugParsing = options.get("debug-parsing", False)
        debugPasses = options.get("debug-passes", False)
        optimizations = options.get("optimize", False)

        ast = self.parser.Parse(source, debug=debugParsing)
        for i, p in enumerate(self.astPasses):
            if not self.__RunPass(ast, i, p, "AST", debugPasses):
                return None

        # Done with the AST, we need to lower to IR now
        lowerPass = LowerToIR.GetPass()
        if not lowerPass.Process(ast):
            print("Failed to lower AST to IR")
            return None

        irModule = lowerPass.Visitor.Module

        for i, p in enumerate(self.irPasses):
            if not optimizations and p.Flags & PassFlags.IsOptimization:
                continue

            if not self.__RunPass(irModule, i, p, "IR", debugPasses):
                return None

        if options.get("wasm", False):
            wasmPass = GenerateWasm.GetPass()
            wasmPass.Process(irModule)
            wasmModule = wasmPass.Visitor.Finalize()
        else:
            wasmModule = None

        return Compiler.Result(irModule=irModule, wasmModule=wasmModule)
