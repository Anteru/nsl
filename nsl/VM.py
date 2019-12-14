from typing import Dict, Any
from . import (LinearIR, Errors)
import math
import copy

class ExecutionContext:
    def __init__(self, functions, globalScope: Dict[str, Any]):
        self.__globalScope = globalScope
        self.__functions = functions

    def Invoke(self, functionName, **args):
        function = self.__functions[functionName]

        # Internally, function args are always a flat list for fast access
        # The global invoke method uses key-value pairs though, so we build
        # a flat list and use the provided arguments (or None if nothing was
        # set)        
        functionArgs = [args.get(arg.GetName(), None)
            for arg in function.Type.GetArguments()]

        return self.__Execute(self.__functions[functionName], functionArgs)

    def _Invoke(self, functionName, args):
        '''Similar to Invoke, but we get a list of args and those are matched
        to the arguments of the function already.'''
        function = self.__functions[functionName]

        return self.__Execute(function, args)

    def __MatrixMatrixMultiply(self, resultShape, m0, m1):
        result = [
            [0 for _ in range(resultShape[1])] for _ in range(resultShape[0])
        ]

        for i in range(len(m0)):
            for j in range(len(m1[0])):
                for k in range(len(m0[0])):
                    result[i][j] += m0[i][k] * m1[k][j]

        return result

    def __Execute(self, function: LinearIR.Function, args):
        instructions = list()
        blockOffsets = {}

        for bb in function.BasicBlocks:
            blockOffsets[bb.Reference] = len(instructions)
            instructions.extend(bb.Instructions)

        localScope = dict()

        # Register all constants
        for constant in function.Constants:
            localScope [constant.Reference] = constant.Value

        currentInstruction = 0
        lastInstruction = len(instructions)

        while currentInstruction < lastInstruction:
            instruction = instructions[currentInstruction]
            currentInstruction += 1

            opCode = instruction.OpCode

            if opCode == LinearIR.OpCode.LOAD:
                ref = instruction.Reference
                if instruction.Scope == LinearIR.VariableAccessScope.GLOBAL:
                    localScope[ref] = self.__globalScope[instruction.Variable]
                elif instruction.Scope == LinearIR.VariableAccessScope.FUNCTION_ARGUMENT:
                    localScope[ref] = args[instruction.Variable]
                else:
                    localScope[ref] = localScope[instruction.Variable]
            elif opCode == LinearIR.OpCode.STORE:
                if instruction.Scope == LinearIR.VariableAccessScope.GLOBAL:
                    self.__globalScope[instruction.Variable] = localScope[instruction.Store.Reference]
                elif instruction.Scope == LinearIR.VariableAccessScope.FUNCTION_ARGUMENT:
                    args[instruction.Variable] = localScope[instruction.Store.Reference]
                else:
                    localScope[instruction.Variable] = localScope[instruction.Store.Reference]
            elif opCode == LinearIR.OpCode.LOAD_ARRAY \
              or opCode == LinearIR.OpCode.VECTOR_GET \
              or opCode == LinearIR.OpCode.MATRIX_GET:
                ref = instruction.Reference
                var = localScope[instruction.Array.Reference][
                    localScope[instruction.Index.Reference]
                ]
                localScope[ref] = var
            elif opCode == LinearIR.OpCode.LOAD_MEMBER:
                ref = instruction.Reference
                var = localScope[instruction.Variable.Reference][
                    instruction.Member
                ]
                localScope[ref] = var
            elif opCode == LinearIR.OpCode.STORE_MEMBER:
                ref = instruction.Reference
                localScope[instruction.Variable.Reference][
                    instruction.Member
                ] = localScope[instruction.Store.Reference]
            elif opCode == LinearIR.OpCode.SHUFFLE:
                ref = instruction.Reference
                indices = instruction.Indices
                first = localScope[instruction.First.Reference]
                second = localScope[instruction.Second.Reference]
                if not isinstance(first, list):
                    first = [first]
                if not isinstance(second, list):
                    second = [second]
                combined = first + second
                result = [combined[i] for i in indices]
                localScope[ref] = result
            elif opCode == LinearIR.OpCode.STORE_ARRAY:
                ref = instruction.Reference
                var = localScope[instruction.Store.Reference]
                array = instruction.Array.Reference
                localScope[array][
                    localScope[instruction.Index.Reference]
                ] = var
            elif opCode.value >> 16 == 0x1:
                operation = instruction.OpCode
                op1 = localScope[instruction.Values[0].Reference]
                op2 = localScope[instruction.Values[1].Reference]
                ref = instruction.Reference

                if operation == LinearIR.OpCode.ADD:
                    localScope[ref] = op1 + op2
                elif operation == LinearIR.OpCode.SUB:
                    localScope[ref] = op1 - op2
                elif operation == LinearIR.OpCode.DIV:
                    localScope[ref] = op1 / op2
                elif operation == LinearIR.OpCode.MUL:
                    localScope[ref] = op1 * op2
                elif operation == LinearIR.OpCode.MOD:
                    localScope[ref] = op1 % op2
                elif operation == LinearIR.OpCode.LG_AND:
                    localScope[ref] = 1 if op1 and op2 else 0
                elif operation == LinearIR.OpCode.LG_OR:
                    localScope[ref] = 1 if op1 or op2 else 0
                elif operation == LinearIR.OpCode.CMP_GT:
                    localScope[ref] = 1 if op1 > op2 else 0
                elif operation == LinearIR.OpCode.CMP_GE:
                    localScope[ref] = 1 if op1 >= op2 else 0
                elif operation == LinearIR.OpCode.CMP_LT:
                    localScope[ref] = 1 if op1 < op2 else 0
                elif operation == LinearIR.OpCode.CMP_LE:
                    localScope[ref] = 1 if op1 <= op2 else 0
                elif operation == LinearIR.OpCode.CMP_EQ:
                    localScope[ref] = 1 if op1 == op2 else 0
                elif operation == LinearIR.OpCode.CMP_NE:
                    localScope[ref] = 1 if op1 != op2 else 0
                elif operation == LinearIR.OpCode.VECTOR_ADD:
                    localScope[ref] = [x + y for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_SUB:
                    localScope[ref] = [x - y for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_DIV:
                    localScope[ref] = [x / y for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_MUL:
                    localScope[ref] = [x * y for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_CMP_GT:
                    localScope[ref] = [1 if x > y else 0 for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_CMP_GE:
                    localScope[ref] = [1 if x >= y else 0 for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_CMP_LT:
                    localScope[ref] = [1 if x < y else 0 for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_CMP_LE:
                    localScope[ref] = [1 if x <= y else 0 for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_CMP_EQ:
                    localScope[ref] = [1 if x == y else 0 for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_CMP_NE:
                    localScope[ref] = [1 if x != y else 0 for x, y in zip(op1, op2)]
                elif operation == LinearIR.OpCode.VECTOR_MUL_SCALAR:
                    localScope[ref] = [v * op2 for v in op1]
                elif operation == LinearIR.OpCode.VECTOR_DIV_SCALAR:
                    localScope[ref] = [v / op2 for v in op1]
                elif operation == LinearIR.OpCode.MATRIX_MUL_MATRIX:
                    localScope[ref] = self.__MatrixMatrixMultiply(
                        instruction.Type.Shape, op1, op2)
                else:
                    Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                        f'Unsupported binary operation: {operation}'
                    )
            elif opCode == LinearIR.OpCode.BRANCH:
                if instruction.Predicate:
                    predicate = localScope[instruction.Predicate.Reference]

                    # Predicated branches must have at least a 'true' target
                    assert instruction.TrueBlock.Reference is not None

                    if predicate:
                        currentInstruction = blockOffsets[instruction.TrueBlock.Reference]
                    else:
                        if instruction.FalseBlock:
                            currentInstruction = blockOffsets[instruction.FalseBlock.Reference]
                else:
                    currentInstruction = blockOffsets[instruction.TrueBlock.Reference]
            elif opCode == LinearIR.OpCode.RETURN:
                if instruction.Value:
                    return localScope[instruction.Value.Reference]
                else:
                    return None
            elif opCode == LinearIR.OpCode.CALL:
                args = [
                    localScope[arg.Reference] for arg in
                    instruction.Arguments
                ]
                localScope[instruction.Reference] = self._Invoke(instruction.Function, args)
            elif opCode == LinearIR.OpCode.NEW_VARIABLE:
                varType = instruction.Type
                var = 0
                if varType.Kind == LinearIR.TypeKind.Vector:
                    var = [0] * varType.Size
                elif varType.Kind == LinearIR.TypeKind.Matrix:
                    var = [
                        [0] * varType.ColumnCount
                    ] * varType.RowCount
                else:
                    # structures not handled yet
                    pass
                # The semantics of NEW_VARIABLE are such that it registers a new
                # variable and loads it at the same time
                localScope[instruction.Name] = var
                localScope[instruction.Reference] = var
            elif opCode == LinearIR.OpCode.CAST:
                ref = instruction.Reference
                var = localScope[instruction.Value.Reference]

                assert instruction.Type.IsScalar()

                if isinstance(instruction.Type, LinearIR.IntegerType):
                    if not instruction.Type.Unsigned:
                        var = math.floor(var)
                    else:
                        var = abs(math.floor(var))
                else:
                    # Must be float
                    assert isinstance(instruction.Type, LinearIR.FloatType)

                    var = float(var)
                
                localScope[ref] = var
            elif opCode == LinearIR.OpCode.CONSTRUCT_PRIMITIVE:
                ref = instruction.Reference
                if instruction.Type.Kind == LinearIR.TypeKind.Vector:
                    var = []
                    for value in instruction.Values:
                        value = localScope[value.Reference]
                        if isinstance(value, list):
                            var += value
                        else:
                            var.append(value)
                    localScope[ref] = var
                elif instruction.Type.Kind == LinearIR.TypeKind.Matrix:
                    var = []
                    for value in instruction.Values:
                        value = localScope[value.Reference]
                        assert isinstance(value, list)
                        var.append(value)
                    localScope[ref] = var
                else:
                    Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                        f'Cannot construct primitive of type: {instruction.Type}'
                    )
            elif opCode == LinearIR.OpCode.VECTOR_SET:
                ref = instruction.Reference
                var = localScope[instruction.Store.Reference]
                array = instruction.Array.Reference
                result = copy.deepcopy(localScope[array])
                result [localScope[instruction.Index.Reference]] = var
                localScope[ref] = result
            elif opCode == LinearIR.OpCode.MATRIX_SET:
                ref = instruction.Reference
                var = localScope[instruction.Store.Reference]
                array = instruction.Array.Reference
                result = copy.deepcopy (localScope[array])
                result [localScope[instruction.Index.Reference]] = var
                localScope[ref] = result
            else:
                raise Exception(f"Unhandled opcode: {opCode}")


class Program:
    def __init__(self, functions, globalSymbols):
        self.__functions = functions
        self.__globals = globalSymbols

    @property
    def Functions(self):
        return self.__functions

    @property
    def Globals(self):
        return self.__globals

class Linker:
    def __init__(self):
        self.__modules = []
        self.__functions = {}
        self.__globals =  {}

    def AddModule(self, module: LinearIR.Module):
        self.__modules.append(module)
        for k,v in module.Functions.items():
            assert k not in self.__functions
            self.__functions[k] = v

        for k,v in module.Globals.items():
            assert k not in self.__globals
            self.__globals[k] = v

    def Link(self) -> Program:
        return Program(self.__functions, self.__globals)

class VirtualMachine:
    def __init__(self, program: Program):
        self.__globalScope = {k: None for k in program.Globals.keys()}

        self.__ctx = ExecutionContext(program.Functions, self.__globalScope)

    def SetGlobal(self, globalVariableName, value):
        self.__globalScope [globalVariableName] = value

    def GetGlobal(self, globalVariableName):
        return self.__globalScope[globalVariableName]

    def Invoke(self, functionName, **args):
        return self.__ctx.Invoke(functionName, **args)
        