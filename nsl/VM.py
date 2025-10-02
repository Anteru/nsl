from typing import Dict, Any, List
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
        functionArgs = [args.get(arg, None)
            for arg in function.Type.Arguments]

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

    def __CreateInstance(self, varType: LinearIR.Type):
        if varType.IsPrimitive():
            return self.__CreatePrimitiveInstance(varType)
        elif varType.IsStructure():
            return self.__CreateStructureInstance(varType)
        elif varType.IsArray():
            assert isinstance(varType, LinearIR.ArrayType)
            result = [self.__CreateInstance(varType.ElementType)] * varType.Size[0]
            for dimSize in varType.Size[1:]:
                result = [result] * dimSize
            return result

    def __CreatePrimitiveInstance(self, primitiveType: LinearIR.Type):
        match primitiveType.Kind:
            case LinearIR.TypeKind.Vector:
                return [0] * primitiveType.Size
            case LinearIR.TypeKind.Matrix:
                return [
                        [0] * primitiveType.ColumnCount
                    ] * primitiveType.RowCount
            case LinearIR.TypeKind.Scalar:
                return 0

        raise Exception('Cannot create primitive instance')

    def __CreateStructureInstance(self, structureType: LinearIR.StructureType) -> object:
        r = {}
        for name, fieldType in structureType.Fields.items():
            value = self.__CreateInstance(fieldType)
            r[name] = value
        return r

    def __Execute(self, function: LinearIR.Function, args):
        instructions : List[LinearIR.Instruction] = list()
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

            match opCode:
                case LinearIR.OpCode.LOAD:
                    ref = instruction.Reference
                    match instruction.Scope:
                        case LinearIR.VariableAccessScope.GLOBAL:
                            localScope[ref] = self.__globalScope[instruction.Variable]
                        case LinearIR.VariableAccessScope.FUNCTION_ARGUMENT:
                            localScope[ref] = args[instruction.Variable]
                        case LinearIR.VariableAccessScope.FUNCTION_LOCAL:
                            localScope[ref] = localScope[instruction.Variable]
                case LinearIR.OpCode.STORE:
                    match instruction.Scope:
                        case LinearIR.VariableAccessScope.GLOBAL:
                            self.__globalScope[instruction.Variable] = localScope[instruction.Store.Reference]
                        case LinearIR.VariableAccessScope.FUNCTION_ARGUMENT:
                            args[instruction.Variable] = localScope[instruction.Store.Reference]
                        case LinearIR.VariableAccessScope.FUNCTION_LOCAL:
                            localScope[instruction.Variable] = localScope[instruction.Store.Reference]
                case LinearIR.OpCode.LOAD_ARRAY | LinearIR.OpCode.VECTOR_GET | LinearIR.OpCode.MATRIX_GET:
                    ref = instruction.Reference
                    var = localScope[instruction.Array.Reference][
                        localScope[instruction.Index.Reference]
                    ]
                    localScope[ref] = var
                case LinearIR.OpCode.LOAD_MEMBER:
                    ref = instruction.Reference
                    var = localScope[instruction.Variable.Reference][
                        instruction.Member
                    ]
                    localScope[ref] = var
                case LinearIR.OpCode.STORE_MEMBER:
                    ref = instruction.Reference
                    localScope[instruction.Variable.Reference][
                        instruction.Member
                    ] = localScope[instruction.Store.Reference]
                case LinearIR.OpCode.SHUFFLE:
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
                case LinearIR.OpCode.STORE_ARRAY:
                    ref = instruction.Reference
                    var = localScope[instruction.Store.Reference]
                    array = instruction.Array.Reference
                    localScope[array][
                        localScope[instruction.Index.Reference]
                    ] = var
                case _ if (opCode.value >> 16) == 0x1:
                    operation = instruction.OpCode
                    op1 = localScope[instruction.Values[0].Reference]
                    op2 = localScope[instruction.Values[1].Reference]
                    ref = instruction.Reference

                    match operation:
                        case LinearIR.OpCode.ADD:
                            localScope[ref] = op1 + op2
                        case LinearIR.OpCode.SUB:
                            localScope[ref] = op1 - op2
                        case LinearIR.OpCode.DIV:
                            localScope[ref] = op1 / op2
                        case LinearIR.OpCode.MUL:
                            localScope[ref] = op1 * op2
                        case LinearIR.OpCode.MOD:
                            localScope[ref] = op1 % op2
                        case LinearIR.OpCode.LG_AND:
                            localScope[ref] = 1 if op1 and op2 else 0
                        case LinearIR.OpCode.LG_OR:
                            localScope[ref] = 1 if op1 or op2 else 0
                        case LinearIR.OpCode.CMP_GT:
                            localScope[ref] = 1 if op1 > op2 else 0
                        case LinearIR.OpCode.CMP_GE:
                            localScope[ref] = 1 if op1 >= op2 else 0
                        case LinearIR.OpCode.CMP_LT:
                            localScope[ref] = 1 if op1 < op2 else 0
                        case LinearIR.OpCode.CMP_LE:
                            localScope[ref] = 1 if op1 <= op2 else 0
                        case LinearIR.OpCode.CMP_EQ:
                            localScope[ref] = 1 if op1 == op2 else 0
                        case LinearIR.OpCode.CMP_NE:
                            localScope[ref] = 1 if op1 != op2 else 0
                        case LinearIR.OpCode.VECTOR_ADD:
                            localScope[ref] = [x + y for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_SUB:
                            localScope[ref] = [x - y for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_DIV:
                            localScope[ref] = [x / y for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_MUL:
                            localScope[ref] = [x * y for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_CMP_GT:
                            localScope[ref] = [1 if x > y else 0 for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_CMP_GE:
                            localScope[ref] = [1 if x >= y else 0 for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_CMP_LT:
                            localScope[ref] = [1 if x < y else 0 for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_CMP_LE:
                            localScope[ref] = [1 if x <= y else 0 for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_CMP_EQ:
                            localScope[ref] = [1 if x == y else 0 for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_CMP_NE:
                            localScope[ref] = [1 if x != y else 0 for x, y in zip(op1, op2)]
                        case LinearIR.OpCode.VECTOR_MUL_SCALAR:
                            localScope[ref] = [v * op2 for v in op1]
                        case LinearIR.OpCode.VECTOR_DIV_SCALAR:
                            localScope[ref] = [v / op2 for v in op1]
                        case LinearIR.OpCode.MATRIX_MUL_MATRIX:
                            localScope[ref] = self.__MatrixMatrixMultiply(
                                instruction.Type.Shape, op1, op2)
                        case _:
                            Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                                f'Unsupported binary operation: {operation}'
                            )
                case LinearIR.OpCode.BRANCH:
                    if instruction.Predicate:
                        predicate = localScope[instruction.Predicate.Reference]

                        # Predicated branches must have both targets set
                        assert instruction.TrueBlock.Reference is not None
                        assert instruction.FalseBlock.Reference is not None
                        if predicate:
                            currentInstruction = blockOffsets[instruction.TrueBlock.Reference]
                        else:
                            currentInstruction = blockOffsets[instruction.FalseBlock.Reference]

                    else:
                        currentInstruction = blockOffsets[instruction.TrueBlock.Reference]
                case LinearIR.OpCode.RETURN:
                    if instruction.Value:
                        return localScope[instruction.Value.Reference]
                    else:
                        return None
                case LinearIR.OpCode.CALL:
                    args = [
                        localScope[arg.Reference] for arg in
                        instruction.Arguments
                    ]
                    localScope[instruction.Reference] = self._Invoke(instruction.Function, args)
                case LinearIR.OpCode.NEW_VARIABLE:
                    varType = instruction.Type
                    var = self.__CreateInstance(varType)

                    # The semantics of NEW_VARIABLE are such that it registers a new
                    # variable and loads it at the same time
                    localScope[instruction.Name] = var
                    localScope[instruction.Reference] = var
                case LinearIR.OpCode.CAST:
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
                case LinearIR.OpCode.CONSTRUCT_PRIMITIVE:
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
                case LinearIR.OpCode.VECTOR_SET:
                    ref = instruction.Reference
                    var = localScope[instruction.Store.Reference]
                    array = instruction.Array.Reference
                    result = copy.deepcopy(localScope[array])
                    result [localScope[instruction.Index.Reference]] = var
                    localScope[ref] = result
                case LinearIR.OpCode.MATRIX_SET:
                    ref = instruction.Reference
                    var = localScope[instruction.Store.Reference]
                    array = instruction.Array.Reference
                    result = copy.deepcopy (localScope[array])
                    result [localScope[instruction.Index.Reference]] = var
                    localScope[ref] = result
                case _:
                    raise Exception(f"Unhandled opcode: {opCode}")

class VirtualMachine:
    def __init__(self, program: LinearIR.Program):
        self.__globalScope = {k: None for k in program.Globals.keys()}

        self.__ctx = ExecutionContext(program.Functions, self.__globalScope)

    def SetGlobal(self, globalVariableName, value):
        self.__globalScope [globalVariableName] = value

    def GetGlobal(self, globalVariableName):
        return self.__globalScope[globalVariableName]

    def Invoke(self, functionName, **args) -> Any:
        return self.__ctx.Invoke(functionName, **args)
        