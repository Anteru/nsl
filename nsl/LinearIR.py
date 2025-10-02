from abc import ABC
from typing import Callable, DefaultDict, List, Dict, Iterable, Optional, Tuple, Union
from . import op
from enum import Enum
import collections
from . import Errors
from .Visitor import Node, Visitor
from collections import defaultdict

class OpCode(Enum):
    INVALID = 0
    ASSIGN = 0x0_0001

    BRANCH = 0x0_0101
    RETURN = 0x0_0102
    CALL = 0x0_0201

    CAST = 0x0_1001

    NEW_VARIABLE = 0x0_2001
    CONSTRUCT_PRIMITIVE = 0x02002

    # Binary
    ADD = 0x1_0002
    SUB = 0x1_0003
    MUL = 0x1_0004
    DIV = 0x1_0005
    MOD = 0x1_0006
    
    VECTOR_ADD = 0x1_1002
    VECTOR_SUB = 0x1_1003
    VECTOR_MUL = 0x1_1004
    VECTOR_DIV = 0x1_1005
    VECTOR_MOD = 0x1_1006

    VECTOR_MUL_SCALAR = 0x1_2004
    VECTOR_DIV_SCALAR = 0x1_2005

    MATRIX_MUL_MATRIX = 0x1_3004

    # comparison
    CMP_GT = 0x1_0100
    CMP_LT = 0x1_0101
    CMP_LE = 0x1_0102
    CMP_GE = 0x1_0103
    CMP_NE = 0x1_0104
    CMP_EQ = 0x1_0105
    
    VECTOR_CMP_GT = 0x1_1100
    VECTOR_CMP_LT = 0x1_1101
    VECTOR_CMP_LE = 0x1_1102
    VECTOR_CMP_GE = 0x1_1103
    VECTOR_CMP_NE = 0x1_1104
    VECTOR_CMP_EQ = 0x1_1105

    # logic
    LG_OR   = 0x1_0300
    LG_AND  = 0x1_0301
    LG_NOT  = 0x1_0302

    BIT_OR  = 0x1_0400
    BIT_AND = 0x1_0401
    BIT_NOT = 0x1_0402
    BIT_XOR = 0x1_0403

    # Unary
    UA_ADD = 0x2_0120
    UA_SUB = 0x2_0121

    LOAD = 0x6_0001
    STORE = 0x6_1001
    
    LOAD_ARRAY = 0x6_0002
    STORE_ARRAY = 0x6_1002

    LOAD_MEMBER = 0x6_0003
    STORE_MEMBER = 0x6_1003

    VECTOR_GET = 0x6_0004
    VECTOR_SET = 0x6_1004

    MATRIX_GET = 0x6_0005
    MATRIX_SET = 0x6_1005

    SHUFFLE = 0x6_0010

class TypeKind(Enum):
    Scalar = 1
    Vector = 2
    Matrix = 3
    Array = 4
    Structure = 5
    Void = 6
    Function = 7
    Unknown = -1

class Type:
    @property
    def Kind(self) -> TypeKind:
        return TypeKind.Unknown

    def IsMatrix(self):
        return self.Kind == TypeKind.Matrix

    def IsVector(self):
        return self.Kind == TypeKind.Vector

    def IsScalar(self):
        return self.Kind == TypeKind.Scalar

    def IsArray(self):
        return self.Kind == TypeKind.Array

    def IsStructure(self):
        return self.Kind == TypeKind.Structure

    def IsVoid(self):
        return self.Kind == TypeKind.Void

    def IsPrimitive(self):
        return self.Kind in {TypeKind.Scalar, TypeKind.Vector, TypeKind.Matrix}

class ScalarType(Type):
    @property
    def Kind(self):
        return TypeKind.Scalar

class IntegerType(ScalarType):
    def __init__(self, *, unsigned: bool = False):
        self.__unsigned = unsigned

    @property
    def Unsigned(self) -> bool:
        return self.__unsigned

    def __str__(self):
        if self.Unsigned:
            return 'uint'
        
        return 'int'

class FloatType(ScalarType):
    def __str__(self):
        return 'float'

class VoidType(Type):
    @property
    def Kind(self):
        return TypeKind.Void

    def __str__(self):
        return 'void'

class FunctionType(Type):
    def __init__(self, returnType: Type, arguments: Dict[str, Type]):
        self.__arguments = arguments
        self.__returnType = returnType

    @property
    def Kind(self):
        return TypeKind.Function

    @property
    def Arguments(self):
        return self.__arguments

    @property
    def ReturnType(self):
        return self.__returnType

class VectorType(Type):
    def __init__(self, elementType: ScalarType, size: int):
        self.__elementType = elementType
        self.__size = size

    @property
    def Kind(self):
        return TypeKind.Vector

    @property
    def Size(self):
        return self.__size

    @property
    def ElementType(self):
        return self.__elementType

    def __str__(self):
        return f'<{self.Size} × {self.ElementType}>'

class MatrixType(Type):
    def __init__(self, rowType: VectorType, rowCount: int):
        self.__rowType = rowType
        self.__rowCount = rowCount
        self.__columnCount = rowType.Size
    
    @property
    def Kind(self):
        return TypeKind.Matrix

    @property
    def RowCount(self):
        return self.__rowCount

    @property
    def ColumnCount(self):
        return self.__columnCount

    @property
    def ElementType(self):
        return self.__rowType.ElementType

    @property
    def RowType(self):
        return self.__rowType

    @property
    def Shape(self):
        return (self.__columnCount, self.__rowCount)

    def __str__(self):
        return f'<{self.RowCount} × {self.RowType}>'

class StructureType(Type):
    def __init__(self, fields: Dict[str, Type], *, name: str =''):
        self.__fields = fields
        self.__name = name

    @property
    def Fields(self):
        return self.__fields

    @property
    def Name(self):
        return self.__name
    
    @property
    def Kind(self):
        return TypeKind.Structure

    def __str__(self):
        return '{' + ', '.join([f'{v} {k}' for k,v in self.Fields.items()]) + '}'

class ArrayType(Type):
    def __init__(self, elementType: Type, size: List[int]):
        self.__elementType = elementType
        self.__size = size

    @property
    def Kind(self):
        return TypeKind.Array

    @property
    def Size(self):
        return self.__size

    @property
    def ElementType(self):
        return self.__elementType

    def __str__(self):
        return f'{self.ElementType}' + ''.join([f'[{s}]' for s in self.__size])

class Value(Node):
    # A value consists of a type and a reference, which uniquely identifies the
    # value within its function
    def __init__(self, valueType: Type):
        self.__type = valueType
        self.__reference = -1

    @property
    def Type(self):
        return self.__type

    @property
    def Reference(self):
        return self.__reference

    def SetReference(self, number: int):
        assert number >= 0
        self.__reference = number

class ConstantValue(Value):
    def __init__(self, valueType: Type, constantValue: Union[float, int, bool]):
        super().__init__(valueType)
        self.__value = constantValue

    @property
    def Value(self):
        return self.__value

    def __str__(self):
        return f'{self.Type}({self.Value})'

class ValueUser(Value):
    def __init__(self, valueType: Type):
        super().__init__(valueType)

    @property
    def Uses(self) -> Iterable[int]:
        # Return a list of all references this value user is referencing
        return []

    def ReplaceUses(self, ref: int, newValue: Value):
        # Replace all uses referencing ``ref`` with the new value
        raise NotImplementedError

class Instruction(ValueUser, ABC):
    def __init__(self, opCode: OpCode, returnType: Type):
        super().__init__(returnType)
        self.__parent = None
        self.__opcode = opCode

    def SetParent(self, basicBlock: 'BasicBlock'):
        self.__parent = basicBlock

    @property
    def Parent(self) -> Optional['BasicBlock']:
        return self.__parent

    @property
    def OpCode(self) -> OpCode:
        return self.__opcode

    def _SetOpCode(self, opcode: OpCode):
        self.__opcode = opcode

    def _ReplaceUsesInList(self, valueList: List[Value], ref: int, newValue: Value):
        for i in range(len(valueList)):
            if valueList[i].Reference == ref:
                valueList[i] = newValue

class BasicBlock(Value):
    def __init__(self, function: "Function"):
        super().__init__(VoidType())
        self.__instructions: List[Instruction] = []
        self.__function = function
        self.__replaceUses = {}
        self.__replacements = {}
        # Link between reference and instruction using it. This makes replacing
        # very fast, as we can find all instructions that reference a given
        # instruction directly
        self.__uses: DefaultDict[int, List[Value]] = defaultdict(list)

    def UpdateUses(self):
        self.__uses = defaultdict(list)

        for i in self.__instructions:
            # We call set here to remove duplicates. It should not make any
            # functional difference though to just iterate the uses, but there
            # may be duplicates
            for use in set(i.Uses):
                self.__uses[use].append(i)

    # Dictionary containing a reference as the key, and a list of instructions
    # referencing it as the value. Only valid after UpdateUses()
    @property
    def Uses(self) -> Dict[int, List[Value]]:
        return self.__uses

    @property
    def Parent(self):
        return self.__function

    @property
    def Instructions(self) -> List[Instruction]:
        return self.__instructions

    def GetPreviousInstruction(self, i) -> Optional[Instruction]:
        for index, instruction in enumerate(self.__instructions):
            if instruction == i:
                if index > 0:
                    return self.__instructions[index - 1]
                else:
                    break
        return None

    def _Traverse(self, function: Callable[[List[Instruction]], List[Instruction]]):
        self.__replacements = {}
        self.__replaceUses = {}
        self.__instructions = function(self.__instructions)

        # During traversal, we can register replacements which cannot be
        # executed immediately, we record them and then apply them here

        if self.__replaceUses:
            # We replace uses first: Otherwise, we could remove an instruction
            # and all references to it, and there is no way to fix this
            # once the reference to the instruction has been removed
            # For instance, if we have ``ret %7``, and we remove the instruction
            # producing %7, then we'll have ``ret None``, and if we want to
            # replace that with a new reference, we can't as the reference to
            # %7 is missing.
            self.Parent.ReplaceUses(self.__replaceUses)

        if self.__replacements:
            self.__Replace()

            # Our uses may have changed, so we need to update them
            self.UpdateUses()

            # We need to update all uses of a replaced instruction as well, as we
            # reference instructions directly and otherwise the references would
            # remain pointing to old instructions
            self.Parent.ReplaceUses(self.__replacements)

        self.__replacements = {}
        self.__replaceUses = {}

    def AddInstruction(self, instruction: Instruction):
        instruction.SetParent(self)
        self.__function.RegisterValue(instruction)
        self.__instructions.append(instruction)
        return instruction

    def AddInstructionBefore(self, instruction: Instruction,
                            insertionPoint: Instruction):
        for i, e in enumerate(self.__instructions):
            if e == insertionPoint:
                self.__instructions.insert(i, instruction)
                break

        instruction.SetParent(self)
        self.__function.RegisterValue(instruction)
        return instruction

    def AddInstructionAfter(self, instruction: Instruction,
                            insertionPoint: Instruction):
        for i, e in enumerate(self.__instructions):
            if e == insertionPoint:
                self.__instructions.insert(i + 1, instruction)
                break

        instruction.SetParent(self)
        self.__function.RegisterValue(instruction)
        return instruction

    def Replace(self, old: Union[int, Value], new: Optional[Value]):
        if isinstance(old, Value):
            self.__replacements[old.Reference] = new
        else:
            self.__replacements[old] = new

    def ReplaceUses(self, old: Union[int, Value], new: Optional[Value]):
        if isinstance(old, Value):
            self.__replaceUses[old.Reference] = new
        else:
            self.__replaceUses[old] = new

    def __Replace(self):
        # We search all instructions, check if they're marked for replacement,
        # and if so, plug in the new instruction right there with the same
        # reference. Afterwards, we purge empty slots
        # Other uses of the old instruction need to be updated using
        # ReplaceUses()
        newInstructions = []
        for instruction in self.__instructions:
            if instruction.Reference in self.__replacements:
                newInstruction = self.__replacements[instruction.Reference]
                if isinstance(newInstruction, Instruction):
                    newInstruction.SetReference(instruction.Reference)
                    newInstructions.append(newInstruction)
            else:
                newInstructions.append(instruction)

        self.__instructions = newInstructions

class Function(Value):
    def __init__(self, name: str, functionType: FunctionType):
        super().__init__(functionType)
        self.__basicBlocks: List[BasicBlock] = []
        self.__name = name
        self.__values = []
        self.__constants = {}
        self.__uses = defaultdict(list)

    def CreateBasicBlock(self):
        bb = BasicBlock (self)
        self.RegisterValue(bb)
        self.__basicBlocks.append (bb)
        return bb

    def UpdateUses(self):
        # Update the uses of all basic blocks and the function itself
        self.__uses = defaultdict(list)

        for bb in self.BasicBlocks:
            bb.UpdateUses()
            self.__uses.update (bb.Uses)
    
    @property
    def BasicBlocks(self):
        return self.__basicBlocks

    @property
    def Instructions(self):
        result = []
        for bb in self.BasicBlocks:
            result.extend(bb.Instructions)
        return result

    def _Traverse(self, function: Callable[[List[BasicBlock]], List[BasicBlock]]):
        self.__basicBlocks = function(self.__basicBlocks)

    def RegisterValue(self, value: Value):
        n = len(self.__values)
        value.SetReference(n)
        self.__values.append(value)
        return n

    def CreateConstant(self, constantType: Type, value: Union[int, float, bool]):
        result = self.__constants.get(value, None)
        if result:
            return result
        
        cv = ConstantValue(constantType, value)
        self.RegisterValue (cv)
        self.__constants[value] = cv

        return cv
    
    @property
    def Constants(self):
        return self.__constants.values()

    @property
    def Name(self):
        return self.__name

    def ReplaceUses(self, uses):
        for ref, new in uses.items():
            for instruction in self.__uses[ref]:
                instruction.ReplaceUses(ref, new)

        # We have replaced uses across several basic blocks, we need to let them
        # know that their uses are no longer valid
        self.UpdateUses()

class Module(Node):
    def __init__(self):
        self.__functions: Dict[str, Function] = collections.OrderedDict()
        self.__globals = collections.OrderedDict()
        self.__imports = set()
        self.__metadata = dict()

    @property
    def Metadata(self):
        return self.__metadata

    @property
    def Functions(self) -> Dict[str, Function]:
        return self.__functions

    @property
    def Globals(self):
        return self.__globals

    @property
    def Imports(self):
        return self.__imports

    def CreateFunction(self, name: str, functionType: FunctionType):
        f = Function(name, functionType)
        self.__functions[name] = f
        return f

    def CreateGlobalVariable(self, name: str, variableType: Type):
        self.__globals[name]= variableType

    def AddImport(self, name: str):
        self.__imports.add(name)

    def _Traverse(self, function: Callable[[Dict[str, Function]], Dict[str, Function]]):
        self.__functions = function(self.__functions)

class BinaryInstruction(Instruction):
    def __init__(self, operation: OpCode, returnType: Type,
        v1: Value, v2: Value):
        super().__init__(operation, returnType)
        self.__values = [v1, v2]

    def ReplaceUses(self, ref, newValue):
        self._ReplaceUsesInList(self.__values, ref, newValue)

    @property
    def Uses(self):
        return [v.Reference for v in self.__values]

    @staticmethod
    def FromOperation(operation: op.Operation, returnType: Type,
        v1: Value, v2: Value):
        assert returnType.IsPrimitive()
        assert v1.Type.IsPrimitive()
        assert v2.Type.IsPrimitive()

        if returnType.IsScalar():
            mapping = {
                op.Operation.ASSIGN: OpCode.ASSIGN,

                op.Operation.ADD: OpCode.ADD,
                op.Operation.MUL: OpCode.MUL,
                op.Operation.SUB: OpCode.SUB,
                op.Operation.DIV: OpCode.DIV,
                op.Operation.MOD: OpCode.MOD,

                op.Operation.LG_AND: OpCode.LG_AND,
                op.Operation.LG_OR: OpCode.LG_OR,
                
                op.Operation.CMP_GT: OpCode.CMP_GT,
                op.Operation.CMP_GE: OpCode.CMP_GE,
                op.Operation.CMP_LT: OpCode.CMP_LT,
                op.Operation.CMP_LE: OpCode.CMP_LE,
                op.Operation.CMP_EQ: OpCode.CMP_EQ,
                op.Operation.CMP_NE: OpCode.CMP_NE,
            }
        elif returnType.IsVector():
            mapping = {
                op.Operation.ASSIGN: OpCode.ASSIGN,

                op.Operation.ADD: OpCode.VECTOR_ADD,
                op.Operation.MUL: OpCode.VECTOR_MUL,
                op.Operation.SUB: OpCode.VECTOR_SUB,
                op.Operation.DIV: OpCode.VECTOR_DIV,
                
                op.Operation.CMP_GT: OpCode.VECTOR_CMP_GT,
                op.Operation.CMP_GE: OpCode.VECTOR_CMP_GE,
                op.Operation.CMP_LT: OpCode.VECTOR_CMP_LT,
                op.Operation.CMP_LE: OpCode.VECTOR_CMP_LE,
                op.Operation.CMP_EQ: OpCode.VECTOR_CMP_EQ,
                op.Operation.CMP_NE: OpCode.VECTOR_CMP_NE,
            }
        else:
            Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                f"Cannot lower binary operation {operation} with types: {v1.Type}, {v2.Type}")

        if operation == op.Operation.MUL and v1.Type.IsVector () and v2.Type.IsScalar():
            return BinaryInstruction(OpCode.VECTOR_MUL_SCALAR, returnType,
                v1, v2)
        elif operation == op.Operation.DIV and v1.Type.IsVector () and v2.Type.IsScalar ():
            return BinaryInstruction(OpCode.VECTOR_DIV_SCALAR, returnType,
                v1, v2)        

        return BinaryInstruction(mapping[operation], returnType,
            v1, v2)

    @property
    def Values(self):
        return self.__values

class CompareInstruction(Instruction):
    def __init__(self, predicate: OpCode, returnType: Type,
        v1: Value, v2: Value):
        super().__init__(predicate, returnType)
        self.__values = [v1, v2]

    def ReplaceUses(self, ref, newValue):
        self._ReplaceUsesInList(self.__values, ref, newValue)

    @property
    def Uses(self):
        return [v.Reference for v in self.__values]

class BranchInstruction(Instruction):
    def __init__(self, trueBlock: Optional[BasicBlock],
        falseBlock: Optional[BasicBlock] = None,
        predicate: Optional[Value] = None):
        super().__init__(OpCode.BRANCH, VoidType())
        self.__trueBlock: Optional[BasicBlock] = trueBlock
        self.__falseBlock: Optional[BasicBlock] = falseBlock
        self.__predicate = predicate

    def ReplaceUses(self, ref, newValue):
        assert self.__trueBlock is not None

        if self.__trueBlock.Reference == ref:
            self.__trueBlock = newValue

        if self.__falseBlock and self.__falseBlock.Reference == ref:
            self.__falseBlock = ref

        if self.__predicate and self.__predicate.Reference == ref:
            self.__predicate = ref

    @property
    def Uses(self):
        yield self.__trueBlock

        if self.__falseBlock:
            yield self.__falseBlock

        if self.__predicate:
            yield self.__predicate

    def SetTrueBlock(self, trueBlock: BasicBlock):
        self.__trueBlock = trueBlock

    @property
    def TrueBlock(self) -> Optional[BasicBlock]:
        return self.__trueBlock

    def SetFalseBlock(self, falseBlock: BasicBlock):
        self.__falseBlock = falseBlock

    @property
    def FalseBlock(self):
        return self.__falseBlock

    @property
    def Predicate(self):
        return self.__predicate

class UnaryInstruction(Instruction):
    def __init__(self, operation: OpCode, returnType: Type, value: Value):
        super().__init__(operation, returnType)
        self.__value = value

    @property
    def Value(self):
        return self.__value

    def ReplaceUses(self, ref, newValue: Value):
        if self.__value.Reference == ref:
            self.__value = newValue

    @property
    def Uses(self):
        yield self.__value.Reference

class CastInstruction(UnaryInstruction):
    def __init__(self, value: Value, targetType: Type):
        super().__init__(OpCode.CAST, targetType, value)

class ReturnInstruction(Instruction):
    def __init__(self, value: Optional[Value] = None):
        if value:
            super().__init__(OpCode.RETURN, value.Type)
        else:
            super().__init__(OpCode.RETURN, VoidType())
        self.__value = value

    @property
    def Value(self):
        return self.__value

    def ReplaceUses(self, ref: int, newValue: Value):
        if self.__value and self.__value.Reference == ref:
            self.__value = newValue

    @property
    def Uses(self):
        if self.__value:
            yield self.__value.Reference
        else:
            return []

class VariableAccessScope(Enum):
    GLOBAL = 0
    FUNCTION_ARGUMENT = 1
    FUNCTION_LOCAL = 2

class ConstructPrimitiveInstruction(Instruction):
    def __init__(self, returnType: Type,
        items: List[Value]):
        super().__init__(OpCode.CONSTRUCT_PRIMITIVE, returnType)
        self.__values = items

    @property
    def Values(self):
        return self.__values

    def ReplaceUses(self, ref, newValue):
        self._ReplaceUsesInList(self.__values, ref, newValue)

    @property
    def Uses(self):
        return [v.Reference for v in self.__values]

class MemberAccessInstruction(Instruction):
    def __init__(self, memberType: Type, variable: Value, member: str,
        accessScope: VariableAccessScope = VariableAccessScope.FUNCTION_LOCAL):
        super().__init__(OpCode.LOAD_MEMBER, memberType)
        self.__variable = variable
        self.__member = member
        self.__store = None
        self.__scope = accessScope

    @property
    def Variable(self):
        return self.__variable

    @property
    def Member(self):
        return self.__member

    @property
    def Scope(self):
        return self.__scope

    def SetStore(self, destination: Value):
        self._SetOpCode (OpCode.STORE_MEMBER)
        self.__store = destination

    @property
    def Store(self):
        return self.__store

    def ReplaceUses(self, ref, newValue):
        if self.__parent.Reference == ref:
            self.__parent = newValue

        if self.__store and self.__store.Reference == newValue:
            self.__store = newValue

    @property
    def Uses(self):
        yield self.__variable.Reference
        if self.__store:
            yield self.__store.Reference

class ShuffleInstruction(Instruction):
    def __init__(self, returnType: VectorType,
        first: Value,
        second: Value,
        indices: List[int]):
        super().__init__(OpCode.SHUFFLE, returnType)
        self.__first = first
        self.__second = second
        self.__indices = indices

    @property
    def First(self):
        return self.__first

    @property
    def Second(self):
        return self.__second

    @property
    def Indices(self):
        return self.__indices

    def ReplaceUses(self, ref, newValue):
        if self.__first.Reference == ref:
            self.__first = newValue
        
        if self.__second.Reference == ref:
            self.__second = newValue

    @property
    def Uses(self):
        yield self.__first.Reference
        yield self.__second.Reference

class VariableAccessInstruction(Instruction):
    def __init__(self, returnType: Type, variableName: Union[str, int],
            accessScope: VariableAccessScope = VariableAccessScope.GLOBAL):
        super().__init__(OpCode.LOAD, returnType)
        self.__variable = variableName
        self.__store = None
        self.__scope = accessScope

    def WithVariable(self, newVariableName: Union[str, int]):
        """Create a copy of this instruction with a different variable name."""
        result = VariableAccessInstruction(self.Type, newVariableName,
            self.__scope)
        if self.Store:
            result.SetStore(self.Store)
        result.SetParent(self.Parent)
        result.SetReference(self.Reference)
        return result

    @property
    def Variable(self):
        return self.__variable

    def SetStore(self, destination: Value):
        self.__store = destination
        self._SetOpCode(OpCode.STORE)

    @property
    def Store(self):
        return self.__store

    @property
    def Scope(self):
        return self.__scope

    def ReplaceUses(self, ref, newValue):
        if self.__store and self.__store.Reference == ref:
            self.__store = newValue

    @property
    def Uses(self):
        if self.__store:
            yield self.__store.Reference
        else:
            return []

class CallInstruction(Instruction):
    def __init__(self, returnType: Type, functionName: str,
        arguments: List[Value] = []):
        super().__init__(OpCode.CALL, returnType)
        self.__function = functionName
        self.__arguments = arguments

    @property
    def Arguments(self):
        return self.__arguments

    @property
    def Function(self):
        return self.__function

    def ReplaceUses(self, ref, newValue):
        self._ReplaceUsesInList(self.__arguments, ref, newValue)

    @property
    def Uses(self):
        return [a.Reference for a in self.__arguments]

class _IndexedAccessBase(Instruction):
    def __init__(self, returnType: Type, array: Value, index: Value,
        opCodes: Tuple[OpCode, OpCode]):
        super().__init__(opCodes[0], returnType)

        assert index.Type.IsScalar()

        self.__opCodes = opCodes
        self.__array = array
        self.__index = index
        self.__store = None

    @property
    def Array(self):
        return self.__array

    @property
    def Index(self):
        return self.__index

    def SetStore(self, value: Value):
        assert value is not None

        self.__store = value
        self._SetOpCode(self.__opCodes[1])

    @property
    def Store(self):
        return self.__store

    def ReplaceUses(self, ref, newValue):
        if self.__array.Reference == ref:
            self.__array = newValue

        if self.__index.Reference == ref:
            self.__index = newValue

        if self.__store and self.__store.Reference == ref:
            self.__store = newValue

    @property
    def Uses(self):
        yield self.__array.Reference
        yield self.__index.Reference

        if self.__store:
            yield self.__store.Reference

class ArrayAccessInstruction(_IndexedAccessBase):
    def __init__(self, returnType: Type, array: Value, index: Value):
        super().__init__(returnType, array, index,
            (OpCode.LOAD_ARRAY, OpCode.STORE_ARRAY))
        assert array.Type.IsArray()

class VectorAccessInstruction(_IndexedAccessBase):
    def __init__(self, returnType: Type, vector: Value, index: Value):
        super().__init__(returnType, vector, index,
            (OpCode.VECTOR_GET, OpCode.VECTOR_SET))
        assert vector.Type.IsVector()

class MatrixAccessInstruction(_IndexedAccessBase):
    def __init__(self, returnType: Type, matrix: Value, index: Value):
        super().__init__(returnType, matrix, index,
            (OpCode.MATRIX_GET, OpCode.MATRIX_SET))
        assert matrix.Type.IsMatrix()

class DeclareVariableInstruction(Instruction):
    def __init__(self, variableType, name = None,
        scope = VariableAccessScope.GLOBAL):
        super().__init__(OpCode.NEW_VARIABLE, variableType)
        self.__name = name
        self.__scope = scope

    def ReplaceUses(self, ref: int, newValue: Value):
        pass

    @property
    def Name(self):
        return self.__name

    @property
    def Scope(self):
        return self.__scope

    def SetReference(self, reference: int):
        super().SetReference(reference)
        if self.__name is None:
            self.__name = f'${self.Reference}'

class InstructionPrinter(Visitor):
    def __init__(self, printFunction=print):
        self.__printFunction = printFunction

    def __Print(self, *args, end='\n'):
        self.__printFunction(*args, end=end)

    def __FormatReference(self, v: Value):
        if isinstance(v, ConstantValue):
            return str(v.Value)
        else:
            return f'%{v.Reference}'

    def __FormatType(self, t: Type):
        return str(t)

    def __FormatLabel(self, label: Value):
        return f'label bb_{label.Reference}'

    def Print(self, i):
        self.v_Visit(i)

    def v_Function(self, function: Function, ctx=None):
        self.__Print('function', function.Name,
            '(' + ', '.join(map(str, function.Type.Arguments)) + ')')
        for basicBlock in function.BasicBlocks:
            self.v_Visit(basicBlock, 1)
        self.__Print()

    def v_BasicBlock(self, bb: BasicBlock, ctx=None):
        self.__Print(f'bb_{bb.Reference}: ')
        for i in bb.Instructions:
            self.__Print(' ' * (ctx * 4), end='')
            self.v_Visit(i, ctx + 1)
    
    def v_ReturnInstruction(self, ri: ReturnInstruction, ctx=None):
        if ri.Value:
            self.__Print('ret', self.__FormatReference(ri.Value))
        else:
            self.__Print('ret')

    def v_CastInstruction(self, ci: CastInstruction, ctx=None):
        self.__Print(self.__FormatReference(ci), '=',
            f'cast {self.__FormatType(ci.Type)}',
            self.__FormatReference (ci.Value))

    def v_BinaryInstruction(self, bi: BinaryInstruction, ctx=None):
        self.__Print(self.__FormatReference(bi), '=',
            f'{bi.OpCode.name.lower()} {self.__FormatType(bi.Type)}',
            self.__FormatReference(bi.Values[0]),
            self.__FormatReference(bi.Values[1]))

    def v_CallInstruction(self, ci: CallInstruction, ctx=None):
        self.__Print(self.__FormatReference(ci), '=',
            'call',
            self.__FormatType(ci.Type),
            ci.Function,
            ", ".join([self.__FormatReference(arg) for arg in ci.Arguments]))

    def __FormatScope(self, vas: VariableAccessScope):
        if vas == VariableAccessScope.GLOBAL:
            return 'global'
        elif vas == VariableAccessScope.FUNCTION_ARGUMENT:
            return 'arg'
        else:
            return 'local'

    def v_VariableAccessInstruction(self, li: VariableAccessInstruction, ctx=None):
        scope = self.__FormatScope(li.Scope)

        if li.Store:
            self.__Print(f'store.{scope}',
                self.__FormatType(li.Type),
                li.Variable,
                self.__FormatReference(li.Store))
        else:
            self.__Print(self.__FormatReference(li), '=',
                f'load.{scope}',
                self.__FormatType(li.Type),
                li.Variable)

    def v_ArrayAccessInstruction(self, aai: ArrayAccessInstruction, ctx=None):
        if aai.Store:
            self.__Print(f'store',
                self.__FormatReference(aai.Array),
                self.__FormatReference(aai.Index),
                self.__FormatReference(aai.Store))
        else:
            self.__Print(self.__FormatReference(aai), '=',
                f'load',
                self.__FormatType(aai.Type),
                self.__FormatReference(aai.Array),
                self.__FormatReference(aai.Index))

    def __printAccessInstruction(self, read: str, write: str,
        instruction: _IndexedAccessBase, ctx=None):
        if instruction.Store:
            self.__Print(
                self.__FormatReference(instruction), '=',
                write,
                self.__FormatReference(instruction.Array),
                self.__FormatReference(instruction.Index),
                self.__FormatReference(instruction.Store))
        else:
            self.__Print(self.__FormatReference(instruction), '=',
                read,
                self.__FormatType(instruction.Type),
                self.__FormatReference(instruction.Array),
                self.__FormatReference(instruction.Index))


    def v_VectorAccessInstruction(self, vai: VectorAccessInstruction, ctx=None):
        self.__printAccessInstruction('vectorget', 'vectorset',
            vai, ctx)

    def v_MatrixAccessInstruction(self, mai: MatrixAccessInstruction, ctx=None):
        self.__printAccessInstruction('matrixget', 'matrixset',
            mai, ctx)

    def v_MemberAccessInstruction(self, mai: MemberAccessInstruction, ctx=None):
        if mai.Store:
            self.__Print(
                f'fieldset',
                self.__FormatReference(mai.Parent),
                mai.Member,
                self.__FormatReference(mai.Store))
        else:
            self.__Print(self.__FormatReference(mai), '=',
                f'fieldget',
                self.__FormatType(mai.Type),
                self.__FormatReference(mai.Parent),
                mai.Member)

    def v_BranchInstruction(self, bi: BranchInstruction, ctx=None):
        if bi.Predicate is None:
            self.__Print('branch', self.__FormatLabel(bi.TrueBlock))
        else:
            self.__Print('branch', self.__FormatReference(bi.Predicate), end='')
            self.__Print(',', self.__FormatLabel (bi.TrueBlock), end='')
            if bi.FalseBlock:
                self.__Print(',', self.__FormatLabel (bi.FalseBlock), end='')
            self.__Print()

    def v_DeclareVariableInstruction(self, dvi: DeclareVariableInstruction, ctx=None):
        scope = self.__FormatScope(dvi.Scope)
        self.__Print(self.__FormatReference(dvi), '=',
            f'var.{scope}',
            self.__FormatType(dvi.Type),
            dvi.Name)

    def v_ConstructPrimitiveInstruction(self, cpi: ConstructPrimitiveInstruction, ctx=None):
        self.__Print(self.__FormatReference(cpi), '=',
            'constructprimitive',
            self.__FormatType(cpi.Type),
            ', '.join([self.__FormatReference(v) for v in cpi.Values]))

    def v_ShuffleInstruction(self, si: ShuffleInstruction, ctx=None):
        self.__Print(self.__FormatReference(si), '=',
            'shuffle',
            self.__FormatReference(si.First),
            self.__FormatReference(si.Second),
            ', '.join(map(str, si.Indices)))


class ModuleLoader:
    def Load(self, moduleName: str) -> Module:
        raise NotImplementedError

class FilesystemModuleLoader(ModuleLoader):
    def Load(self, moduleName: str) -> Module:
        import pickle
        import pathlib
        # Try to open the full path first, and if missing, try with '.nslir'
        path = pathlib.Path(moduleName)
        if path.exists():
            module = pickle.load(path.open('rb'))
        else:
            path = path.with_suffix('.nslir')
            if path.exists():
                module = pickle.load(path.open('rb'))
            else:
                raise RuntimeError(f"Could not find module '{moduleName}'")
        assert isinstance(module, Module)
        return module

class MemoryModuleLoader(ModuleLoader):
    def __init__(self):
        self.__modules: Dict[str, Module] = {}

    def AddModule(self, name: str, module: Module):
        self.__modules[name] = module

    def Load(self, moduleName: str) -> Module:
        return self.__modules[moduleName]

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
    def __init__(self, *, loader = FilesystemModuleLoader()):
        self.__modules = []
        self.__functions = {}
        self.__globals =  {}
        self.__loader = loader
        self.__pendingImports = set()

    def AddModule(self, module: Module):
        self.__modules.append(module)
        for k,v in module.Functions.items():
            assert k not in self.__functions
            self.__functions[k] = v

        for k,v in module.Globals.items():
            assert k not in self.__globals
            self.__globals[k] = v

        self.__pendingImports.update(module.Imports)

    def Link(self) -> Program:
        # add all imported modules
        for importedModule in self.__pendingImports:
            self.AddModule(self.__loader.Load(importedModule))

        return Program(self.__functions, self.__globals)
