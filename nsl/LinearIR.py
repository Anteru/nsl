from typing import List, Dict, Iterable, Optional
from . import op
from enum import Enum
import collections
from . import Errors, types
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

    # Unary
    UA_ADD = 0x2_0120
    UA_SUB = 0x2_0121

    # logic
    LG_OR   = 0x4_0300
    LG_AND  = 0x4_0301
    LG_NOT  = 0x4_0302

    BIT_OR  = 0x5_0400
    BIT_AND = 0x5_0401
    BIT_NOT = 0x5_0402
    BIT_XOR = 0x5_0403

    LOAD = 0x6_0001
    STORE = 0x6_1001
    
    LOAD_ARRAY = 0x6_0002
    STORE_ARRAY = 0x6_1002

    LOAD_MEMBER = 0x6_0003
    STORE_MEMBER = 0x6_1003

    VECTOR_GET = 0x6_0004
    VECTOR_SET = 0x6_1004

    SHUFFLE = 0x6_0010
    
class Value(Node):
    # A value consists of a type and a reference, which unique identifies the
    # value within its function
    def __init__(self, valueType: types.Type):
        self.__type = valueType
        self.__reference = -1

    @property
    def Type(self):
        return self.__type

    @property
    def Reference(self):
        return self.__reference

    def SetReference(self, number):
        self.__reference = number

class ConstantValue(Value):
    def __init__(self, valueType: types.Type, constantValue):
        super().__init__(valueType)
        self.__value = constantValue

    @property
    def Value(self):
        return self.__value

    def __str__(self):
        return f'{self.Type}({self.Value})'

class ValueUser(Value):
    def __init__(self, valueType: types.Type):
        super().__init__(valueType)

    @property
    def Uses(self) -> Iterable[int]:
        # Return a list of all references this value user is referencing
        return []

    def ReplaceUses(self, ref: int, newValue: Value):
        # Replace all uses referencing ``ref`` with the new value
        pass

class Instruction(ValueUser):
    def __init__(self, opCode: OpCode, returnType: types.Type):
        super().__init__(returnType)
        self.__parent = None
        self.__opcode = opCode

    def SetParent(self, basicBlock):
        self.__parent = basicBlock

    @property
    def Parent(self):
        return self.__parent

    @property
    def OpCode(self):
        return self.__opcode

    def _SetOpCode(self, opcode):
        self.__opcode = opcode

    def _ReplaceUsesInList(self, valueList: List[Value], ref: int, newValue: Value):
        for i in range(len(valueList)):
            if valueList[i].Reference == ref:
                valueList[i] = newValue

class BasicBlock(Value):
    def __init__(self, function):
        super().__init__(types.Void())
        self.__instructions = []
        self.__predecessors = []
        self.__successors = []
        self.__function = function
        self.__replacements = {}
        # Link between reference and instruction using it. This makes replacing
        # very fast, as we can find all instructions that reference a given
        # instruction directly
        self.__uses = defaultdict(list)

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
    def Uses(self) -> Dict[int, Value]:
        return self.__uses

    @property
    def Parent(self):
        return self.__function

    @property
    def Instructions(self):
        return self.__instructions

    def _Traverse(self, function):
        self.__replacements = {}
        self.__instructions = function(self.__instructions)

        if self.__replacements:
            # During traversal, we can register replacements which cannot be
            # executed immediately, we record them and then apply them here
            self.__Replace()

            # Our uses may have changed, so we need to update them
            self.UpdateUses()

            # We need to update all uses of a replaced instruction as well, as we
            # reference instructions directly and otherwise the references would
            # remain pointing to old instructions
            self.Parent.ReplaceUses(self.__replacements)

        self.__replacements = {}

    def AddInstruction(self, instruction: Instruction):
        instruction.SetParent(self)
        self.__function.RegisterValue(instruction)
        self.__instructions.append(instruction)
        return instruction

    def Replace(self, oldInstruction: Instruction, newInstruction: Optional[Value]):
        self.__replacements[oldInstruction.Reference] = newInstruction

    def __Replace(self):
        # We search all instructions, check if they're marked for replacement,
        # and if so, plug in the new instruction right there with the same
        # reference. Afterwards, we purge empty slots
        # Other uses of the old instruction need to be updated using
        # ReplaceUses()
        for index in range(len(self.__instructions)):
            instruction = self.__instructions[index]
            if instruction.Reference in self.__replacements:
                newInstruction = self.__replacements[instruction.Reference]
                newInstruction.SetReference(instruction.Reference)
                if isinstance(newInstruction, Instruction):
                    self.__instructions[index] = newInstruction
                else:
                    # Constant value or something like that
                    self.__instructions[index] = None

        self.__instructions = [i for i in self.__instructions if i]


class Function(Value):
    def __init__(self, name, functionType: types.Function):
        super().__init__(functionType)
        self.__basicBlocks = []
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

    def _Traverse(self, function):
        self.__basicBlocks = function(self.__basicBlocks)

    def RegisterValue(self, value: Value):
        n = len(self.__values)
        value.SetReference(n)
        self.__values.append(value)
        return n

    def CreateConstant(self, constantType: types.Type, value):
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

class Program(Node):
    def __init__(self):
        self.__functions = collections.OrderedDict()
        self.__globals = collections.OrderedDict()

    @property
    def Functions(self):
        return self.__functions

    @property
    def Globals(self):
        return self.__globals

    def CreateFunction(self, name, functionType):
        f = Function(name, functionType)
        self.__functions[name] = f
        return f

    def CreateGlobalVariable(self, name, variableType):
        self.__globals[name]= variableType

    def _Traverse(self, function):
        self.__functions = function(self.__functions)

class BinaryInstruction(Instruction):
    def __init__(self, operation: OpCode, returnType: types.Type,
        v1: Value, v2: Value):
        super().__init__(operation, returnType)
        self.__values = [v1, v2]

    def ReplaceUses(self, ref, newValue):
        self._ReplaceUsesInList(self.__values, ref, newValue)

    @property
    def Uses(self):
        return [v.Reference for v in self.__values]

    @staticmethod
    def FromOperation(operation: op.Operation, returnType: types.Type,
        v1: Value, v2: Value):
        assert isinstance(returnType, types.PrimitiveType)
        assert isinstance(v1.Type, types.PrimitiveType)
        assert isinstance(v2.Type, types.PrimitiveType)

        if returnType.IsScalar():
            mapping = {
                op.Operation.ASSIGN: OpCode.ASSIGN,

                op.Operation.ADD: OpCode.ADD,
                op.Operation.MUL: OpCode.MUL,
                op.Operation.SUB: OpCode.SUB,
                op.Operation.DIV: OpCode.DIV,
                op.Operation.MOD: OpCode.MOD,
                
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
    def __init__(self, predicate: OpCode, returnType: types.Type,
        v1: Value, v2: Value):
        super().__init__(predicate, returnType)
        self.__predicate = predicate
        self.__values = [v1, v2]

    def ReplaceUses(self, ref, newValue):
        self._ReplaceUsesInList(self.__values, ref, newValue)

    @property
    def Uses(self):
        return [v.Reference for v in self.__values]

class BranchInstruction(Instruction):
    def __init__(self, trueBlock: Optional[BasicBlock],
        falseBlock: BasicBlock = None,
        predicate: Value = None):
        super().__init__(OpCode.BRANCH, types.Void())
        self.__trueBlock = trueBlock
        self.__falseBlock = falseBlock
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
    def TrueBlock(self):
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
    def __init__(self, operation: OpCode, returnType: types.Type, value: Value):
        super().__init__(operation, returnType)
        self.__operation = operation
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
    def __init__(self, value: Value, targetType: types.Type):
        super().__init__(OpCode.CAST, targetType, value)

class ReturnInstruction(Instruction):
    def __init__(self, value: Value):
        if value:
            super().__init__(OpCode.RETURN, value.Type)
        else:
            super().__init__(OpCode.RETURN, types.Void())
        self.__value = value

    @property
    def Value(self):
        return self.__value

    def ReplaceUses(self, ref, newValue):
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
    def __init__(self, returnType: types.Type,
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
    def __init__(self, memberType, variable: Value, member: str,
        accessScope: VariableAccessScope = VariableAccessScope.FUNCTION_LOCAL):
        super().__init__(OpCode.INVALID, memberType)
        self.__parent = variable
        self.__member = member
        self.__store = None
        self.__scope = accessScope

    @property
    def Parent(self):
        return self.__parent

    @property
    def Member(self):
        return self.__member

    @property
    def Scope(self):
        return self.__scope

    def SetStore(self, destination: Value):
        self.__store = destination

    @property
    def Store(self):
        return self.__store

    def ReplaceUses(self, ref, newValue):
        if self.__parent.Reference == ref:
            self.__parent = newValue

        if self.__store and self.__store.Reference == newValue:
            self.__store == newValue

    @property
    def Uses(self):
        yield self.__parent.Reference
        if self.__store:
            yield self.__store.Reference

class ShuffleInstruction(Instruction):
    def __init__(self, returnType: types.VectorType,
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
    def __init__(self, returnType: types.Type, variableName,
            accessScope: VariableAccessScope = VariableAccessScope.GLOBAL):
        super().__init__(OpCode.LOAD, returnType)
        self.__variable = variableName
        self.__store = None
        self.__scope = accessScope

    @property
    def Variable(self):
        return self.__variable

    def SetStore(self, destination):
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
    def __init__(self, returnType: types.Type, functionName: str,
        arguments = []):
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
    def __init__(self, returnType: types.Type, array, index,
        opCodes = (None, None)):
        super().__init__(opCodes[0], returnType)
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

    def SetStore(self, destination):
        self.__store = destination
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
    def __init__(self, returnType: types.Type, array: Value, index: Value):
        super().__init__(returnType, array, index,
            (OpCode.LOAD_ARRAY, OpCode.STORE_ARRAY))

class ComponentAccessInstruction(_IndexedAccessBase):
    def __init__(self, returnType: types.Type, array: Value, index: Value):
        super().__init__(returnType, array, index,
            (OpCode.VECTOR_GET, OpCode.VECTOR_SET))

class DeclareVariableInstruction(Instruction):
    def __init__(self, variableType, name = None,
        scope = VariableAccessScope.GLOBAL):
        super().__init__(OpCode.NEW_VARIABLE, variableType)
        self.__name = name
        self.__scope = scope

    @property
    def Name(self):
        return self.__name

    @property
    def Scope(self):
        return self.__scope

    def SetReference(self, reference):
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

    def __FormatType(self, t: types.Type):
        return str(t)

    def __FormatLabel(self, label):
        return f'label bb_{label.Reference}'

    def Print(self, i):
        self.v_Visit(i)

    def v_Function(self, function, ctx=None):
        self.__Print('function', function.Name,
            '(' + ', '.join(map(str, function.Type.GetArguments())) + ')')
        for basicBlock in function.BasicBlocks:
            self.v_Visit(basicBlock, 1)
        self.__Print()

    def v_BasicBlock(self, bb, ctx=None):
        self.__Print(f'bb_{bb.Reference}: ')
        for i in bb.Instructions:
            self.__Print(' ' * (ctx * 4), end='')
            self.v_Visit(i, ctx + 1)
    
    def v_ReturnInstruction(self, ri, ctx=None):
        if ri.Value:
            self.__Print('ret', self.__FormatReference(ri.Value))
        else:
            self.__Print('ret')

    def v_CastInstruction(self, ci, ctx=None):
        self.__Print(self.__FormatReference(ci), '=',
            f'cast.{self.__FormatType(ci.Type)}',
            self.__FormatReference (ci.Value))

    def v_BinaryInstruction(self, bi, ctx=None):
        self.__Print(self.__FormatReference(bi), '=',
            f'{bi.OpCode.name.lower()}.{self.__FormatType(bi.Type)}',
            self.__FormatReference(bi.Values[0]),
            self.__FormatReference(bi.Values[1]))

    def v_CallInstruction(self, ci, ctx=None):
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

    def v_VariableAccessInstruction(self, li, ctx=None):
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

    def v_ArrayAccessInstruction(self, aai, ctx=None):
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

    def v_ComponentAccessInstruction(self, aai, ctx=None):
        if aai.Store:
            self.__Print(
                self.__FormatReference(aai), '=',
                f'vectorset',
                self.__FormatReference(aai.Array),
                self.__FormatReference(aai.Index),
                self.__FormatReference(aai.Store))
        else:
            self.__Print(self.__FormatReference(aai), '=',
                f'vectorget',
                self.__FormatType(aai.Type),
                self.__FormatReference(aai.Array),
                self.__FormatReference(aai.Index))

    def v_MemberAccessInstruction(self, mai, ctx=None):
        if mai.Store:
            self.__Print(self.__FormatReference(mai), '=',
                f'setmember',
                self.__FormatReference(mai.Parent),
                mai.Member,
                self.__FormatReference(mai.Store))
        else:
            self.__Print(self.__FormatReference(mai), '=',
                f'getmember',
                self.__FormatType(mai.Type),
                self.__FormatReference(mai.Parent),
                mai.Member)

    def v_BranchInstruction(self, bi, ctx=None):
        if bi.Predicate is None:
            self.__Print('branch', self.__FormatLabel(bi.TrueBlock))
        else:
            self.__Print('branch', self.__FormatReference(bi.Predicate), end='')
            self.__Print(',', self.__FormatLabel (bi.TrueBlock), end='')
            if bi.FalseBlock:
                self.__Print(',', self.__FormatLabel (bi.FalseBlock), end='')
            self.__Print()

    def v_DeclareVariableInstruction(self, dvi, ctx=None):
        scope = self.__FormatScope(dvi.Scope)
        self.__Print(self.__FormatReference(dvi), '=',
            f'var.{scope}',
            self.__FormatType(dvi.Type),
            dvi.Name)

    def v_ConstructPrimitiveInstruction(self, cpi, ctx=None):
        self.__Print(self.__FormatReference(cpi), '=',
            'constructprimitive',
            self.__FormatType(cpi.Type),
            ', '.join([self.__FormatReference(v) for v in cpi.Values]))

    def v_ShuffleInstruction(self, si, ctx=None):
        self.__Print(self.__FormatReference(si), '=',
            'shuffle',
            self.__FormatReference(si.First),
            self.__FormatReference(si.Second),
            ', '.join(map(str, si.Indices)))
