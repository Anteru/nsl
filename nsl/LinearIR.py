from . import op
from enum import Enum
import collections
from . import types
from .ast import Visitor


class OpCode(Enum):
    INVALID = 0
    ASSIGN = 0x0_0001

    BRANCH = 0x0_0101
    RETURN = 0x0_0102
    CALL = 0x0_0201

    CAST = 0x0_1001

    # Binary
    ADD = 0x1_0002
    SUB = 0x1_0003
    MUL = 0x1_0004
    DIV = 0x1_0005
    MOD = 0x1_0006

    # comparison
    CMP_GT = 0x1_0100
    CMP_LT = 0x1_0101
    CMP_LE = 0x1_0102
    CMP_GE = 0x1_0103
    CMP_NE = 0x1_0104
    CMP_EQ = 0x1_0105

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
    LOAD_ARRAY = 0x6_0002
    STORE = 0x6_1001
    STORE_ARRAY = 0x6_1002
    
class Value:
    def __init__(self, valueType: types.Type):
        self.__type = valueType
        self.__number = id(self)

    @property
    def Type(self):
        return self.__type

    @property
    def Reference(self):
        return f'{self.__number}'

    def SetReference(self, number):
        self.__number = number

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

class Instruction(ValueUser):
    def __init__(self, opCode: OpCode, returnType: types.Type):
        super().__init__(returnType)
        self.__parent = None
        self.__opcode = opCode

    def SetParent(self, basicBlock):
        self.__parent = basicBlock

    @property
    def OpCode(self):
        return self.__opcode

    def _SetOpCode(self, opcode):
        self.__opcode = opcode

class BasicBlock(Value):
    def __init__(self, function):
        super().__init__(types.Void())
        self.__instructions = []
        self.__predecessors = []
        self.__successors = []
        self.__function = function

    @property
    def Instructions(self):
        return self.__instructions

    def AddInstruction(self, instruction: Instruction):
        instruction.SetParent(self)
        self.__function.RegisterValue(instruction)
        self.__instructions.append(instruction)
        return instruction

class Function(Value):
    def __init__(self, name, functionType):
        super().__init__(functionType)
        self.__basicBlocks = []
        self.__name = name
        self.__values = []
        self.__constants = []

    def CreateBasicBlock(self):
        bb = BasicBlock (self)
        self.RegisterValue(bb)
        self.__basicBlocks.append (bb)
        return bb
    
    @property
    def BasicBlocks(self):
        return self.__basicBlocks

    def RegisterValue(self, value: Value):
        n = len(self.__values)
        value.SetReference(n)
        self.__values.append(value)
        return n

    def RegisterConstant(self, value: ConstantValue):
        self.__constants.append (value)
        return self.RegisterValue (value)
    
    @property
    def Constants(self):
        return self.__constants

    @property
    def Name(self):
        return self.__name

class Program:
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

    @staticmethod
    def FromOperation(operation: op.Operation, returnType: types.Type,
        v1: Value, v2: Value):
        mapping = {
            op.Operation.ADD: OpCode.ADD,
            op.Operation.MUL: OpCode.MUL,
            op.Operation.SUB: OpCode.SUB,
            op.Operation.DIV: OpCode.DIV,
            op.Operation.ASSIGN: OpCode.ASSIGN,
            
            op.Operation.CMP_GT: OpCode.CMP_GT,
            op.Operation.CMP_LT: OpCode.CMP_LT,
            op.Operation.CMP_EQ: OpCode.CMP_EQ,
        }

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

class BranchInstruction(Instruction):
    def __init__(self, trueBlock: BasicBlock,
        falseBlock: BasicBlock = None,
        predicate: Value = None):
        super().__init__(OpCode.BRANCH, types.Void())
        self.__trueBlock = trueBlock
        self.__falseBlock = falseBlock
        self.__predicate = predicate

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

class VariableAccessScope(Enum):
    GLOBAL = 0
    FUNCTION_ARGUMENT = 1
    FUNCTION_LOCAL = 2

class MemberAccessInstruction(Instruction):
    def __init__(self, memberType, variable, member,
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

class CallInstruction(Instruction):
    def __init__(self, returnType: types.Type, functionName: str,
        arguments = [],
        object = None):
        super().__init__(OpCode.CALL, returnType)
        self.__function = functionName
        self.__arguments = arguments
        self.__object = object

    @property
    def Arguments(self):
        return self.__arguments

    @property
    def Function(self):
        return self.__function

    @property
    def Object(self):
        return self.__object

class ArrayAccessInstruction(Instruction):
    def __init__(self, returnType: types.Type, array, index):
        super().__init__(OpCode.LOAD_ARRAY, returnType)
        self.__array = array
        self.__index = index
        self.__store = None

    @property
    def Array(self):
        return self.__array

    @property
    def Index(self):
        return self.__index

    @property
    def Scope(self):
        return self.__scope

    def SetStore(self, destination):
        self.__store = destination

    @property
    def Store(self):
        return self.__store

class ComponentAccessInstruction(Instruction):
    def __init__(self, returnType: types.Type, array, index):
        super().__init__(OpCode.INVALID, returnType)
        self.__array = array
        self.__index = index
        self.__store = None

    @property
    def Array(self):
        return self.__array

    @property
    def Index(self):
        return self.__index

    @property
    def Scope(self):
        return self.__scope

    def SetStore(self, destination):
        self.__store = destination

    @property
    def Store(self):
        return self.__store

class DeclareVariableInstruction(Instruction):
    def __init__(self, variableType, name = None,
        scope = VariableAccessScope.GLOBAL):
        super().__init__(OpCode.INVALID, variableType)
        self.__name = name
        self.__scope = scope

    @property
    def Name(self):
        return self.__name

    @property
    def Scope(self):
        return self.__scope

class InstructionPrinter(Visitor):
    def __init__(self, printFunction=print):
        self.__printFunction = printFunction

    def __Print(self, *args, end='\n'):
        self.__printFunction(*args, end=end)

    def __FormatReference(self, v: Value):
        if isinstance(v, ConstantValue):
            return v.Value
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
        if ci.Object:
            self.__Print(self.__FormatReference(ci), '=',
                'call',
                self.__FormatReference(ci.Object),
                self.__FormatType(ci.Type),
                ci.Function,
                ", ".join([self.__FormatReference(arg) for arg in ci.Arguments]))
        else:
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

    def v_ComponentAccessInstruction(self, cai, ctx=None):
        if cai.Store:
            self.__Print(self.__FormatReference(cai), '=',
                f'setelement',
                self.__FormatReference(cai.Array),
                self.__FormatReference(cai.Index),
                self.__FormatReference(cai.Store))
        else:
            self.__Print(self.__FormatReference(cai), '=',
                f'getelement',
                self.__FormatType(cai.Type),
                self.__FormatReference(cai.Array),
                self.__FormatReference(cai.Index))

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
        if dvi.Name:
            self.__Print(f'var.{scope}',
                self.__FormatType(dvi.Type),
                dvi.Name)
        else:
            self.__Print(self.__FormatReference(dvi), '=',
                f'var.{scope}',
                self.__FormatType(dvi.Type))
