from . import op
from enum import Enum
import collections
from . import types
from .ast import Visitor

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
    def __init__(self, returnType: types.Type):
        super().__init__(returnType)
        self.__parent = None

    def SetParent(self, basicBlock):
        self.__parent = basicBlock

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

class Function(Value):
    def __init__(self, name, functionType):
        super().__init__(functionType)
        self.__basicBlocks = []
        self.__name = name
        self.__values = []

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

    @property
    def Name(self):
        return self.__name

class Program:
    def __init__(self):
        self.__functions = collections.OrderedDict()

    def CreateFunction(self, name, functionType):
        f = Function(name, functionType)
        self.__functions[name] = f
        return f

class OpCode(Enum):
    ASSIGN = 1

    # Binary
    ADD = 102
    SUB = 103
    MUL = 104
    DIV = 105
    MOD = 106

    # Unary
    UA_ADD = 120
    UA_SUB = 121

    # comparison
    CMP_GT = 200
    CMP_LT = 201
    CMP_LE = 202
    CMP_GE = 203
    CMP_NE = 204
    CMP_EQ = 205

    # logic
    LG_OR   = 300
    LG_AND  = 301
    LG_NOT  = 302

    BIT_OR  = 400
    BIT_AND = 401
    BIT_XOR = 402
    BIT_NOT = 403

    BRANCH = 10500
    RETURN = 10501

    CAST = 10131

class BinaryInstruction(Instruction):
    def __init__(self, operation: OpCode, returnType: types.Type,
        v1: Value, v2: Value):
        super().__init__(returnType)
        self.__operation = operation
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
            op.Operation.CMP_LT: OpCode.CMP_LT
        }

        return BinaryInstruction(mapping[operation], returnType,
            v1, v2)

    @property
    def Operation(self):
        return self.__operation

    @property
    def Values(self):
        return self.__values

class CompareInstruction(Instruction):
    def __init__(self, predicate: OpCode, returnType: types.Type,
        v1: Value, v2: Value):
        super().__init__(returnType)
        self.__predicate = predicate
        self.__values = [v1, v2]

class BranchInstruction(Instruction):
    def __init__(self, trueBlock: BasicBlock,
        falseBlock: BasicBlock = None,
        predicate: Value = None):
        super().__init__(types.Void())
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
        super().__init__(returnType)
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
            super().__init__(value.Type)
        else:
            super().__init__(types.Void())
        self.__value = value

    @property
    def Value(self):
        return self.__value

class MemberAccessInstruction(Instruction):
    def __init__(self, memberType, variable, member):
        super().__init__(memberType)
        self.__parent = variable
        self.__member = member
        self.__store = None

    @property
    def Parent(self):
        return self.__parent

    @property
    def Member(self):
        return self.__member

    def SetStore(self, destination: Value):
        self.__store = destination

    @property
    def Store(self):
        return self.__store

class ConstructPrimitiveInstruction(Instruction):
    def __init__(self, primitiveType: types.PrimitiveType, values):
        super().__init__(primitiveType)
        self.__values = values

    @property
    def Arguments(self):
        return self.__values

class VariableAccessInstruction(Instruction):
    def __init__(self, returnType: types.Type, variableName):
        super().__init__(returnType)
        self.__variable = variableName
        self.__store = None

    @property
    def Variable(self):
        return self.__variable

    def SetStore(self, destination):
        self.__store = destination

    @property
    def Store(self):
        return self.__store

class CallInstruction(Instruction):
    def __init__(self, returnType: types.Type, functionName: str,
        arguments = [],
        object = None):
        super().__init__(returnType)
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
        super().__init__(returnType)
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

    @property
    def Store(self):
        return self.__store


class InstructionPrinter(Visitor):
    def __init__(self):
        pass

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
        print('function', function.Name,
            '(' + ', '.join(map(str, function.Type.GetArguments())) + ')')
        for basicBlock in function.BasicBlocks:
            self.v_Visit(basicBlock, 1)
        print()

    def v_BasicBlock(self, bb, ctx=None):
        print(f'bb_{bb.Reference}: ')
        for i in bb.Instructions:
            print(' ' * (ctx * 4), end='')
            self.v_Visit(i, ctx + 1)

    def v_VariableAccessInstruction(self, li, ctx=None):
        if li.Store:
            print('store',
                self.__FormatType(li.Type),
                li.Variable,
                self.__FormatReference(li.Store))
        else:
            print(self.__FormatReference(li), '=',
                'load',
                self.__FormatType(li.Type),
                li.Variable)
    
    def v_ReturnInstruction(self, ri, ctx=None):
        if ri.Value:
            print('ret', self.__FormatReference(ri.Value))
        else:
            print('ret')

    def v_CastInstruction(self, ci, ctx=None):
        print(self.__FormatReference(ci), '=',
            f'cast.{self.__FormatType(ci.Type)}',
            self.__FormatReference (ci.Value))

    def v_BinaryInstruction(self, bi, ctx=None):
        print(self.__FormatReference(bi), '=',
            f'{bi.Operation.name.lower()}.{self.__FormatType(bi.Type)}',
            self.__FormatReference(bi.Values[0]),
            self.__FormatReference(bi.Values[1]))

    def v_CallInstruction(self, ci, ctx=None):
        if ci.Object:
            print(self.__FormatReference(ci), '=',
                'call',
                self.__FormatReference(ci.Object),
                self.__FormatType(ci.Type),
                ci.Function,
                ", ".join([self.__FormatReference(arg) for arg in ci.Arguments]))
        else:
            print(self.__FormatReference(ci), '=',
                'call',
                self.__FormatType(ci.Type),
                ci.Function,
                ", ".join([self.__FormatReference(arg) for arg in ci.Arguments]))

    def v_ConstructPrimitiveInstruction(self, cpi, ctx=None):
        print(self.__FormatReference(cpi), '=',
            f'construct.{self.__FormatType(cpi.Type)}',
            ", ".join([self.__FormatReference(arg) for arg in cpi.Arguments]))

    def v_ArrayAccessInstruction(self, aai, ctx=None):
        if aai.Store:
            print('store',
                self.__FormatReference(aai.Array),
                self.__FormatReference(aai.Index),
                self.__FormatReference(aai.Store))
        else:
            print(self.__FormatReference(aai), '=',
                'load',
                self.__FormatType(aai.Type),
                self.__FormatReference(aai.Array),
                self.__FormatReference(aai.Index))

    def v_MemberAccessInstruction(self, mai, ctx=None):
        if mai.Store:
            print('setmember',
                self.__FormatReference(mai.Parent),
                mai.Member,
                self.__FormatReference(mai.Store))
        else:
            print(self.__FormatReference(mai), '=',
                'getmember',
                self.__FormatType(mai.Type),
                self.__FormatReference(mai.Parent),
                mai.Member)

    def v_BranchInstruction(self, bi, ctx=None):
        if bi.Predicate is None:
            print('branch', self.__FormatLabel(bi.TrueBlock))
        else:
            print('branch', self.__FormatReference(bi.Predicate), end='')
            print(',', self.__FormatLabel (bi.TrueBlock), end='')
            if bi.FalseBlock:
                print(',', self.__FormatLabel (bi.FalseBlock), end='')
            print()