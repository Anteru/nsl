from collections import OrderedDict
from nsl import op, Errors
from enum import Enum

class UnknownSymbol(Exception):
    def __init__(self, symbol):
        self.__symbol = symbol

    def __str__(self):
        return "Unknown symbol: '{}'".format (self.__symbol)

class UnknownFunction(Exception):
    pass

class NotMatchingFunctionOverload(Exception):
    pass

class InvalidDeclaration(Exception):
    def __init__(self, message):
        self.message = message

class Scope:
    def __init__(self, parent = None):
        # We store everything in ordered dicts so we can use
        # this for structures/classes without further modification
        self.symbols = OrderedDict ()
        self.parent = parent
        self.functions = OrderedDict ()

        self.registeredObjects = set ()
        
    def GetSymbolNames(self):
        return self.symbols.keys()
    
    def HasParent(self):
        return self.parent is not None
    
    def GetParent(self):
        return self.parent

    def RegisterVariable(self, symbol, typeinfo):
        assert isinstance (typeinfo, Type), 'Expected Type instance but got {}'.format (type(typeinfo))
        assert symbol not in self.symbols

        if symbol in self.registeredObjects:
            raise InvalidDeclaration ("Cannot define variable '{}': A function with that name already exists in the current scope.".format (symbol))

        self.symbols [symbol] = typeinfo
        self.registeredObjects.add (symbol)

    def GetVariableType (self, symbol):
        if symbol in self.symbols:
            return self.symbols [symbol]
        else:
            if self.parent is not None:
                return self.parent.GetVariableType (symbol)
            else:
                raise UnknownSymbol(symbol)

    def RegisterFunction(self, functionName, typeinfo):
        if functionName in self.registeredObjects and functionName in self.symbols:
            raise InvalidDeclaration ("Cannot define function '{}': A variable with that name already exists in the current scope.".format (functionName))

        if not functionName in self.functions:
            self.functions [functionName] = []
        self.functions [functionName].append (typeinfo)
        self.registeredObjects.add (functionName)

    def GetFunctionType(self, functionName, argumentTypes):
        '''Get a matching function.

        @param argumentTypes: The type of each function parameter.'''

        # Resolve overloaded functions
        if not functionName in self.functions:
            if not self.parent is None:
                return self.parent.GetFunctionType(functionName,
                                                   argumentTypes)
            else:
                Errors.ERROR_UNKNOWN_FUNCTION_CALL.Raise (functionName)

        def GetFirst(t):
            return t[0]

        def IsValidCandidate(candidate):
            return candidate[0] >= 0

        candidates = self.functions [functionName]
        ranking = list (filter (IsValidCandidate,
                          sorted ([(candidate.Match (argumentTypes), candidate)
                                   for candidate in candidates],
                          key=GetFirst)))

        if len(ranking) == 0:
            Errors.ERROR_NO_MATCHING_OVERLOAD_FUNCTION_CALL.Raise (functionName)

        if len(ranking) == 1:
            return ranking[0][1]

        # if the first two candidates have the same score, the overload is
        # ambiguous
        if ranking[0][0] == ranking[1][0]:
            Errors.ERROR_AMBIGUOUS_FUNCTION_CALL.Raise (functionName)

        return ranking[0][1]

class Type:
    '''Base class for all type calculations.'''
    def GetName(self):
        assert False, 'Base type has no defined name'
        return None

    def IsPrimitive(self):
        return False

    def IsAggregate(self):
        return False

    def IsArray(self):
        return False

    def HasSemantic(self):
        return False

    def NeedsResolve (self):
        return False
    
    def GetElementType(self):
        '''For vector, matrix and array types, return the element type.
        
        For multi-dimensional arrays this will return the innermost basic type.'''
        return self

def Resolve(theType, scope):
    if theType.NeedsResolve ():
        result = scope.GetVariableType (theType.GetName ())
        assert not isinstance (result, UnresolvedType)
        return result
    else:
        return theType

def ResolveFunction(theType, scope, argumentTypes):
    if theType.NeedsResolve ():
        return scope.GetFunctionType (theType.GetName (), argumentTypes)
    else:
        return theType

class UnresolvedType:
    def __init__(self, name):
        self.__name = name

    def NeedsResolve(self):
        return True

    def GetName (self):
        return self.__name

def IsCompatible(left, right):
    '''Check if two types are compatible.

    All NSL numeric types are compatible. Arrays are compatible if
    they consist of the same element type and have the same size.
    Structs must have the same type to be compatible, which is
    stricter than the DX rules (which allow input structs to consume
    a subset of the output struct elements, i.e. the first n elements.'''
    if left.IsArray() and not right.IsArray ():
        return False
    elif not left.IsArray () and right.IsArray ():
        return False
    elif left.IsArray() and right.IsArray ():
        if left.GetSize() != right.GetSize ():
            return False
        else:
            return IsCompatible (left.GetType (), right.GetType ())
    elif left.IsPrimitive() and right.IsPrimitive():
        if isinstance(left, Void) or isinstance(right, Void):
            return isinstance (left, Void) and isinstance (right, Void)

        # reduce single-component vector types to scalars
        if left.IsVector () and left.GetSize () == 1:
                left = left.GetElementType ()
        if right.IsVector () and right.GetSize () == 1:
                right = right.GetElementType ()

        # Our primitive types are all compatible
        if left.IsVector() and right.IsVector ():
            return left.GetSize () == right.GetSize()
        elif left.IsMatrix () and right.IsMatrix ():
            return ((left.GetRows () == right.GetRows ()) and
                    (left.GetColumns () == right.GetColumns ()))
        elif left.IsScalar () and right.IsScalar ():
            return True
        else:
            return False
    elif left.IsPrimitive () and right.IsAggregate():
        return False
    elif left.IsAggregate () and right.IsPrimitive ():
        return False
    else:
        # both aggregate
        return left == right

def Match(leftType, rightType):
    '''Match two types.
    @return How well the two types match. 0 indicates the types are
        equal, 1 indicates that the right type has to be implicitly
        converted to the left and -1 means the types are not
        convertible.'''
    if not IsCompatible(leftType, rightType):
        return -1
    elif (leftType == rightType):
        return 0
    else:
        return 1

class ExpressionType:
    '''Describe the type of an expression.

    Result is the result type of the complete expression, once all operands
    have been taken into account.

    For unary expressions, operands must have length 1.
    For binary expressions, operands must have length 2.
    For ternary expressions, operands must have length 3.'''
    def __init__ (self, result, operands):
        self._result = result
        self._operands = operands

    def GetReturnType (self):
        return self._result

    def GetOperandType (self, index):
        return self._operands [index]
    
def _MergeScalarTypes(left, right):
    '''Given two scalar types, return the wider type.'''
    assert isinstance (left, ScalarType)
    assert isinstance (right, ScalarType)
    
    if isinstance (left, Float) or isinstance (right, Float):
        return Float ()
    
    if isinstance (left, Integer) or isinstance (right, Integer):
        return Integer ()
    
    assert isinstance (left, UnsignedInteger)
    assert isinstance (right, UnsignedInteger)
    
    return UnsignedInteger ()

def _MergePrimitiveTypes(left, right):
    if isinstance(left, ScalarType) and isinstance(right, ScalarType):
        return _MergeScalarTypes(left, right)
    elif isinstance (left, VectorType) and isinstance(right, VectorType):
        assert (left.GetSize () == right.GetSize ())
        return VectorType (
            _MergeScalarTypes (
                left.GetElementType (), 
                right.GetElementType()),
            left.GetSize ())
    elif isinstance (left, MatrixType) and isinstance (right, MatrixType):
        assert (left.GetSize () == right.GetSize ())        
        return VectorType (
            _MergeScalarTypes (
                left.GetElementType (), 
                right.GetElementType()),
            left.GetRowCount (), left.GetColumnCount ())
        
def ResolveBinaryExpressionType (operation, left, right):
    '''Get the type of an expression combining two elements,
    one of type left and one of type right. This performs the standard
    type promotion rules (``int->float``, ``int->uint``) and expects that both
    types are compatible to start with.

    :param operation: The operation, must be a binary operation
    :param left: The left operand type
    :param right: The right operand type'''
    # must be primitive type, we don't support operations on
    # aggregate types
    assert isinstance (left, PrimitiveType), '{} is not a primitive type'.format (left)
    assert isinstance (right, PrimitiveType), '{} is not a primitive type'.format (right)
    assert isinstance (operation, op.Operation)

    if op.IsComparison (operation):
        # Cast may be still necessary if we compare integers with floats
        baseType = _MergePrimitiveTypes (left, right)
        return ExpressionType (Integer (), [baseType, baseType])
    
    if operation == op.Operation.MUL and (left.IsMatrix () and right.IsVector ()):
        if left.GetColumnCount () != right.GetSize ():
            Errors.ERROR_INCOMPATIBLE_TYPES.Raise (left, right)
        baseType = _MergeScalarTypes(left.GetElementType (), right.GetElementType ())
        
        return ExpressionType (VectorType (baseType, left.GetRowCount ()),
            [
                MatrixType (baseType, left.GetRowCount (), left.GetColumnCount ()),
                VectorType (baseType, right.GetSize ())
            ])

    if left == right:
        return ExpressionType (left, [left, right])
    
    # make sure both are of the same type class (i.e. scalar, matrix or vector)
    if left.GetKind () != right.GetKind ():
        Errors.ERROR_INCOMPATIBLE_TYPES.Raise (left, right)
            
    baseType = _MergePrimitiveTypes(left, right)
    return ExpressionType (baseType, [baseType, baseType])
        

def IsValidInput(outputStructType, inputStructType):
    return False

class PrimitiveTypeKind(Enum):
    Scalar = 1
    Vector = 2
    Matrix = 3

class PrimitiveType(Type):
    '''Primitive, or built-in type base class.'''
    def IsPrimitive(self):
        return True

    def __eq__(self, other):
        '''All primitive types have a valid __repr__ implementation, so just
        use that.'''
        return repr(self) == repr(other)

    def IsScalar (self):
        return self.GetKind () == PrimitiveTypeKind.Scalar

    def IsVector (self):
        return self.GetKind () == PrimitiveTypeKind.Vector

    def IsMatrix (self):
        return self.GetKind () == PrimitiveTypeKind.Matrix
    
    def GetKind(self):
        return None

class AggregateType(Type):
    def IsAggregate(self):
        return True

class ArrayType(AggregateType):
    def __init__(self, elementType, arraySize):
        Type.__init__(self)
        assert arraySize > 0
        self.__elementType = elementType
        self.__arraySize = arraySize

    def GetSize(self):
        return self.__arraySize
    
    def GetElementType(self):
        # Need to call recursively in case we have a nested array
        return self.__elementType.GetElementType ()

    def GetName(self):
        return self.GetElementType().GetName () + ' [' + str(self.__arraySize) + ']'

    def __str__(self):
        return '{} [{}]'.format (self.__elementType, self.__arraySize)

    def __repr__(self):
        return 'ArrayType ({}, {})'.format (repr(self.__elementType),
                                            self.__arraySize)

class StructType(AggregateType):
    def __init__(self, name, declarations):
        self._members = Scope ()
        self._name = name
        for (name, elementType) in declarations.items ():
            self._members.RegisterVariable(name, elementType)
        self._declarations = declarations

    def __str__(self):
        return 'struct {}'.format(self._name)

    def __repr__(self):
        return 'StructType ({}, {})'.format (repr(self._name),
                                             repr(self._declarations))

    def GetName(self):
        return self._name

    def GetMembers(self):
        return self._members

    def GetVariableType(self, variableName):
        return self._members.GetVariableType (variableName)

class ClassType(StructType):
    '''Struct type with additional support for member functions.'''
    def __init__(self, name, declarations, functions, isInterface=False):
        super(ClassType, self).__init__(name, declarations)
        for func in functions:
            self._members.RegisterFunction(func.GetName (), func)
        self.__isInterface = isInterface

    def __str__(self):
        if self.__isInterface:
            return 'interface {}'.format(self._name)
        else:
            return 'class {}'.format(self._name)
        
    def __repr__(self):
        return 'ClassType ({}, {}, {}, {})'.format (repr(self._name),
                                                    repr(self._declarations), 
                                                    repr(self._members), 
                                                    repr(self.__isInterface))

    def GetFunctionType(self, functionName, argumentTypes):
        return self._members.GetFunctionType(functionName, argumentTypes)

    def IsInterface(self):
        return self.__isInterface

class Function(Type):
    def __init__(self, name, returnType, arguments):
        self.returnType = returnType
        self.arguments = arguments
        self.name = name

    def Resolve(self, scope):
        self.returnType = Resolve(self.returnType, scope)
        self.__argumentTypes = OrderedDict ()
        for counter, arg in enumerate (self.arguments):
            if arg.HasName ():
                self.__argumentTypes [arg.GetName ()] = Resolve (arg.GetType (), scope)
            else:
                # generate an invalid name
                self.__argumentTypes ['$unnamed_arg${}'.format (counter)] = Resolve (arg.GetType (), scope)

    def GetName(self):
        return self.name

    def GetMangledName(self):
        return '@{}->{}`{}'.format (self.name, str(self.returnType),
                                    ','.join ([str(arg) for arg in self.__argumentTypes.values ()]))

    def __str__(self):
        return 'function {0} ({1}) -> {2}'.format (self.name,
            ', '.join(self.__argumentTypes.keys ()), self.returnType)

    def __repr__(self):
        return 'Function (\'{}\', {}, [{}])'.format (self.name, repr(self.returnType),
                                                 ', '.join ([repr(arg) for arg in self.__argumentTypes.keys ()]))

    def Match(self, parameterList):
        '''Match the function signature against a parameter list.
        @return: A score indicating how well the function signature
            matches the argument list. 0 means all types match,
            negative numbers indicate the function does not match the
            signature at all and positive numbers indicate how many
            (implicit) conversions have to be performed to match.'''
        matchingArguments = self.arguments
        
        if (len(parameterList) != len(self.arguments)):
            # cut off optional arguments until it matches
            if not all ([arg.IsOptional() for arg in matchingArguments[len(parameterList):]]):
                return -1
            else:
                matchingArguments = matchingArguments[:len(parameterList)]
        
        matchingArgumentTypes = list (self.__argumentTypes.values ())[:len(parameterList)]
        
        return sum([Match (e[0], e[1]) for e in zip (parameterList, matchingArgumentTypes)])

    def GetReturnType (self):
        '''The return type of this function, potentially unresolved.'''
        return self.returnType

    def GetArguments(self):
        return self.arguments
    
    def GetArgumentTypes(self):
        return self.__argumentTypes

class Void(Type):
    def GetName(self):
        return 'void'

    def __repr__(self):
        return 'Void ()'

    def __str__(self):
        return 'void'

class ScalarType(PrimitiveType):    
    def GetKind(self):
        return PrimitiveTypeKind.Scalar

class Float(ScalarType):
    def GetName(self):
        return 'float'

    def __repr__(self):
        return 'Float ()'

    def __str__(self):
        return 'float'

class Integer(ScalarType):
    def GetName(self):
        return 'int'

    def __repr__(self):
        return 'Integer ()'

    def __str__(self):
        return 'int'

class UnsignedInteger(ScalarType):
    def GetName(self):
        return 'uint'

    def __repr__(self):
        return 'UnsignedInteger ()'

    def __str__(self):
        return 'uint'

class VectorType(PrimitiveType):
    def __init__(self, componentType, componentCount):
        assert componentCount > 0
        assert isinstance(componentType, ScalarType)
        super(VectorType, self).__init__()
        self.__componentType = componentType
        self.__componentCount = componentCount
    
    def GetElementType(self):
        return self.__componentType

    def GetSize(self):
        return self.__componentCount

    def GetName(self):
        return '{}{}'.format (self.__componentType.GetName (),
                              self.__componentCount)

    def __repr__(self):
        return 'VectorType ({}, {})'.format (repr(self.__componentType), 
                                             self.__componentCount)

    def __str__(self):
        return '{}{}'.format(self.__componentType, self.__componentCount)
    
    def GetKind(self):
        return PrimitiveTypeKind.Vector

class MatrixOrder(Enum):
    RowMajor       = 1
    ColumnMajor    = 2

class MatrixType(PrimitiveType):
    def __init__(self, componentType, rows, columns,
                 order = MatrixOrder.RowMajor):
        assert rows > 0 and columns > 0
        assert isinstance(componentType, ScalarType)
        super(MatrixType, self).__init__()
        self.__componentType = componentType
        self.__size = (rows, columns,)
        self.__order = order

    def GetOrder (self):
        return self.__order

    def GetRowCount (self):
        return self.__size [0]

    def GetColumnCount (self):
        return self.__size [1]

    def GetElementType(self):
        return self.__componentType

    def GetSize(self):
        return self.__size

    def GetName(self):
        return str(self)
    
    def GetKind(self):
        return PrimitiveTypeKind.Matrix

    def __repr__(self):
        return 'MatrixType ({}, {}, {})'.format (repr(self.__componentType),
                                                 self.GetRowCount (), 
                                                 self.GetColumnCount ())

    def __str__(self):
        return '{}{}x{}'.format (self.__componentType.GetName (),
                              self.GetRowCount (),
                              self.GetColumnCount ())
class Operator:
    pass

class BinaryOperator(Operator):
    def __init__(self, operation, leftType, rightType, resultType):
        self.operation = operation
        self.leftType = leftType
        self.rightType = rightType
        self.resultType = resultType

class UnaryOperator(Operator):
    def __init__(self, operation, inputType, resultType):
        self.operation = operation
        self.inputType = inputType
        self.resultType = resultType

class TernaryOperator(Operator):
    def __init__(self, operation, conditionType, trueExpressionType, falseExpressionType, resultType):
        self.operation = operation
        self.conditionType = conditionType
        self.trueExpressionType = trueExpressionType
        self.falseExpressionType = falseExpressionType
        self.resultType = resultType

def BuiltinTypeFactory(typeName):
    typeDict = {
        'float'         : Float (),
        'float2'        : VectorType (Float (), 2),
        'float3'        : VectorType (Float (), 3),
        'float4'        : VectorType (Float (), 4),
        'int'           : Integer (),
        'int2'          : VectorType (Integer (), 2),
        'int3'          : VectorType (Integer (), 3),
        'int4'          : VectorType (Integer (), 4),
        'uint'          : UnsignedInteger (),
        'uint2'         : VectorType (UnsignedInteger (), 2),
        'uint3'         : VectorType (UnsignedInteger (), 3),
        'uint4'         : VectorType (UnsignedInteger (), 4),
        'matrix3x3'     : MatrixType (Float (), 3, 3),
        'matrix4x4'     : MatrixType (Float (), 4, 4),
        'void'          : Void ()
    }

    return typeDict [typeName]
