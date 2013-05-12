from collections import OrderedDict
import nsl.ast

class UnknownSymbol(Exception):
    def __init__(self, symbol):
        self.symbol = symbol

    def __str__(self):
        return "Unknown symbol: '{}'".format (self.symbol)

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
                raise UnknownFunction ()

        def GetFirst(tuple):
            return tuple[0]

        def IsValidCandidate(candidate):
            return candidate[0] >= 0

        candidates = self.functions [functionName]
        ranking = list (filter (IsValidCandidate,
                          sorted ([(candidate.Match (argumentTypes), candidate)
                                   for candidate in candidates],
                          key=GetFirst)))

        if len(ranking) == 0:
            raise UnknownFunction()

        if len(ranking) == 1:
            return ranking[0][1]

        # if the first two candidates have the same score, the overload is
        # ambiguous
        if ranking[0][0] == ranking[1][0]:
            raise UnknownFunction()

        return ranking[0][1]

class Type:
    '''Base class for all type calculations.'''
    def GetName(self):
        assert False, 'Base type has no defined name'
        return 'undef'

    def IsPrimitive(self):
        return False

    def IsComplex(self):
        return False

    def IsArray(self):
        return False

    def HasSemantic(self):
        return False

    def NeedsResolve (self):
        return False

def Resolve(type, scope):
    if type.NeedsResolve ():
        result = scope.GetVariableType (type.GetName ())
        assert not isinstance (result, UnresolvedType)
        return result
    else:
        return type

def ResolveFunction(type, scope, argumentTypes):
    if type.NeedsResolve ():
        return scope.GetFunctionType (type.GetName (), argumentTypes)
    else:
        return type

class UnresolvedType:
    def __init__(self, name):
        self.name = name

    def NeedsResolve(self):
        return True

    def GetName (self):
        return self.name

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
        if left.IsVector ():
            if left.GetSize () == 1:
                left = left.GetType ()
        if right.IsVector ():
            if right.GetSize () == 1:
                right = right.GetType ()

        # Our primitive types are all compatible
        if left.IsVector() and right.IsVector ():
            return left.GetSize () == right.GetSize()
        elif left.IsMatrix () and right.IsMatrix ():
            return ((left.GetRows () == right.GetRows ()) and
                    (left.GetColumns () == right.GetColumns ()))
        elif left.IsScalar () and right.IsScalar ():
            return True
        else:
            if (left.IsVector () or left.IsMatrix ()) and right.IsScalar ():
                # Vector/Matrix and scalars can be combined
                return True
            elif (right.IsVector () or right. IsMatrix ()) and left.IsScalar ():
                # Scalar and vector/matrix can be combined
                return True
            elif left.IsMatrix () and right.IsScalar ():
                # matrix * vector
                return left.GetColumns () == right.GetSize ()
            else:
                return False
    elif left.IsPrimitive () and right.IsComplex ():
        return False
    elif left.IsComplex () and right.IsPrimitive ():
        return False
    else:
        # both complex
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

def Promote(t1, t2):
    # TODO: scalars can get promoted to 1-element vectors
    if isinstance (t1, VectorType) and isinstance (t2, VectorType):
        assert t1.GetSize () == t2.GetSize (), \
            'Cannot combine {} and {} to unified vector type'.format (t1, t2)
        return VectorType (Promote (t1.GetType (), t2.GetType (),
                                    t1.GetSize ()))
    if isinstance (t1, Float) or isinstance (t2, Float):
        return Float ()
    elif isinstance (t1, Integer) or isinstance (t2, Integer):
        return Integer ()
    else:
        return t1

def GetCombinedType(expr, left, right):
    '''Get the type of an expression combining two elements,
    one of type left and one of type right. This performs the standard
    type promotion rules (int->float, int->uint) and expects that both
    types are compatible to start with.

    @param expr: A binary expression'''
    assert IsCompatible (left, right), 'Incompatible types'
    # must be primitive type, we don't support operations on
    # complex types
    assert isinstance (left, PrimitiveType)
    assert isinstance (right, PrimitiveType)

    if nsl.ast.IsComparison (expr.GetOperation ()):
        return Integer ()

    if left == right:
        return left

    if left.IsMatrix () and right.IsScalar ():
        return right
    elif (left.IsVector () or left.IsMatrix ()) and right.IsScalar ():
        # Vector/Matrix and scalars can be combined
        return left
    elif left.IsScalar () and (right.IsVector () or right.IsMatrix ()):
        # Scalar and vector/matrix can be combined
        return right

    # otherwise, promote
    return Promote (left, right)

def IsValidInput(outputStructType, inputStructType):
    return False

class PrimitiveType(Type):
    '''Primitive, or built-in type base class.'''
    def IsPrimitive(self):
        return True

    def __eq__(self, other):
        '''All primitive types have a valid __repr__ implementation, so just
        use that.'''
        return repr(self) == repr(other)

    def __init__(self):
        self.semantic = None

    def SetSemantic(self, semantic):
        assert semantic is not None
        self.semantic = semantic

    def GetSemantic(self):
        return self.semantic

    def HasSemantic(self):
        return self.semantic is not None

    def IsScalar (self):
        return False

    def IsVector (self):
        return False

    def IsMatrix (self):
        return False

class ArrayType(Type):
    def __init__(self, elementType, arraySize):
        Type.__init__(self)
        assert arraySize > 0
        self.elementType = elementType
        self.arraySize = arraySize

    def GetSize(self):
        return self.arraySize

    def GetType(self):
        return self.elementType

    def GetName(self):
        return self.GetType().GetName () + ' [' + str(self.arraySize) + ']'

    def __str__(self):
        return '{} [{}]'.format (self.elementType, self.arraySize)

    def __repr__(self):
        return 'ArrayType ({}, {})'.format (repr(self.elementType),
                                            self.arraySize)

class ComplexType(Type):
    def IsComplex(self):
        return True

class StructType(ComplexType):
    def __init__(self, name, declarations):
        self.members = Scope ()
        self.name = name
        for (name, type) in declarations.items ():
            self.members.RegisterVariable(name, type)
        self.declarations = declarations

    def __str__(self):
        return 'struct {}'.format(self.name)

    def __repr__(self):
        return 'StructType ({}, {})'.format (repr(self.name),
                                             repr(self.declarations))

    def GetName(self):
        return self.name

    def GetMembers(self):
        return self.members

    def GetVariableType(self, variableName):
        return self.members.GetVariableType (variableName)

class ClassType(StructType):
    '''Struct type with additional support for member functions.'''
    def __init__(self, name, declarations, functions):
        StructType.__init__(self, name, declarations)
        for func in functions:
            self.members.RegisterFunction(func.GetName (), func)

    def GetFunctionType(self, functionName, argumentTypes):
        return self.members.GetFunctionType(functionName, argumentTypes)

class Function(Type):
    def __init__(self, name, returnType, arguments):
        self.returnType = returnType
        self.arguments = arguments
        self.name = name

    def Resolve(self, scope):
        self.returnType = Resolve(self.returnType, scope)
        tmpArgs = self.arguments
        self.arguments = OrderedDict ()
        counter = 0
        for arg in tmpArgs:
            if arg.HasName ():
                self.arguments [arg.GetName ()] = Resolve (arg.GetType (), scope)
            else:
                # generate an invalid name
                self.arguments ['$unnamed_arg${}'.format (counter)] = Resolve (arg.GetType (), scope)
            counter += 1

    def GetName(self):
        return self.name

    def GetMangledName(self):
        return '@{}->{}`{}'.format (self.name, str(self.returnType),
                                    ','.join ([str(arg) for arg in self.arguments.values ()]))

    def __str__(self):
        return 'function {0} ({1}) -> {2}'.format (self.name,
            ', '.join(self.arguments.keys ()), self.returnType)

    def __repr__(self):
        return 'Function (\'{}\', {}, [{}])'.format (self.name, repr(self.returnType),
                                                 ', '.join ([repr(arg) for arg in self.arguments.keys ()]))

    def Match(self, parameterList):
        '''Match the function signature against a parameter list.
        @return: A score indicating how well the function signature
            matches the argument list. 0 means all types match,
            negative numbers indicate the function does not match the
            signature at all and positive numbers indicate how many
            (implicit) conversions have to be performed to match.'''
        if (len(parameterList) != len(self.arguments)):
            return -1
        else:
            return sum([Match (e[0], e[1]) for e in zip (parameterList, self.arguments.values ())])

    def GetReturnType (self):
        '''The return type of this function, potentially unresolved.'''
        return self.returnType

    def GetArguments(self):
        return self.arguments

class Void(Type):
    def GetName(self):
        return 'void'

    def __repr__(self):
        return 'Void ()'

    def __str__(self):
        return 'void'

class ScalarType(PrimitiveType):
    def IsScalar (self):
        return True

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
        assert isinstance(componentType, PrimitiveType)
        PrimitiveType.__init__(self)
        self.componentType = componentType
        self.componentCount = componentCount

    def IsVector(self):
        return True

    def GetType(self):
        return self.componentType

    def GetSize(self):
        return self.componentCount

    def GetName(self):
        return '{}{}'.format (self.componentType.GetName (),
                              self.componentCount)

    def __repr__(self):
        return 'VectorType (' + repr(self.componentType) + \
            ', {})'.format(self.componentCount)

    def __str__(self):
        return '{}{}'.format(self.componentType, self.componentCount)

class MatrixOrder:
    ROW_MAJOR       = 1
    COLUMN_MAJOR    = 2

class MatrixType(PrimitiveType):
    def __init__(self, componentType, rows, columns,
                 order = MatrixOrder.ROW_MAJOR):
        assert rows > 0 and columns > 0
        assert isinstance(componentType, PrimitiveType)
        PrimitiveType.__init__(self)
        self.componentType = componentType
        self.size = (rows, columns,)
        self.order = order

    def IsMatrix (self):
        return True

    def GetOrder (self):
        return self.order

    def GetRowCount (self):
        return self.size [0]

    def GetColumnCount (self):
        return self.size [1]

    def GetType(self):
        return self.componentType

    def GetSize(self):
        return self.size

    def GetName(self):
        return str(self)

    def __repr__(self):
        return 'MatrixType (' + repr(self.componentType) + \
            ', {}, {})'.format(self.GetRowCount (), self.GetColumnCount ())

    def __str__(self):
        return '{}{}x{}'.format (self.componentType.GetName (),
                              self.GetRowCount (),
                              self.GetColumnCount ())

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