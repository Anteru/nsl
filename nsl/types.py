from collections import OrderedDict
from nsl import op, Errors
from enum import Enum
import collections
from typing import List, Tuple, Optional


class UnknownSymbolException(Exception):
    def __init__(self, symbol: str):
        self.__symbol = symbol

    def GetSymbol(self) -> str:
        return self.__symbol

    def __str__(self):
        return "Unknown symbol: '{}'".format(self.__symbol)


class UnknownTypeException(Exception):
    def __init__(self, name="<unknown>"):
        self.__typename = name

    def GetName(self) -> str:
        return self.__typename

    def __str__(self):
        return "Unknown type: '{}'".format(self.__typename)


class InvalidDeclaration(Exception):
    def __init__(self, message: str):
        self.message = message


class Type:
    """Base class for all type calculations."""

    def GetName(self) -> str:
        raise Exception()

    def IsPrimitive(self) -> bool:
        return False

    def IsAggregate(self) -> bool:
        return False

    def IsArray(self) -> bool:
        return False

    def NeedsResolve(self) -> bool:
        return False

    def GetComponentType(self) -> "Type":
        """For vector, matrix and array types, return the element type."""
        return self


class UnresolvedType(Type):
    def __init__(self, name):
        self.__name = name

    def NeedsResolve(self):
        return True

    def GetName(self):
        return self.__name


class PrimitiveTypeKind(Enum):
    Scalar = 1
    Vector = 2
    Matrix = 3


class PrimitiveType(Type):
    """Primitive, or built-in type base class."""

    def IsPrimitive(self):
        return True

    def __eq__(self, other):
        """All primitive types have a valid __repr__ implementation, so just
        use that."""
        return repr(self) == repr(other)

    def IsScalar(self):
        return self.GetKind() == PrimitiveTypeKind.Scalar

    def IsVector(self):
        return self.GetKind() == PrimitiveTypeKind.Vector

    def IsMatrix(self):
        return self.GetKind() == PrimitiveTypeKind.Matrix

    def GetKind(self):
        return None


class AggregateType(Type):
    def IsAggregate(self):
        return True


class ArrayType(AggregateType):
    def __init__(self, elementType: Type, arraySize):
        Type.__init__(self)
        assert isinstance(arraySize, collections.abc.Sequence)
        assert len(arraySize) > 0
        for el in arraySize:
            assert el > 0
        self.__elementType = elementType
        self.__arraySize = tuple(arraySize)

    def IsArray(self):
        return True

    def GetSize(self):
        return self.__arraySize

    def GetComponentType(self) -> Type:
        return self.__elementType

    def WithComponentType(self, componentType):
        return ArrayType(componentType, self.__arraySize)

    def NeedsResolve(self):
        return self.__elementType.NeedsResolve()

    def GetName(self):
        return (
            self.GetComponentType().GetName()
            + " "
            + "".join(["[{}]".format(s) for s in self.__arraySize])
        )

    def __str__(self):
        return "{}{}".format(
            self.__elementType,
            "".join(["[{}]".format(s) for s in self.__arraySize]),
        )

    def __repr__(self):
        return "ArrayType ({}, {})".format(
            repr(self.__elementType), self.__arraySize
        )


class StructType(AggregateType):
    def __init__(self, name: str, declarations):
        self._members = Scope()
        self._name = name
        for name, elementType in declarations.items():
            self._members.RegisterVariable(name, elementType)
        self._declarations = declarations

    def __str__(self):
        return "{}".format(self._name)

    def __repr__(self):
        return "StructType ({}, {})".format(
            repr(self._name), repr(self._declarations)
        )

    def GetName(self):
        return self._name

    def GetMembers(self):
        return self._members

    def GetFieldType(self, variableName) -> Type:
        return self._members.GetFieldType(variableName)


class Function(Type):
    def __init__(self, name: str, returnType: Type, arguments, exported=False):
        self.returnType = returnType
        self.arguments = arguments
        self.name = name
        self.exported = exported
        self.__argumentTypes = collections.OrderedDict()

    def Resolve(self, scope):
        self.returnType = ResolveType(self.returnType, scope)
        for counter, arg in enumerate(self.arguments):
            if arg.HasName():
                self.__argumentTypes[arg.GetName()] = ResolveType(
                    arg.GetType(), scope
                )
            else:
                # generate an invalid name
                self.__argumentTypes["$arg${}".format(counter)] = ResolveType(
                    arg.GetType(), scope
                )

    def GetName(self):
        return self.name

    def GetMangledName(self):
        return "@{}->{}`{}".format(
            self.name,
            str(self.returnType),
            ",".join([str(arg) for arg in self.__argumentTypes.values()]),
        )

    def __str__(self):
        return "function {0} ({1}) -> {2}".format(
            self.name, ", ".join(self.__argumentTypes.keys()), self.returnType
        )

    def __repr__(self):
        return "Function ('{}', {}, [{}])".format(
            self.name,
            repr(self.returnType),
            ", ".join([repr(arg) for arg in self.__argumentTypes.keys()]),
        )

    def Match(self, parameterList: List[Type]) -> int:
        """Match the function signature against a parameter list.
        @return: A score indicating how well the function signature
                matches the argument list. 0 means all types match,
                negative numbers indicate the function does not match the
                signature at all and positive numbers indicate how many
                (implicit) conversions have to be performed to match."""
        matchingArguments = self.arguments

        if len(parameterList) < len(self.arguments):
            # Try to cut off optional arguments
            if not all(
                [
                    arg.IsOptional()
                    for arg in matchingArguments[len(parameterList) :]
                ]
            ):
                return -1
        elif len(parameterList) > len(self.arguments):
            # More parameters than arguments, this is not a valid overload
            return -1

        matchingArgumentTypes = list(self.__argumentTypes.values())[
            : len(parameterList)
        ]

        return sum(
            [
                Match(e[0], e[1])
                for e in zip(parameterList, matchingArgumentTypes)
            ]
        )

    def GetReturnType(self) -> Type:
        """The return type of this function, potentially unresolved."""
        return self.returnType

    def GetArguments(self):
        return self.arguments

    def GetArgumentTypes(self) -> OrderedDict[str, Type]:
        return self.__argumentTypes


class Void(Type):
    def GetName(self):
        return "void"

    def __repr__(self):
        return "Void ()"

    def __str__(self):
        return "void"


class ScalarType(PrimitiveType):
    def GetKind(self):
        return PrimitiveTypeKind.Scalar

    def GetComponentType(self):
        return self


class Float(ScalarType):
    def GetName(self):
        return "float"

    def __repr__(self):
        return "Float ()"

    def __str__(self):
        return "float"


class Integer(ScalarType):
    def GetName(self):
        return "int"

    def __repr__(self):
        return "Integer ()"

    def __str__(self):
        return "int"


class UnsignedInteger(ScalarType):
    def GetName(self):
        return "uint"

    def __repr__(self):
        return "UnsignedInteger ()"

    def __str__(self):
        return "uint"


class VectorType(PrimitiveType):
    def __init__(self, componentType: ScalarType, componentCount: int):
        assert componentCount > 0
        assert isinstance(componentType, ScalarType)
        super(VectorType, self).__init__()
        self.__componentType = componentType
        self.__componentCount = componentCount

    def GetComponentType(self) -> ScalarType:
        return self.__componentType

    def GetSize(self):
        return (self.__componentCount,)

    def GetComponentCount(self):
        return self.__componentCount

    def GetName(self):
        return "{}{}".format(
            self.__componentType.GetName(), self.__componentCount
        )

    def __repr__(self):
        return "VectorType ({}, {})".format(
            repr(self.__componentType), self.__componentCount
        )

    def __str__(self):
        return "{}{}".format(self.__componentType, self.__componentCount)

    def GetKind(self):
        return PrimitiveTypeKind.Vector

    def WithComponentType(self, componentType):
        """Return a copy of this type with a new component type."""
        return VectorType(componentType, self.__componentCount)


class MatrixType(PrimitiveType):
    def __init__(self, componentType: Type, rows: int, columns: int):
        assert rows > 0 and columns > 0
        assert isinstance(componentType, ScalarType)
        super(MatrixType, self).__init__()
        self.__componentType = componentType
        self.__size = (
            rows,
            columns,
        )

    def GetRowCount(self) -> int:
        return self.__size[0]

    def GetColumnCount(self) -> int:
        return self.__size[1]

    def GetComponentType(self) -> Type:
        return self.__componentType

    def GetSize(self) -> Tuple[int, int]:
        return self.__size

    def GetName(self) -> str:
        return str(self)

    def GetKind(self):
        return PrimitiveTypeKind.Matrix

    def __repr__(self):
        return "MatrixType ({}, {}, {})".format(
            repr(self.__componentType),
            self.GetRowCount(),
            self.GetColumnCount(),
        )

    def __str__(self):
        return "{}{}x{}".format(
            self.__componentType.GetName(),
            self.GetRowCount(),
            self.GetColumnCount(),
        )

    def WithComponentType(self, componentType):
        """Return a copy of this type with a new component type."""
        return MatrixType(componentType, self.__size[0], self.__size[1])


def IsCompatible(left, right):
    """Check if two types are compatible.

    All NSL numeric types are compatible. Arrays are compatible if
    they consist of the same element type and have the same size.
    Structs must have the same type to be compatible, which is
    stricter than the DX rules (which allow input structs to consume
    a subset of the output struct elements, i.e. the first n elements."""
    if left.IsArray() and not right.IsArray():
        return False
    elif not left.IsArray() and right.IsArray():
        return False
    elif left.IsArray() and right.IsArray():
        if left.GetSize() != right.GetSize():
            return False
        else:
            return IsCompatible(left.GetType(), right.GetType())
    elif left.IsPrimitive() and right.IsPrimitive():
        if isinstance(left, Void) or isinstance(right, Void):
            return isinstance(left, Void) and isinstance(right, Void)

        # reduce single-component vector types to scalars
        if left.IsVector() and left.GetSize() == (1,):
            left = left.GetComponentType()
        if right.IsVector() and right.GetSize() == (1,):
            right = right.GetComponentType()

        # Our primitive types are all compatible
        if left.IsVector() and right.IsVector():
            return left.GetSize() == right.GetSize()
        elif left.IsMatrix() and right.IsMatrix():
            return (left.GetRows() == right.GetRows()) and (
                left.GetColumns() == right.GetColumns()
            )
        elif left.IsScalar() and right.IsScalar():
            return True
        else:
            return False
    elif left.IsPrimitive() and right.IsAggregate():
        return False
    elif left.IsAggregate() and right.IsPrimitive():
        return False
    else:
        # both aggregate
        return left == right


def Match(leftType, rightType):
    """Match two types.
    @return How well the two types match. 0 indicates the types are
            equal, 1 indicates that the right type has to be implicitly
            converted to the left and -1 means the types are not
            convertible."""
    assert isinstance(leftType, Type)
    assert isinstance(rightType, Type)

    if not IsCompatible(leftType, rightType):
        return -1
    elif leftType == rightType:
        return 0
    else:
        return 1


class ExpressionType:
    """Describe the type of an expression.

    Result is the result type of the complete expression, once all operands
    have been taken into account.

    For unary expressions, operands must have length 1.
    For binary expressions, operands must have length 2.
    For ternary expressions, operands must have length 3."""

    def __init__(self, result, operands):
        self._result = result
        self._operands = operands

    def GetReturnType(self):
        return self._result

    def GetOperandType(self, index):
        return self._operands[index]


def _GetCommonScalarType(left, right):
    """Given two scalar types, get a common scalar type."""
    assert isinstance(left, ScalarType)
    assert isinstance(right, ScalarType)

    if isinstance(left, Float) or isinstance(right, Float):
        return Float()

    if isinstance(left, Integer) or isinstance(right, Integer):
        return Integer()

    assert isinstance(left, UnsignedInteger)
    assert isinstance(right, UnsignedInteger)

    return UnsignedInteger()


def _GetCommonPrimitiveType(left, right):
    if isinstance(left, ScalarType) and isinstance(right, ScalarType):
        return _GetCommonScalarType(left, right)
    elif isinstance(left, VectorType) and isinstance(right, VectorType):
        assert left.GetSize() == right.GetSize()
        assert isinstance(left, VectorType)
        assert isinstance(right, VectorType)
        return VectorType(
            _GetCommonScalarType(
                left.GetComponentType(), right.GetComponentType()
            ),
            left.GetComponentCount(),
        )
    elif isinstance(left, MatrixType) and isinstance(right, MatrixType):
        assert left.GetSize() == right.GetSize()
        return MatrixType(
            _GetCommonScalarType(
                left.GetComponentType(), right.GetComponentType()
            ),
            left.GetRowCount(),
            left.GetColumnCount(),
        )


def _GetRowsColumns(primitiveType: PrimitiveType):
    assert isinstance(primitiveType, PrimitiveType)
    if primitiveType.IsMatrix():
        assert isinstance(primitiveType, MatrixType)
        return primitiveType.GetSize()
    elif primitiveType.IsVector():
        assert isinstance(primitiveType, VectorType)
        return primitiveType.GetSize()[0], 1
    elif primitiveType.IsScalar():
        return 1, 1


def ResolveType(theType: Type, scope: "Scope"):
    if theType.IsArray() and theType.GetComponentType().NeedsResolve():
        resolvedComponentType = scope.GetType(
            theType.GetComponentType().GetName()
        )
        assert not isinstance(resolvedComponentType, UnresolvedType)
        assert isinstance(theType, ArrayType)
        result = theType.WithComponentType(resolvedComponentType)
        return result
    elif theType.NeedsResolve():
        result = scope.GetType(theType.GetName())
        assert not isinstance(result, UnresolvedType)
        return result
    else:
        return theType


def ResolveFunction(theType: Type, scope, argumentTypes):
    if theType.NeedsResolve():
        return scope.FindFunction(theType.GetName(), argumentTypes)
    else:
        return theType


def ResolveBinaryExpressionType(
    operation: op.Operation, left: Type, right: Type
):
    """Get the type of an expression combining two elements,
    one of type left and one of type right. This performs the standard
    type promotion rules (``int->float``, ``int->uint``) and expects that both
    types are compatible to start with.

    :param operation: The operation, must be a binary operation
    :param left: The left operand type
    :param right: The right operand type"""
    # must be primitive type, we don't support operations on
    # aggregate types
    assert isinstance(left, PrimitiveType), "{} is not a primitive type".format(
        left
    )
    assert isinstance(
        right, PrimitiveType
    ), "{} is not a primitive type".format(right)
    assert isinstance(operation, op.Operation)

    if op.IsComparison(operation):
        # Cast may be still necessary if we compare integers with floats
        baseType = _GetCommonPrimitiveType(left, right)

        if left.IsVector() and right.IsVector():
            assert isinstance(left, VectorType)
            assert isinstance(right, VectorType)
            assert left.GetComponentCount() == right.GetComponentCount()
            return ExpressionType(
                VectorType(Integer(), left.GetComponentCount()),
                [baseType, baseType],
            )
        return ExpressionType(Integer(), [baseType, baseType])

    # Multiply and divide have special rules -- matrices, vectors and scalars
    # can participate in those
    leftRightIsScalar = left.IsScalar() and right.IsScalar()
    if (
        operation in {op.Operation.MUL, op.Operation.DIV}
        and not leftRightIsScalar
    ):
        baseType = _GetCommonPrimitiveType(
            left.GetComponentType(), right.GetComponentType()
        )

        if operation == op.Operation.DIV:
            # Left can have any shape, right must be scalar
            if not right.IsScalar():
                Errors.ERROR_INVALID_BINARY_EXPRESSION_OPERATION.Raise(
                    operation, left, right
                )

            # In this branch, right and left cannot be scalar simultaneously
            assert not left.IsScalar()
            assert isinstance(left, VectorType) or isinstance(left, MatrixType)
            resultType = left.WithComponentType(baseType)
            return ExpressionType(resultType, [resultType, baseType])

        # At this point, it must be a MUL
        assert operation == op.Operation.MUL

        # Only one of both can be scalar
        if left.IsScalar():
            assert isinstance(right, VectorType) or isinstance(
                right, MatrixType
            )
            resultType = right.WithComponentType(baseType)
            return ExpressionType(resultType, [baseType, resultType])
        elif right.IsScalar():
            assert isinstance(left, VectorType) or isinstance(left, MatrixType)
            resultType = left.WithComponentType(baseType)
            return ExpressionType(resultType, [resultType, baseType])

        leftShape = _GetRowsColumns(left)
        rightShape = _GetRowsColumns(right)

        # At this point, must be a MUL of matrix * vector or matrix * matrix
        # We must prevent vector * vector
        if leftShape[1] != rightShape[0]:
            Errors.ERROR_INVALID_BINARY_EXPRESSION_OPERATION.Raise(
                operation, left, right
            )

        # Matrix * Vector or Matrix * Matrix
        resultShape = (leftShape[0], rightShape[1])
        if resultShape[1] == 1:
            assert isinstance(baseType, ScalarType)
            assert isinstance(left, VectorType) or isinstance(left, MatrixType)
            assert isinstance(right, VectorType) or isinstance(
                right, MatrixType
            )

            return ExpressionType(
                VectorType(baseType, resultShape[0]),
                [
                    left.WithComponentType(baseType),
                    right.WithComponentType(baseType),
                ],
            )
        elif resultShape[1] > 1:
            assert isinstance(baseType, ScalarType)
            assert isinstance(left, VectorType) or isinstance(left, MatrixType)
            assert isinstance(right, VectorType) or isinstance(
                right, MatrixType
            )

            return ExpressionType(
                MatrixType(baseType, resultShape[0], resultShape[1]),
                [
                    left.WithComponentType(baseType),
                    right.WithComponentType(baseType),
                ],
            )
        else:
            Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                "Invalid binary expression"
            )

    if left == right:
        return ExpressionType(left, [left, right])

    # make sure both are of the same type class (i.e. scalar, matrix or vector)
    if left.GetKind() != right.GetKind():
        Errors.ERROR_INCOMPATIBLE_TYPES.Raise(left, right)

    baseType = _GetCommonPrimitiveType(left, right)
    return ExpressionType(baseType, [baseType, baseType])


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
    def __init__(
        self,
        operation,
        conditionType,
        trueExpressionType,
        falseExpressionType,
        resultType,
    ):
        self.operation = operation
        self.conditionType = conditionType
        self.trueExpressionType = trueExpressionType
        self.falseExpressionType = falseExpressionType
        self.resultType = resultType


def BuiltinTypeFactory(typeName):
    typeDict = {
        "float": Float(),
        "float2": VectorType(Float(), 2),
        "float3": VectorType(Float(), 3),
        "float4": VectorType(Float(), 4),
        "int": Integer(),
        "int2": VectorType(Integer(), 2),
        "int3": VectorType(Integer(), 3),
        "int4": VectorType(Integer(), 4),
        "uint": UnsignedInteger(),
        "uint2": VectorType(UnsignedInteger(), 2),
        "uint3": VectorType(UnsignedInteger(), 3),
        "uint4": VectorType(UnsignedInteger(), 4),
        "matrix3x3": MatrixType(Float(), 3, 3),
        "matrix4x4": MatrixType(Float(), 4, 4),
        "float3x3": MatrixType(Float(), 3, 3),
        "float4x4": MatrixType(Float(), 4, 4),
        "void": Void(),
    }

    return typeDict[typeName]


class Scope:
    """Handles generic symbols and functions, which allow overloading."""

    def __init__(self, parent: Optional["Scope"] = None):
        # We store everything in ordered dicts so we can use
        # this for structures/classes without further modification
        self.__symbols = collections.OrderedDict()
        self.__types = collections.OrderedDict()
        self.__parent = parent
        self.__functions = collections.OrderedDict()

        self.__registeredObjects = set()

    def GetSymbolNames(self):
        return self.__symbols.keys()

    def HasParent(self) -> bool:
        return self.__parent is not None

    def GetParent(self):
        return self.__parent

    def RegisterType(self, typename: str, typeinfo: Type):
        assert typename not in self.__types

        if typename in self.__registeredObjects:
            raise InvalidDeclaration(
                "Cannot define type '{}': A function or variable with that name already exists in the current scope.".format(
                    typename
                )
            )

        self.__types[typename] = typeinfo
        self.__registeredObjects.add(typename)

    def RegisterVariable(self, symbol: str, typeinfo: Type):
        assert isinstance(
            typeinfo, Type
        ), "Expected Type instance but got {}".format(type(typeinfo))
        assert symbol not in self.__symbols

        if symbol in self.__registeredObjects:
            raise InvalidDeclaration(
                "Cannot define variable '{}': A function or type with that name already exists in the current scope.".format(
                    symbol
                )
            )

        self.__symbols[symbol] = typeinfo
        self.__registeredObjects.add(symbol)

    def GetFieldType(self, symbol: str) -> Type:
        if symbol in self.__symbols:
            return self.__symbols[symbol]
        else:
            if self.__parent is not None:
                return self.__parent.GetFieldType(symbol)
            else:
                raise UnknownSymbolException(symbol)

    def GetType(self, typename: str) -> Type:
        if typename in self.__types:
            return self.__types[typename]
        else:
            if self.__parent is not None:
                return self.__parent.GetType(typename)
            else:
                raise UnknownTypeException(typename)

    def RegisterFunction(self, functionName, typeinfo):
        if (
            functionName in self.__registeredObjects
            and functionName in self.__symbols
        ):
            raise InvalidDeclaration(
                "Cannot define function '{}': A variable or type with that name already exists in the current scope.".format(
                    functionName
                )
            )

        if functionName not in self.__functions:
            self.__functions[functionName] = []
        self.__functions[functionName].append(typeinfo)
        self.__registeredObjects.add(functionName)

    def FindFunction(self, functionName: str, argumentTypes: List[Type]):
        """Find a function with the specified name matching the argument types.

        If the function is overloaded, this will find the best candidate."""

        # Resolve overloaded __functions
        if functionName not in self.__functions:
            if self.__parent is not None:
                return self.__parent.FindFunction(functionName, argumentTypes)
            else:
                Errors.ERROR_UNKNOWN_FUNCTION_CALL.Raise(functionName)

        def GetFirst(t):
            return t[0]

        def IsValidCandidate(candidate):
            return candidate[0] >= 0

        candidates = self.__functions[functionName]
        ranking = list(
            filter(
                IsValidCandidate,
                sorted(
                    [
                        (candidate.Match(argumentTypes), candidate)
                        for candidate in candidates
                    ],
                    key=GetFirst,
                ),
            )
        )

        if len(ranking) == 0:
            Errors.ERROR_NO_MATCHING_OVERLOAD_FUNCTION_CALL.Raise(functionName)

        if len(ranking) == 1:
            return ranking[0][1]

        # if the first two candidates have the same score, the overload is
        # ambiguous
        if ranking[0][0] == ranking[1][0]:
            Errors.ERROR_AMBIGUOUS_FUNCTION_CALL.Raise(functionName)

        return ranking[0][1]
