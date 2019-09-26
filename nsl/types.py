from collections import OrderedDict
from nsl import op, Errors
from enum import Enum
import collections

class UnknownSymbol(Exception):
	def __init__(self, symbol):
		self.__symbol = symbol

	def __str__(self):
		return "Unknown symbol: '{}'".format (self.__symbol)
	
class UnknownType(Exception):
	def __init__(self, name):
		self.__typename = name
		
	def __str__(self):
		return "Unknown type: '{}'".format (self.__typename)

class UnknownFunction(Exception):
	pass

class InvalidDeclaration(Exception):
	def __init__(self, message):
		self.message = message

class Scope:
	'''Handles generic __symbols and __functions, which allow overloading.'''
	def __init__(self, parent = None):
		# We store everything in ordered dicts so we can use
		# this for structures/classes without further modification
		self.__symbols = OrderedDict ()
		self.__types = OrderedDict ()
		self.__parent = parent
		self.__functions = OrderedDict ()

		self.__registeredObjects = set ()

	def GetSymbolNames(self):
		return self.__symbols.keys()

	def HasParent(self):
		return self.__parent is not None

	def GetParent(self):
		return self.__parent
	
	def RegisterType(self, typename, typeinfo):
		assert isinstance (typeinfo, Type)
		assert typename not in self.__types
		
		if typename in self.__registeredObjects:
			raise InvalidDeclaration ("Cannot define type '{}': A function or variable with that name already exists in the current scope.".format (typename))
		
		self.__types [typename] = typeinfo
		self.__registeredObjects.add (typename)


	def RegisterVariable(self, symbol, typeinfo):
		assert isinstance (typeinfo, Type), 'Expected Type instance but got {}'.format (type(typeinfo))
		assert symbol not in self.__symbols

		if symbol in self.__registeredObjects:
			raise InvalidDeclaration ("Cannot define variable '{}': A function or type with that name already exists in the current scope.".format (symbol))

		self.__symbols [symbol] = typeinfo
		self.__registeredObjects.add (symbol)

	def GetFieldType (self, symbol):
		if symbol in self.__symbols:
			return self.__symbols [symbol]
		else:
			if self.__parent is not None:
				return self.__parent.GetFieldType (symbol)
			else:
				raise UnknownSymbol(symbol)

	def GetType (self, typename):
		if typename in self.__types:
			return self.__types [typename]
		else:
			if self.__parent is not None:
				return self.__parent.GetType (typename)
			else:
				raise UnknownType(typename)

	def RegisterFunction(self, functionName, typeinfo):
		if functionName in self.__registeredObjects and functionName in self.__symbols:
			raise InvalidDeclaration ("Cannot define function '{}': A variable or type with that name already exists in the current scope.".format (functionName))

		if not functionName in self.__functions:
			self.__functions [functionName] = []
		self.__functions [functionName].append (typeinfo)
		self.__registeredObjects.add (functionName)

	def GetMethodType(self, functionName, argumentTypes):
		'''Get a matching function.

		@param argumentTypes: The type of each function parameter.'''

		# Resolve overloaded __functions
		if not functionName in self.__functions:
			if not self.__parent is None:
				return self.__parent.GetMethodType(functionName,
												   argumentTypes)
			else:
				Errors.ERROR_UNKNOWN_FUNCTION_CALL.Raise (functionName)

		def GetFirst(t):
			return t[0]

		def IsValidCandidate(candidate):
			return candidate[0] >= 0

		candidates = self.__functions [functionName]
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
		raise Exception()

	def IsPrimitive(self):
		return False

	def IsAggregate(self):
		return False

	def IsArray(self):
		return False

	def NeedsResolve (self):
		return False

	def GetElementType(self):
		'''For vector, matrix and array types, return the element type.'''
		return self

def ResolveType(theType, scope):
	if theType.NeedsResolve ():
		result = scope.GetType (theType.GetName ())
		assert not isinstance (result, UnresolvedType)
		return result
	else:
		return theType

def ResolveFunction(theType, scope, argumentTypes):
	if theType.NeedsResolve ():
		return scope.GetMethodType (theType.GetName (), argumentTypes)
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
		if left.IsVector () and left.GetSize () == (1,):
				left = left.GetElementType ()
		if right.IsVector () and right.GetSize () == (1,):
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
	assert isinstance(leftType, Type)
	assert isinstance(rightType, Type)
	
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

def _GetCommonScalarType(left, right):
	'''Given two scalar types, get a common scalar type.'''
	assert isinstance (left, ScalarType)
	assert isinstance (right, ScalarType)

	if isinstance (left, Float) or isinstance (right, Float):
		return Float ()

	if isinstance (left, Integer) or isinstance (right, Integer):
		return Integer ()

	assert isinstance (left, UnsignedInteger)
	assert isinstance (right, UnsignedInteger)

	return UnsignedInteger ()

def _GetCommonPrimitiveType(left, right):
	if isinstance(left, ScalarType) and isinstance(right, ScalarType):
		return _GetCommonScalarType(left, right)
	elif isinstance (left, VectorType) and isinstance(right, VectorType):
		assert (left.GetSize () == right.GetSize ())
		return VectorType (
			_GetCommonScalarType (
				left.GetElementType (),
				right.GetElementType()),
			left.GetSize ())
	elif isinstance (left, MatrixType) and isinstance (right, MatrixType):
		assert (left.GetSize () == right.GetSize ())
		return MatrixType (
			_GetCommonScalarType (
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
		baseType = _GetCommonPrimitiveType (left, right)
		return ExpressionType (Integer (), [baseType, baseType])

	if operation in {op.Operation.MUL, op.Operation.DIV} and (left.IsMatrix () or left.IsVector ()):
		if right.IsScalar ():
			baseType = _GetCommonScalarType (left.GetElementType (), right)
			return ExpressionType (left.WithComponentType (baseType),
				[
					left.WithComponentType (baseType),
					right
				])
			
		if operation == op.Operation.DIV and (right.IsMatrix () or right.IsScalar ()):
			Errors.ERROR_INVALID_BINARY_EXPRESSION_OPERATION.Raise (operation, left, right)

		leftSize = left.GetSize ()
		rightSize = right.GetSize ()

		if len(leftSize) == 1:
			leftSize = (leftSize[0], 1)

		if len(rightSize) == 1:
			rightSize = (rightSize[0], 1)

		# Shape must be as follows
		# Left side is a NxM matrix
		# Right side must be a MxN matrix
		# Result is a MxM matrix

		if leftSize[1] != rightSize [0]:
			Errors.ERROR_INCOMPATIBLE_TYPES.Raise (left, right)

		baseType = _GetCommonScalarType(left.GetElementType (), right.GetElementType ())

		if right.IsVector ():
			return ExpressionType (VectorType (baseType, left.GetRowCount ()),
				[
					left.WithComponentType (baseType),
					right.WithComponentType (baseType)
				])
		else:
			return ExpressionType (MatrixType (baseType, leftSize [1], rightSize [0]),
				[
					left.WithComponentType (baseType),
					right.WithComponentType (baseType)
				])

	if left == right:
		return ExpressionType (left, [left, right])

	# make sure both are of the same type class (i.e. scalar, matrix or vector)
	if left.GetKind () != right.GetKind ():
		Errors.ERROR_INCOMPATIBLE_TYPES.Raise (left, right)

	baseType = _GetCommonPrimitiveType(left, right)
	return ExpressionType (baseType, [baseType, baseType])

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
		assert isinstance(arraySize, collections.abc.Sequence)
		assert len(arraySize) > 0
		for l in arraySize:
			assert l > 0
		self.__elementType = elementType
		self.__arraySize = tuple (arraySize)

	def IsArray(self):
		return True

	def GetSize(self):
		return self.__arraySize

	def GetElementType(self):
		return self.__elementType

	def GetName(self):
		return self.GetElementType().GetName () + ' ' + ''.join(['[{}]'.format(s) for s in self.__arraySize])

	def __str__(self):
		return '{}{}'.format (self.__elementType, ''.join(['[{}]'.format(s) for s in self.__arraySize]))

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

	def GetFieldType(self, variableName):
		return self._members.GetFieldType (variableName)

class ClassType(StructType):
	'''Struct type with additional support for member __functions.'''
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

	def GetMethodType(self, functionName, argumentTypes):
		return self._members.GetMethodType(functionName, argumentTypes)

	def IsInterface(self):
		return self.__isInterface

class Function(Type):
	def __init__(self, name, returnType, arguments):
		self.returnType = returnType
		self.arguments = arguments
		self.name = name

	def Resolve(self, scope):
		self.returnType = ResolveType(self.returnType, scope)
		self.__argumentTypes = OrderedDict ()
		for counter, arg in enumerate (self.arguments):
			if arg.HasName ():
				self.__argumentTypes [arg.GetName ()] = ResolveType (arg.GetType (), scope)
			else:
				# generate an invalid name
				self.__argumentTypes ['$unnamed_arg${}'.format (counter)] = ResolveType (arg.GetType (), scope)

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
		return (self.__componentCount,)

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

	def WithComponentType(self, componentType):
		'''Return a copy of this type with a new component type.'''
		return VectorType(componentType, self.__componentCount)

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

	def WithComponentType(self, componentType):
		'''Return a copy of this type with a new component type.'''
		return MatrixType(componentType, self.__size [0], self.__size [1], self.__order)

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
