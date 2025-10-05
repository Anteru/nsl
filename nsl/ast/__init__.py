import collections
import collections.abc
from nsl import op, types, Visitor
from enum import Enum
import bisect
from typing import List


class SourceMapping:
    def __init__(self, source, sourceName="<unknown>"):
        self.__sourceName = sourceName

        self.__lineOffsets = []
        currentOffset = 0
        for line in source.split("\n"):
            self.__lineOffsets.append(currentOffset)
            currentOffset += len(line) + 1  # trailing \n

    def GetLineFromOffset(self, offset):
        return bisect.bisect_right(self.__lineOffsets, offset) - 1

    def GetLineStartOffset(self, line):
        return self.__lineOffsets[line]

    def GetSourceName(self):
        return self.__sourceName


class Location:
    def __init__(self, span, sourceMapping=None):
        assert span[1] >= span[0]
        self.__span = span
        self.__sourceMapping = sourceMapping

    @classmethod
    def Merge(cls, *args):
        assert len(args) > 0

        result = args[0].__span
        mapping = args[0].__sourceMapping
        for arg in args[1:]:
            assert isinstance(arg, Location)
            end = max(result[1], arg.GetEnd())
            start = min(result[0], arg.GetBegin())
            result = (
                start,
                end,
            )
        return cls(result, mapping)

    def GetBegin(self):
        return self.__span[0]

    def GetEnd(self):
        return self.__span[1]

    @property
    def IsUnknown(self):
        return self.__span == (-1, -1)

    def __str__(self):
        if self.IsUnknown:
            return "<unknown>"

        if self.__sourceMapping:
            # Lines are 0 based (as are columns), and we need to offset
            # with +1 for display
            startLine = self.__sourceMapping.GetLineFromOffset(self.GetBegin())
            endLine = self.__sourceMapping.GetLineFromOffset(self.GetEnd())

            if startLine == endLine:
                startOffset = self.__sourceMapping.GetLineStartOffset(startLine)
                return "{}:{}-{}".format(
                    startLine + 1,
                    self.GetBegin() - startOffset + 1,
                    self.GetEnd() - startOffset + 1,
                )
            else:
                startOffset = self.__sourceMapping.GetLineStartOffset(startLine)
                endOffset = self.__sourceMapping.GetLineStartOffset(endLine)
                return "{}:{}-{}:{}".format(
                    startLine + 1,
                    self.GetBegin() - startOffset + 1,
                    endLine + 1,
                    self.GetEnd() - endOffset + 1,
                )
        else:
            return "[{},{})".format(self.GetBegin(), self.GetEnd())

    def __repr__(self):
        return "Location({})".format(repr(self.__span))


class Node(Visitor.Node):
    def __init__(self):
        self.__location = Location((-1, -1))

    def Clone(self):
        import copy

        return copy.deepcopy(self)

    def SetLocation(self, location):
        assert isinstance(location, Location)
        self.__location = location

    def GetLocation(self):
        return self.__location


class Module(Node):
    """A single translation module.

    A module consists of types, variables and functions."""

    def __init__(self):
        super().__init__()
        self.__variables = list()
        self.__functions = list()
        # Types may depend on types which are previously defined
        # Ensure ordering by using an ordered dict
        self.__types = collections.OrderedDict()
        self.__imports = set()

    def _Traverse(self, function):
        self.__types = function(self.__types)
        self.__variables = function(self.__variables)
        self.__functions = function(self.__functions)

    def AddDeclaration(self, variable):
        self.__variables.append(variable)

    def AddFunction(self, func):
        self.__functions.append(func)

    def AddType(self, decl):
        self.__types[decl.GetName()] = decl

    def AddImport(self, name):
        self.__imports.add((name))

    def GetDeclarations(self):
        return self.__variables

    def GetTypes(self):
        return self.__types.values()

    def GetFunctions(self):
        return self.__functions

    def GetImports(self):
        return self.__imports

    def __str__(self):
        return (
            """Module ({0} variable(s), {1} function(s), {2} type(s))""".format(
                len(self.__variables), len(self.__functions), len(self.__types)
            )
        )


class Expression(Node):
    def __init__(self, children=[]):
        super().__init__()
        self.children = children
        self.__type: types.Type | None = None

    def GetType(self):
        return self.__type

    def SetType(self, nslType):
        """The type of this expression. This depends on the specific expression type,
        for instance, for a call expression this will be a function type, while for
        an unary expression it will be a primitive or structure type."""
        assert not isinstance(nslType, types.UnresolvedType)
        self.__type = nslType

    def _Traverse(self, function):
        self.children = function(self.children)

    def __iter__(self):
        return self.children.__iter__()


class UnaryExpression(Expression):
    pass


class EmptyExpression(Expression):
    def __init__(self):
        super().__init__()


class CastExpression(UnaryExpression):
    def __init__(self, expr, targetType, implicit=False):
        super().__init__([expr])
        assert isinstance(targetType, types.PrimitiveType)
        self.SetType(targetType)
        self.__implicit = implicit

    def IsImplicit(self):
        return self.__implicit

    def GetArgument(self):
        return self.children[0]

    def __str__(self):
        return "{} ({})".format(self.GetType(), self.GetArgument())

    def __repr__(self):
        return "CastExpression ({}, {}, {})".format(
            repr(self.GetArgument()), repr(self.GetType()), self.IsImplicit()
        )


class ConstructPrimitiveExpression(UnaryExpression):
    """Expression of the type primitive_type (expr, ...)."""

    def __init__(self, targetType: types.PrimitiveType, expressions):
        super().__init__(expressions)
        assert isinstance(targetType, types.PrimitiveType)
        self.SetType(targetType)

    def __str__(self):
        return "{} ({})".format(
            self.GetType().GetName(),
            ", ".join([str(expr) for expr in self.children]),
        )

    def GetArguments(self):
        return self.children

    def SetArguments(self, args):
        self.children = args


class CallExpression(UnaryExpression):
    """A function call of the form ID ([expr], ...). ID references
    an unresolved function type at first."""

    def __init__(self, function: types.Function, expressions: List[Expression]):
        super().__init__(expressions)
        self.function = function

    def __str__(self):
        r = self.function.GetName() + " ("
        r += ", ".join(["{0}".format(str(expr)) for expr in self.children])
        return r + ")"

    def GetArguments(self):
        return self.children

    def SetArguments(self, arguments: List[Expression]):
        self.children = arguments

    def GetFunction(self) -> types.Function:
        return self.function

    def ResolveType(self, scope):
        self.function = types.ResolveFunction(
            self.function,
            scope,
            [expr.GetType() for expr in self.GetArguments()],
        )

        assert isinstance(self.function, types.Function)


class VariableAccessExpression(UnaryExpression):
    pass


class ArrayExpression(VariableAccessExpression):
    """Expression of the form 'id[expr]', where id can be a nested
    access expression itself."""

    def __init__(self, identifier, expression):
        super().__init__()
        self.id = identifier
        self._expression = expression

    def GetParent(self):
        return self.id

    def GetExpression(self):
        return self._expression

    def SetExpression(self, expr):
        self._expression = expr

    def _Traverse(self, function):
        self.id = function(self.id)
        self._expression = function(self._expression)

    def __str__(self):
        return str(self.id) + " [" + str(self._expression) + "]"


class MemberAccessExpression(VariableAccessExpression):
    """Expression of the form 'id.member', where id can be a
    access nested expression itself.

    A member access expression can be a swizzle. If so, ``isSwizzle`` should be
    set to ``True``."""

    def __init__(self, identifier, member):
        super().__init__()
        self.id = identifier
        self.member = member
        self.isSwizzle = False

    def GetMember(self):
        return self.member

    def GetParent(self):
        return self.id

    def _Traverse(self, function):
        self.id = function(self.id)
        self.member = function(self.member)

    def SetSwizzle(self, isSwizzle: bool) -> None:
        self.isSwizzle = isSwizzle

    def __str__(self):
        return str(self.id) + "." + str(self.member)


class BinaryExpression(Expression):
    def __init__(self, op, left, right):
        super().__init__([left, right])
        self.op = op
        self._operator: types.ExpressionType | None = None

    def GetLeft(self):
        return self.children[0]

    def GetRight(self):
        return self.children[1]

    def SetLeft(self, left):
        self.children[0] = left

    def SetRight(self, right):
        self.children[1] = right

    def GetOperation(self):
        """The the operation."""
        return self.op

    def GetOperator(self):
        """Get the used operator. This is an instance of ExpressionType."""
        return self._operator

    def ResolveType(self, left, right):
        self._operator = types.ResolveBinaryExpressionType(self.op, left, right)

    def __str__(self):
        r = ""
        if isinstance(self.GetLeft(), BinaryExpression):
            r += "(" + str(self.GetLeft()) + ")"
        else:
            r += str(self.GetLeft())

        r += " " + op.OpToStr(self.op) + " "

        if isinstance(self.GetRight(), BinaryExpression):
            r += "(" + str(self.GetRight()) + ")"
        else:
            r += str(self.GetRight())

        return r


class AssignmentExpression(BinaryExpression):
    def __init__(self, left, right, *, operation=op.Operation.ASSIGN):
        super().__init__(operation, left, right)

    def ResolveType(self, left, right):
        self._operator = types.ExpressionType(
            self.GetLeft().GetType(),
            [self.GetLeft().GetType(), self.GetRight().GetType()],
        )


class Affix:
    PRE = 1
    POST = 2


class AffixExpression(UnaryExpression):
    def __init__(self, op, expr, affix):
        super().__init__([expr])
        self.op = op
        self.affix = affix

    def IsPostfix(self):
        return self.affix == Affix.POST

    def IsPrefix(self):
        return self.affix == Affix.PRE

    def GetOperation(self):
        return self.op

    def GetExpression(self):
        return self.children[0]

    def __str__(self):
        if self.affix == Affix.PRE:
            if self.op == op.Operation.ADD:
                return f"++{self.children[0]}"
            elif self.op == op.Operation.SUB:
                return f"--{self.children[0]}"
        elif self.affix == Affix.POST:
            if self.op == op.Operation.ADD:
                return f"{self.children[0]}++"
            elif self.op == op.Operation.SUB:
                return f"{self.children[0]}--"


class LiteralExpression(UnaryExpression):
    def __init__(self, value, literalType):
        super().__init__()
        self.value = value
        self.SetType(literalType)

    def GetValue(self):
        return self.value

    def __str__(self):
        return str(self.value)


class PrimaryExpression(UnaryExpression):
    def __init__(self, identifier):
        super().__init__()
        self.identifier = identifier

    def GetName(self):
        return self.identifier

    def __str__(self):
        return self.identifier


class InvalidStructureDefinitionException(Exception):
    def __init__(self, structName: str, memberName: str):
        self.structName = structName
        self.memberName = memberName


class StructureDefinition(Node):
    def __init__(self, name, fields=list()):
        super().__init__()
        self.__name = name
        self.__fields = fields
        self.__type = types.UnresolvedType(name)

        # Check that all element names are unique
        fieldNames = set()
        for field in fields:
            if field.GetName() in fieldNames:
                raise InvalidStructureDefinitionException(name, field.GetName())
            fieldNames.add(field.GetName())

        self.__annotations = []

    def _Traverse(self, function):
        self.__fields = function(self.__fields)

    def AddAnnotation(self, annotation):
        assert isinstance(annotation, Annotation)
        self.__annotations.append(annotation)

    def GetAnnotations(self):
        return self.__annotations

    def GetName(self):
        return self.__name

    def __str__(self):
        return "struct {0} ({1} field(s))".format(
            self.GetName(), len(self.GetFields())
        )

    def GetFields(self):
        return self.__fields

    def SetType(self, structType):
        assert isinstance(structType, types.StructType)
        self.__type = structType

    def GetType(self):
        return self.__type


class InterfaceDefinition(Node):
    def __init__(self, name, methods=list()):
        super().__init__()
        self.__name = name
        self.__methods = methods

        self.__type = types.UnresolvedType(name)

    def _Traverse(self, function):
        self.__methods = function(self.__methods)

    def GetMethods(self):
        return self.__methods

    def GetName(self):
        return self.__name

    def SetType(self, interfaceType):
        assert isinstance(interfaceType, types.ClassType)
        self.__type = interfaceType

    def GetType(self):
        return self.__type


class VariableDeclaration(Node):
    def __init__(self, variableType, symbol, initExpression=None):
        super().__init__()
        self.__symbol = symbol
        self.__initializer = initExpression

        self.__type = variableType

    def ResolveType(self, scope):
        self.__type = types.ResolveType(self.__type, scope)

        return self.__type

    def _Traverse(self, function):
        self.__initializer = function(self.__initializer)

    def __str__(self):
        if not self.__type.NeedsResolve():
            if self.__type.IsArray():
                result = (
                    str(self.__type.GetComponentType())
                    + " "
                    + str(self.GetName())
                    + "["
                    + ", ".join(map(str, self.__type.GetSize()))
                    + "]"
                )
            else:
                result = str(self.__type) + " " + str(self.GetName())
        else:
            result = self.GetName()

        if self.HasInitializerExpression():
            result += "= " + str(self.__initializer)

        return result

    def GetType(self):
        return self.__type

    def GetName(self):
        return self.__symbol

    def HasInitializerExpression(self):
        return self.__initializer is not None

    def GetInitializerExpression(self):
        return self.__initializer


class ArgumentModifier(Enum):
    Optional = 1


class Argument(Node):
    """Function argument. Captures the type (potentially a Type or
    UnresolvedType) and the name of the argument."""

    def __init__(self, argumentType, name=None, modifiers=set()):
        super().__init__()
        self.__type = argumentType
        self.__name = name
        self.__modifiers = modifiers

    def ResolveType(self, scope):
        self.__type = types.ResolveType(self.__type, scope)
        return self.__type

    def GetType(self):
        return self.__type

    def GetName(self):
        return self.__name

    def HasName(self):
        return self.__name is not None

    def GetModifiers(self):
        return self.__modifiers

    def IsOptional(self):
        return ArgumentModifier.Optional in self.__modifiers

    def __str__(self):
        if self.__name is not None:
            return "{} {}".format(self.__type.GetName(), self.__name)
        else:
            return "{} <unnamed>".format(self.__type.GetName())


class Function(Node):
    def __init__(
        self,
        name,
        arguments=list(),
        returnType=types.Void(),
        body=None,
        *,
        isForwardDeclaration=False,
        isExported=False,
    ):
        super().__init__()
        self.name = name
        self.__body = body
        self.__type = types.Function(name, returnType, arguments, isExported)
        self.arguments = arguments
        self.isForwardDeclaration = isForwardDeclaration
        self.isExported = isExported

    def ResolveType(self, scope):
        for arg in self.arguments:
            arg.ResolveType(scope)

    def _Traverse(self, function):
        self.arguments = function(self.arguments)
        if not self.isForwardDeclaration:
            self.__body = function(self.__body)

    def GetName(self):
        return self.name

    def GetType(self):
        return self.__type

    def GetArguments(self):
        return self.arguments

    def GetBody(self):
        return self.__body

    def __str__(self):
        return "{} ({} argument(s))".format(
            self.GetName(), len(self.GetArguments())
        )


class Statement(Node):
    pass


class BranchControl(Enum):
    Default = 0
    Branch = 1


class FlowStatement(Statement):
    pass


class EmptyStatement(Statement):
    pass


class ExpressionStatement(Statement):
    def __init__(self, expr):
        super().__init__()
        self.__expression = expr

    def _Traverse(self, function):
        self.__expression = function(self.__expression)

    def GetExpression(self):
        return self.__expression

    def __str__(self):
        return "Expression"


class CompoundStatement(Statement):
    """Compound statement consisting of zero or more statements.
    Compound statements also create a new visibility block."""

    def __init__(self, stmts):
        super().__init__()
        self.__statements = stmts

    def GetStatements(self):
        return self.__statements

    def SetStatements(self, statements):
        self.__statements = statements

    def _Traverse(self, function):
        self.__statements = function(self.__statements)

    def __len__(self):
        return len(self.__statements)

    def __iter__(self):
        """Iterate over the statements."""
        return self.__statements.__iter__()

    def __str__(self):
        return "{0} statement(s)".format(len(self))


class ReturnStatement(FlowStatement):
    def __init__(self, expression=None):
        super().__init__()
        self.__expression = expression

    def _Traverse(self, function):
        if self.__expression:
            self.__expression = function(self.__expression)

    def GetExpression(self):
        return self.__expression

    def __str__(self):
        if self.__expression:
            return "return " + str(self.__expression)
        else:
            return "return"


class DeclarationStatement(Statement):
    def __init__(self, variableDeclarations):
        super().__init__()
        self.declarations = variableDeclarations

    def GetDeclarations(self):
        return self.declarations

    def _Traverse(self, function):
        self.declarations = function(self.declarations)

    def __str__(self):
        return "{0} declaration(s)".format(len(self.declarations))


class IfStatement(FlowStatement):
    def __init__(
        self,
        cond,
        true_path,
        else_path=None,
        *,
        branch_control=BranchControl.Default,
    ):
        super().__init__()
        self.__condition = cond
        self.__trueBlock = true_path
        self.__elseBlock = else_path
        self.__branchControl = branch_control

    def _Traverse(self, function):
        self.__condition = function(self.__condition)
        self.__trueBlock = function(self.__trueBlock)
        self.__elseBlock = function(self.__elseBlock)

    def GetCondition(self):
        return self.__condition

    def GetTruePath(self):
        return self.__trueBlock

    def GetElsePath(self):
        return self.__elseBlock

    def HasElsePath(self):
        return self.__elseBlock is not None

    def __str__(self):
        return str(self.__condition)


class ContinueStatement(FlowStatement):
    def __init__(self):
        super().__init__()


class BreakStatement(FlowStatement):
    def __init__(self):
        super().__init__()


class ForStatement(FlowStatement):
    def __init__(self, init, cond, increment, body):
        super().__init__()
        self.__initializer = init
        self.__condition = cond
        self.__next = increment
        self.__body = body

    def GetBody(self):
        return self.__body

    def GetInitialization(self):
        return self.__initializer

    def GetCondition(self):
        return self.__condition

    def GetNext(self):
        return self.__next

    def _Traverse(self, function):
        self.__initializer = function(self.__initializer)
        self.__condition = function(self.__condition)
        self.__next = function(self.__next)
        self.__body = function(self.__body)

    def __str__(self):
        return "ForStatement"


class DoStatement(FlowStatement):
    def __init__(self, cond, body):
        super().__init__()
        self.__condition = cond
        self.__body = body

    def _Traverse(self, function):
        self.__body = function(self.__body)
        self.__condition = function(self.__condition)

    def GetCondition(self):
        return self.__condition

    def GetBody(self):
        return self.__body


class WhileStatement(FlowStatement):
    def __init__(self, cond, body):
        super().__init__()
        self.__condition = cond
        self.__body = body

    def _Traverse(self, function):
        self.__body = function(self.__body)
        self.__condition = function(self.__condition)

    def GetCondition(self):
        return self.__condition

    def GetBody(self):
        return self.__body


class Annotation(Node):
    def __init__(self, value):
        super().__init__()
        self.__value = value

    def GetValue(self):
        return self.__value

    def __str__(self):
        return "[{}]".format(self.__value)

    def __repr__(self):
        return "Annotation({})".format(repr(self.__value))
