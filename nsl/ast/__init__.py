import collections.abc
from nsl import op, types
from enum import Enum
import bisect

class InvalidChildType(Exception):
    def __init__(self, actualType):
        self.actualType = actualType

class SourceMapping:
    def __init__(self, source, sourceName = '<unknown>'):
        self.__sourceName = sourceName
        
        self.__lineOffsets = []
        currentOffset = 0
        for line in source.split ('\n'):
            self.__lineOffsets.append(currentOffset)
            currentOffset += len (line) + 1 # trailing \n
            
    def GetLineFromOffset(self, offset):
        return bisect.bisect_right(self.__lineOffsets, offset) - 1
    
    def GetLineStartOffset(self, line):
        return self.__lineOffsets[line]
    
    def GetSourceName(self):
        return self.__sourceName

class Location:
    def __init__(self, span, sourceMapping = None):
        assert span[1] >= span [0]
        self.__span = span
        self.__sourceMapping = sourceMapping
        
    @classmethod
    def Merge (*args):
        assert len(args) > 1
        
        result = args[1]
        for arg in args [2:]:
            assert isinstance (arg, Location)
            end = max (result.GetEnd (), arg.GetEnd ())
            start = min (result.GetBegin (), arg.GetBegin ())
            result.__span = (start, end)
        return result

    def GetBegin(self):
        return self.__span [0]
    
    def GetEnd(self):
        return self.__span [1]
    
    def __str__(self):
        if self.__sourceMapping:
            # Lines are 0 based (as are columns), and we need to offset
            # with +1 for display
            startLine = self.__sourceMapping.GetLineFromOffset (self.GetBegin ())
            endLine = self.__sourceMapping.GetLineFromOffset (self.GetEnd ())
            
            if startLine == endLine:
                startOffset = self.__sourceMapping.GetLineStartOffset(startLine)
                return '{}:{}-{}'.format (
                    startLine + 1,
                    self.GetBegin () - startOffset + 1,
                    self.GetEnd () - startOffset + 1)
            else:
                startOffset = self.__sourceMapping.GetLineStartOffset (startLine)
                endOffset = self.__sourceMapping.GetLineStartOffset(endLine)
                return '{}:{}-{}:{}'.format (
                    startLine + 1,
                    self.GetBegin () - startOffset + 1,
                    endLine + 1,
                    self.GetEnd () - endOffset + 1)
        else:
            return '[{},{})'.format (self.GetBegin (), self.GetEnd ())
    
    def __repr__(self):
        return 'Location({})'.format (repr(self.__span))

class Node:
    def __init__(self):
        self.__location = None
    
    '''Base class for all nodes in the AST.

    The generic AST node provides a traversal function, which traverses into
    all children of the current object. Derived classes can override Traverse()
    to make the traversal more efficient.'''
    def _GetChildren (self):
        return []

    def Clone(self):
        import copy
        return copy.deepcopy(self)
    
    def SetLocation(self, location):
        assert isinstance(location, Location)
        self.__location = location
    
    def GetLocation(self):
        return self.__location

    def AcceptVisitor(self, visitor, ctx=None):
        '''Traverse all children of this node.

        By default, this calls `_GetChildren` to obtain the list of fields
        that contain child nodes, and then traverses into each item of each
        field.'''
        fields = self._GetChildren()
        for e in fields:
            if e is None:
                continue
            if isinstance(e, collections.abc.Sequence) or isinstance(e, collections.abc.Set):
                for c in e:
                    if not isinstance (c, Node):
                        raise InvalidChildType (type(c))
                    visitor.v_Generic (c, ctx)
            elif isinstance(e, collections.abc.Mapping):
                for c in e.values ():
                    if not isinstance (c, Node):
                        raise InvalidChildType (type(c))
                    visitor.v_Generic (c, ctx)
            else:
                if not isinstance (e, Node):
                    raise InvalidChildType (type(e))
                
                visitor.v_Generic (e, ctx)
                    
class Program (Node):
    '''Program container, keeping everything together.'''
    def __init__(self):
        super().__init__()
        self.__variables = list ()
        self.__functions = list ()
        # Types may depend on types which are previously defined
        # Ensure ordering by using an ordered dict
        self.__types = collections.OrderedDict ()

    def _GetChildren (self):
        return [self.__types, self.__variables, self.__functions]

    def AddDeclaration (self, variable):
        self.__variables.append (variable)

    def GetDeclarations (self):
        return self.__variables

    def GetTypes(self):
        return self.__types.values ()

    def GetFunctions(self):
        return self.__functions
    
    def AddFunction(self, func):
        self.__functions.append (func)

    def AddType (self, decl):
        self.__types [decl.GetName ()] = decl

    def __str__(self):
        return '''Program ({0} variable(s), {1} function(s), {2} type(s))'''.format(
            len(self.__variables), len(self.__functions), len (self.__types))

class Expression(Node):
    def __init__(self, children=[]):
        super().__init__()
        self.children = children
        self.__type = None
        
    def GetType(self):
        return self.__type
    
    def SetType(self, nslType):
        '''The type of this expression. This depends on the specific expression type,
        for instance, for a call expression this will be a function type, while for
        an unary expression it will be a primitive or structure type.'''
        assert not isinstance(nslType, types.UnresolvedType)
        self.__type = nslType

    def _GetChildren(self):
        return [self.children]

    def __iter__(self):
        return self.children.__iter__()

class UnaryExpression(Expression):
    pass

class EmptyExpression(Expression):
    def __init__(self):
        super().__init__()

class CastExpression(UnaryExpression):
    def __init__(self, expr, targetType, implicit = False):
        super().__init__([expr])
        assert isinstance (targetType, types.PrimitiveType)
        self.SetType (targetType)
        self.__implicit = implicit
    
    def IsImplicit(self):
        return self.__implicit
    
    def GetArgument(self):
        return self.children[0]

    def __str__(self):
        return '{} ({})'.format (self.GetType(), self.GetArgument())
        
    def __repr__(self):
        return 'CastExpression ({}, {}, {})'.format (
            repr(self.GetArgument()), 
            repr (self.GetType ()), self.IsImplicit ())

class ConstructPrimitiveExpression(UnaryExpression):
    '''Expression of the type primitive_type (expr, ...).'''
    def __init__(self, targetType, expressions):
        super().__init__(expressions)
        assert isinstance (targetType, types.PrimitiveType)
        self.SetType (targetType)

    def __str__(self):
        return '{} ({})'.format (self.GetType ().GetName (),
            ', '.join ([str(expr) for expr in self.children]))

    def GetArguments(self):
        return self.children

    def SetArguments(self, args):            
        self.children = args

class CallExpression(UnaryExpression):
    '''A function call of the form ID ([expr], ...). ID references
    an unresolved function type at first.'''
    def __init__(self, function, expressions):
        super().__init__(expressions)
        self.function = function

    def __str__(self):
        r = self.function.GetName () + ' ('
        r += ', '.join(['{0}'.format(str(expr)) for expr in self.children])
        return r + ')'

    def GetArguments(self):
        return self.children
    
    def SetArguments(self, arguments):
        self.children = arguments
    
    def GetFunction(self):
        return self.function

    def ResolveType(self, scope):
        self.function = types.ResolveFunction(self.function,
            scope, [expr.GetType() for expr in self.GetArguments ()])
        
class MethodCallExpression(CallExpression):
    '''A function call of the form ID.ID ([expr], ...). ID.ID references
    a member function of a class/interface type.'''
    def __init__(self, memberAccess, expressions):
        super().__init__(types.UnresolvedType (memberAccess.member.GetName ()), expressions)
        self.__memberAccess = memberAccess
        
    def _GetChildren(self):
        return [self.__memberAccess, self.children]
        
    def GetMemberAccess(self):
        return self.__memberAccess

    def __str__(self):
        r = str (self.__memberAccess) + ' ('
        r += ', '.join(['{0}'.format(str(expr)) for expr in self.children])
        return r + ')'

class VariableAccessExpression(UnaryExpression):
    pass

class ArrayExpression(VariableAccessExpression):
    '''Expression of the form 'id[expr]', where id can be a nested
    access expression itself.'''
    def __init__(self, identifier, expression):
        super().__init__([identifier, expression])
        self.id = identifier
        self._expression = expression

    def GetParent(self):
        return self.id
    
    def GetExpression(self):
        return self._expression

    def SetExpression(self, expr):
        self._expression = expr

    def __str__(self):
        return str(self.id) + ' [' + str(self._expression) + ']'

class MemberAccessExpression(VariableAccessExpression):
    '''Expression of the form 'id.member', where id can be a
    access nested expression itself.'''
    def __init__(self, identifier, member):
        super().__init__([identifier, member])
        self.id = identifier
        self.member = member

    def GetMember(self):
        return self.member

    def GetParent(self):
        return self.id

    def __str__(self):
        return str(self.id) + '.' + str(self.member)

class BinaryExpression(Expression):
    def __init__(self, op, left, right):
        super().__init__([left, right])
        self.op = op
        self._operator = None

    def GetLeft(self):
        return self.children [0]

    def GetRight(self):
        return self.children [1]

    def SetLeft(self, left):
        self.children [0] = left

    def SetRight(self, right):
        self.children [1] = right

    def GetOperation(self):
        '''The the operation.'''
        return self.op
    
    def GetOperator(self):
        '''Get the used operator. This is an instance of ExpressionType.'''
        return self._operator

    def ResolveType (self, left, right):
        self._operator = types.ResolveBinaryExpressionType (self.op, left, right)

    def __str__(self):
        r = ''
        if (isinstance (self.GetLeft (), BinaryExpression)):
            r += '(' + str (self.GetLeft ()) + ')'
        else:
            r += str (self.GetLeft ())

        r += ' ' + op.OpToStr(self.op) +  ' '

        if (isinstance (self.GetRight (), BinaryExpression)):
            r += '(' + str (self.GetRight ()) + ')'
        else:
            r += str (self.GetRight ())

        return r

class AssignmentExpression(BinaryExpression):
    def __init__(self, left, right):
        super().__init__(op.Operation.ASSIGN, left, right)
        
    def ResolveType(self, left, right):        
        self._operator = types.ExpressionType (self.GetLeft().GetType (),
                                              [self.GetLeft ().GetType(),
                                               self.GetRight ().GetType ()])

class Affix:
    PRE = 1
    POST = 2

class AffixExpression(UnaryExpression):
    def __init__(self, op, expr, affix):
        super().__init__([expr])
        self.op = op
        self.affix = affix

    def IsPostfix (self):
        return self.affix == Affix.POST

    def IsPrefix (self):
        return self.affix == Affix.PRE

    def GetOperation (self):
        return self.op

class LiteralExpression(UnaryExpression):
    def __init__(self, value, literalType):
        super().__init__()
        self.value = value
        self.SetType (literalType)

    def GetValue(self):
        return self.value

    def __str__(self):
        return str (self.value)

class PrimaryExpression(UnaryExpression):
    def __init__(self, identifier):
        super().__init__()
        self.identifier = identifier

    def GetName(self):
        return self.identifier

    def __str__(self):
        return self.identifier

class InvalidStructureDefinition(Exception):
    def __init__(self, structName, memberName):
        self.structName = structName
        self.memberName = memberName

class StructureDefinition(Node):
    def __init__(self, name, fields = list()):
        super().__init__()
        self.__name = name
        self.__fields = fields

        # Check that all element names are unique
        fieldNames = set()
        for field in fields:
            if field.GetName () in fieldNames:
                raise InvalidStructureDefinition()
            fieldNames.add (field.GetName ())

        self.__annotations = []

    def _GetChildren(self):
        return [self.__fields]

    def AddAnnotation (self, annotation):
        assert isinstance(annotation, Annotation)
        self.__annotations.append (annotation)

    def GetAnnotations(self):
        return self.__annotations

    def GetName(self):
        return self.__name

    def __str__(self):
        return 'struct {0} ({1} field(s))'.format (self.GetName (), len (self.GetFields()))

    def GetFields (self):
        return self.__fields

class InterfaceDefinition(Node):
    def __init__(self, name, methods = list ()):
        super().__init__()
        self.__name = name
        self.__methods = methods

    def _GetChildren (self):
        return [self.__methods]
    
    def GetMethods (self):
        return self.__methods
    
    def GetName (self):
        return self.__name

class BuiltinSemantic(Enum):
    Position = 1
    ColorOutput = 2

class Semantic:
    def __init__(self, semantic, slot = None):
        _semanticTable = {
            'Position' : BuiltinSemantic.Position,
            'ColorOutput' : BuiltinSemantic.ColorOutput
        }
        self.__semantic = _semanticTable [semantic]
        self.__slot = slot

    def Get(self):
        return self.__semantic

    def GetSlot (self, default = None):
        if self.__slot:
            return self.__slot
        else:
            return default
        
    def HasSlot (self):
        return self.__slot is not None

    def __str__ (self):
        if self.__slot:
            return '{}[{}]'.format (self.__semantic.name, self.__slot)
        else:
            return str (self.__semantic.name)

    def __repr__(self):
        return 'Semantic ({}, {})'.format (repr(self.__semantic), repr (self.__slot))

class VariableDeclaration(Node):
    def __init__(self, elementType, symbol,
                 semantic = None,
                 initExpression = None,
                 arraySize = None):
        super().__init__()
        self.__symbol = symbol
        self.__semantic = semantic
        self.__initializer = initExpression

        if arraySize is not None:
            self.__type = types.ArrayType (elementType, arraySize)
        else:
            self.__type = elementType

    def ResolveType(self, scope):
        self.__type = types.Resolve(self.__type, scope)

        if self.HasSemantic():
            self.__type.SetSemantic (self.__semantic)

        return self.__type

    def _GetChildren(self):
        return [self.__initializer]

    def __str__(self):
        if not self.__type.NeedsResolve ():
            if self.__type.IsArray ():
                result = str(self.__type.GetElementType ()) + ' ' + str(self.GetName ()) + '[' + ', '.join(map(str, self.__type.GetSize())) + ']'
            else:
                result = str(self.__type) + ' ' + str(self.GetName ())
        else:
            result = self.GetName ()
            
        if (self.HasInitializerExpression ()):
            result += '= ' + str(self.__initializer)

        if (self.HasSemantic()):
            result += ' : ' + str(self.__semantic)

        return result

    def GetType(self):
        return self.__type

    def GetName(self):
        return self.__symbol

    def HasSemantic(self):
        return self.__semantic is not None

    def GetSemantic(self):
        return self.__semantic

    def HasInitializerExpression(self):
        return self.__initializer is not None

    def GetInitializerExpression(self):
        return self.__initializer

class ArgumentModifier(Enum):
    Optional = 1

class Argument(Node):
    '''Function argument. Captures the type (potentially a Type or
    UnresolvedType) and the name of the argument.'''
    def __init__(self, argumentType, name = None, modifiers = set()):
        super().__init__()
        self.__type = argumentType
        self.__name = name
        self.__modifiers = modifiers

    def ResolveType(self, scope):
        self.__type = types.Resolve(self.__type, scope)
        return self.__type

    def GetType(self):
        return self.__type

    def GetName(self):
        return self.__name

    def HasName (self):
        return self.__name is not None
    
    def GetModifiers(self):
        return self.__modifiers
    
    def IsOptional(self):
        return ArgumentModifier.Optional in self.__modifiers

    def __str__(self):
        if self.__name is not None:
            return '{} {}'.format (self.__type.GetName (), self.__name)
        else:
            return '{} <unnamed>'.format (self.__type.GetName ())

class Function(Node):
    def __init__(self, name, arguments = list (),
                 returnType = types.Void (), body = None,
                 isForwardDeclaration = False):
        super().__init__()
        self.name = name
        self.__body = body
        self.__type = types.Function (name, returnType, arguments)
        self.arguments = arguments
        self.isForwardDeclaration = isForwardDeclaration

    def ResolveType(self, scope):
        for arg in self.arguments:
            arg.ResolveType (scope)

    def _GetChildren(self):
        if self.isForwardDeclaration:
            return [self.arguments]
        else:
            return [self.arguments, self.__body]

    def GetName(self):
        return self.name

    def GetType(self):
        return self.__type

    def GetArguments(self):
        return self.arguments

    def GetBody(self):
        return self.__body
    
    def __str__(self):
        return '{} ({} argument(s))'.format (self.GetName (), len (self.GetArguments()))

class ShaderType(Enum):
    Vertex      = 0
    Hull        = 1
    Domain      = 2
    Geometry    = 3
    Pixel       = 4
    Compute     = 5

class Shader(Function):
    '''Shaders are a subclass of fun__functionshe function name is
    autogenerated from the shader type.'''
    def __init__(self, shaderType, returnType, arguments = list (),
                 body = None):
        assert isinstance(shaderType, ShaderType)
        self.shaderType = shaderType     
        if self.shaderType == ShaderType.Vertex:
            self.name = 'VS_main'
        elif self.shaderType == ShaderType.Hull:
            self.name = 'HS_main'
        elif self.shaderType == ShaderType.Domain:
            self.name = 'DS_main'
        elif self.shaderType == ShaderType.Geometry:
            self.name = 'GS_main'
        elif self.shaderType == ShaderType.Pixel:
            self.name = 'PS_main'
        elif self.shaderType == ShaderType.Compute:
            self.name = 'CS_main'

        super().__init__(self.name, arguments, returnType, body)

    def GetShaderType(self):
        return self.shaderType

    def __str__(self):
        return '''shader: '{0}' ({1} argument(s))'''.format(
            self.shaderType.name, len(self.arguments))

class Statement(Node):
    pass

class FlowStatement(Statement):
    pass

class EmptyStatement(Statement):
    pass

class ExpressionStatement(Statement):
    def __init__(self, expr):
        super().__init__()
        self.__expression = expr

    def _GetChildren(self):
        return [self.__expression]

    def GetExpression(self):
        return self.__expression

    def __str__(self):
        return 'Expression'

class CompoundStatement(Statement):
    '''Compound statement consisting of zero or more statements.
    Compound statements also create a new visibility block.'''
    def __init__(self, stmts):
        super().__init__()
        self.__statements = stmts

    def GetStatements(self):
        return self.__statements

    def _GetChildren(self):
        return [self.__statements]

    def __len__(self):
        return len(self.__statements)

    def __iter__(self):
        '''Iterate over the statements.'''
        return self.__statements.__iter__()

    def __str__(self):
        return '{0} statement(s)'.format (len(self))

class ReturnStatement(FlowStatement):
    def __init__(self, expression):
        super().__init__()
        self.__expression = expression

    def _GetChildren(self):
        return [self.__expression]

    def GetExpression(self):
        return self.__expression

    def __str__(self):
        return 'return ' + str(self.__expression)

class DeclarationStatement(Statement):
    def __init__(self, variableDeclarations):
        super().__init__()
        self.declarations = variableDeclarations

    def GetDeclarations(self):
        return self.declarations

    def _GetChildren(self):
        return [self.declarations]

    def __str__(self):
        return '{0} declaration(s)'.format(len(self.declarations))

class IfStatement(FlowStatement):
    def __init__(self, cond, true_path, else_path=None):
        super().__init__()
        self.__condition = cond
        self.__trueBlock = true_path
        self.__elseBlock = else_path

    def _GetChildren(self):
        return [self.__condition, self.__trueBlock, self.__elseBlock]

    def GetCondition(self):
        return self.__condition

    def GetTruePath(self):
        return self.__trueBlock

    def GetElsePath(self):
        return self.__elseBlock

    def HasElsePath(self):
        return self.__elseBlock is not None

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

    def _GetField(self):
        return [self.__initializer, self.__condition, self.__next, self.__body]

    def GetBody (self):
        return self.__body

    def GetInitialization (self):
        return self.__initializer

    def GetCondition(self):
        return self.__condition

    def GetNext (self):
        return self.__next

class DoStatement(FlowStatement):
    def __init__(self, cond, body):
        super().__init__()
        self.__condition = cond
        self.__body = body

    def _GetChildren(self):
        return [self.__body, self.__condition]

    def GetCondition(self):
        return self.__condition

    def GetBody (self):
        return self.__body

class WhileStatement(FlowStatement):
    def __init__(self, cond, body):
        super().__init__()
        self.__condition = cond
        self.__body = body

    def _GetChildren(self):
        return [self.__body, self.__condition]

    def GetCondition(self):
        return self.__condition

    def GetBody (self):
        return self.__body

class Annotation(Node):
    def __init__(self, value):
        super().__init__()
        self.__value = value

    def GetValue(self):
        return self.__value

    def __str__(self):
        return '[{}]'.format (self.__value)

    def __repr__(self):
        return 'Annotation({})'.format (repr(self.__value))

class Visitor:
    def __init__(self):
        pass
    
    def SetErrorHandler (self, errorHandler):
        self.errorHandler = errorHandler
        
    def SetOutput(self, output):
        self.output = output
        
    def Print(self, *args, end='\n'):
        print (*args, end=end, file=self.output)

    def v_Generic (self, obj, ctx=None):
        '''The default visitation function.
        
        As Python doesn't support function overloading per type, this simulates
        the resolve that would happen by obtaining a list of all parent classes
        of the object. For each class, a function v_ClassName is called. This
        makes it possible for instance to have a generic handler for all
        ``Expression`` classes yet keep an overload for ``BinaryExpression``.'''
        import inspect
        
        # This includes the class itself
        baseClasses = list (inspect.getmro (obj.__class__))
        
        for baseClass in baseClasses:
            if baseClass is object:
                break
            
            fname = 'v_{}'.format (baseClass.__name__)
            
            if hasattr(self, fname):
                func = getattr (self, fname)
                return func (obj, ctx)
        
        return self.v_Default (obj, ctx)

    def v_Default(self, obj, ctx):
        print ('Missing visit method: "{}.v_{}"'.format (
            self.__class__.__name__,
            obj.__class__.__name__))
        return None

    def GetContext (self):
        return None

    def v_Visit (self, obj, ctx=None):
        return self.v_Generic (obj, ctx)

    def Visit(self, root):
        return self.v_Generic (root, self.GetContext ())

class DefaultVisitor(Visitor):
    def __init__(self):
        super().__init__()
    
    def v_Default(self, obj, ctx=None):
        '''Traverse further if possible.'''
        super().__init__()
        if hasattr (obj, 'AcceptVisitor'):
            return obj.AcceptVisitor (self, ctx)

class DebugPrintVisitor(DefaultVisitor):
    def __init__(self):
        super().__init__()