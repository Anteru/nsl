import collections.abc
from nsl import op, types
from enum import Enum

class InvalidChildType(Exception):
    def __init__(self, actualType):
        self.actualType = actualType

class Node:
    '''Base class for all nodes in the AST.

    The generic AST node provides a traversal function, which traverses into
    all children of the current object. Derived classes can override Traverse()
    to make the traversal more efficient.'''
    def _GetChildren (self):
        return []

    def Clone(self):
        import copy
        return copy.deepcopy(self)

    def Traverse(self, visitor, ctx=None):
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
                return visitor.v_Generic (e, ctx)

class Program (Node):
    '''Program container, keeping everything together.'''
    def __init__(self):
        self.variables = list ()
        self.functions = list ()
        self.types = dict ()

    def _GetChildren (self):
        return [self.variables, self.functions, self.types]

    def AddDeclaration (self, variable):
        self.variables.append (variable)

    def GetDeclarations (self):
        return self.variables

    def GetTypes(self):
        return self.types.values ()

    def GetFunctions(self):
        return self.functions

    def AddFunction(self, func):
        self.functions.append (func)

    def AddType (self, decl):
        self.types [decl.GetName ()] = decl

    def __str__(self):
        return '''Program ({0} variable(s), {1} function(s), {2} type(s))'''.format(
            len(self.variables), len(self.functions), len(self.types))

class Expression(Node):
    def __init__(self, children=[]):
        self.children = children
        self.__type = types.UnresolvedType
        
    def GetType(self):
        return self.__type
    
    def SetType(self, nslType):
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
        Expression.__iter__(self)

class CastExpression(UnaryExpression):
    def __init__(self, expr, targetType, implicit = False):
        Expression.__init__(self, [expr])
        self.SetType (targetType)
        self.__implicit = implicit
    
    def IsImplicit(self):
        return self.__implicit
    
    def GetArgument(self):
        return self.children[0]

    def __str__(self):
        return '{} ({})'.format (self.GetType(), self.children [0])

class ConstructPrimitiveExpression(UnaryExpression):
    '''Expression of the type primitive_type (expr, ...).'''
    def __init__(self, targetType, expressions):
        Expression.__init__(self, expressions)
        self.SetType (targetType)

    def __str__(self):
        r = self.GetType().GetName () + ' ('
        r += ', '.join([str(expr) for expr in self.children])
        return r + ')'

    def GetArguments(self):
        return self.children

    def SetArguments(self, args):
        self.children = args

class CallExpression(UnaryExpression):
    '''A function call of the form ID ([expr], ...). ID references
    an unresolved function type at first.'''
    def __init__(self, function, expressions):
        Expression.__init__(self, expressions)
        self.function = function

    def __str__(self):
        r = self.function.GetName () + ' ('
        r += ', '.join(['{0}'.format(str(expr)) for expr in self.children])
        return r + ')'

    def GetArguments(self):
        return self.children

    def ResolveType(self, scope):
        self.function = types.ResolveFunction(self.function,
            scope, [expr.__type for expr in self.children])

class VariableAccessExpression(UnaryExpression):
    pass

class ArrayExpression(VariableAccessExpression):
    '''Expression of the form 'id[expr]', where id can be a nested
    access expression itself.'''
    def __init__(self, identifier, expression):
        Expression.__init__(self, [expression])
        self.id = identifier

    def GetParent(self):
        return self.id

    def __str__(self):
        return str(self.id) + ' [' + str(self.children[0]) + ']'

class MemberAccessExpression(VariableAccessExpression):
    '''Expression of the form 'id.member', where id can be a
    access nested expression itself.'''
    def __init__(self, identifier, member):
        Expression.__init__(self)
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
        Expression.__init__(self, [left, right])
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
        BinaryExpression.__init__(self, op.Operation.ASSIGN, left, right)
        
    def ResolveType(self, left, right):        
        self._operator = types.ExpressionType (self.GetLeft().GetType (),
                                              [self.GetLeft ().GetType(),
                                               self.GetRight ().GetType ()])

class Affix:
    PRE = 1
    POST = 2

class AffixExpression(UnaryExpression):
    def __init__(self, op, expr, affix):
        Expression.__init__(self, [expr])
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
        Expression.__init__(self)
        self.value = value
        self.SetType (literalType)

    def GetValue(self):
        return self.value

    def __str__(self):
        return str (self.value)

class PrimaryExpression(UnaryExpression):
    def __init__(self, identifier):
        Expression.__init__(self)
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
    def __init__(self, name, elements = list()):
        self.name = name
        self.elements = elements

        # Check that all element names are unique
        elementNames = set()
        for e in elements:
            if e.GetName () in elementNames:
                raise InvalidStructureDefinition()
            elementNames.add (e.GetName ())

    def _GetChildren(self):
        return [self.elements]

    def GetName(self):
        return self.name

    def __str__(self):
        return 'struct {0} ({1} field(s))'.format (self.GetName (), len (self.elements))

    def GetElements (self):
        return self.elements

class InterfaceDefinition(Node):
    def __init__(self, name, functions = list ()):
        self.name = name
        self.functions = functions

    def _GetChildren (self):
        return [self.functions]

    def GetFunctions (self):
        return self.functions

    def GetName (self):
        return self.name

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
            return '{}[{}]'.format (self.__semantic, self.__slot)
        else:
            return str (self.__semantic)

    def __repr__(self):
        return 'Semantic ({}, {})'.format (repr(self.__semantic), repr (self.__slot))

class VariableDeclaration(Node):
    def __init__(self, elementType, symbol,
                 semantic = None,
                 initExpression = None,
                 arraySize = None):
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
        result = str(self.__type) + ' ' + str(self.GetName ())

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
    NONE = 0
    OPTIONAL = 1

class Argument(Node):
    '''Function argument. Captures the type (potentially a Type or
    UnresolvedType) and the name of the argument.'''
    def __init__(self, argumentType, name = None, modifiers = []):
        self.__type = argumentType
        self.name = name
        self.modifiers = modifiers

    def ResolveType(self, scope):
        self.__type = types.Resolve(self.__type, scope)
        return self.__type

    def GetType(self):
        return self.__type

    def GetName(self):
        return self.name

    def HasName (self):
        return self.name is not None

    def __str__(self):
        if self.name is not None:
            return '{} {}'.format (self.__type.GetName (), self.name)
        else:
            return '{} <unnamed>'.format (self.__type.GetName ())

class Function(Node):
    def __init__(self, name, arguments = list (),
                 returnType = types.Void (), body = None,
                 isForwardDeclaration = False):
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
            return []
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

class ShaderType(Enum):
    Vertex      = 0
    Hull        = 1
    Domain      = 2
    Geometry    = 3
    Pixel       = 4
    Compute     = 5

class Shader(Function):
    '''Shaders are a subclass of functions. The function name is
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

        Function.__init__(self, self.name, arguments, returnType, body)

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
        self.__expression = expression

    def _GetChildren(self):
        return [self.__expression]

    def GetExpression(self):
        return self.__expression

    def __str__(self):
        return 'return ' + str(self.__expression)

class DeclarationStatement(Statement):
    def __init__(self, variableDeclarations):
        self.declarations = variableDeclarations

    def GetDeclarations(self):
        return self.declarations

    def _GetChildren(self):
        return [self.declarations]

    def __str__(self):
        return '{0} declaration(s)'.format(len(self.declarations))

class IfStatement(FlowStatement):
    def __init__(self, cond, true_path, else_path=None):
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
        pass

class BreakStatement(FlowStatement):
    def __init__(self):
        pass

class ForStatement(FlowStatement):
    def __init__(self, init, cond, increment, body):
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
        self.__condition = cond
        self.__body = body

    def _GetChildren(self):
        return [self.__body, self.__condition]

    def GetCondition(self):
        return self.__condition

    def GetBody (self):
        return self.__body

class Visitor:
    def SetErrorHandler (self, errorHandler):
        self.errorHandler = errorHandler

    def v_Generic (self, obj, ctx=None):
        '''The default visitation function.
        
        As Python doesn't support function overloading per type, this simulates
        the resolve that would happen by obtaining a list of all parent classes
        of the object. For each class, a function v_ClassName is called. This
        makes it possible for instance to have a generic handler for all
        ``Expression`` classes yet keep an overload for ``BinaryExpression``.'''
        import inspect
        
        baseClasses = list (inspect.getmro (obj.__class__))
        
        for baseClass in [obj.__class__] + baseClasses:
            if baseClass is object:
                break
            
            fname = 'v_{}'.format (baseClass.__name__)
            
            if hasattr(self, fname):
                func = getattr (self, fname)
                return func (obj, ctx)
        
        return self.v_Default (obj, ctx)

    def v_Default(self, obj, ctx):
        print ('Missing visit method: "{}.v_{}"'.format (self.__class__.__name__,
            obj.__class__.__name__))
        return None

    def GetContext (self):
        return None

    def v_Visit (self, obj, ctx=None):
        return self.v_Generic (obj, ctx)

    def Visit(self, root):
        return self.v_Generic (root, self.GetContext ())

class DefaultVisitor(Visitor):
    def v_Default(self, obj, ctx=None):
        '''Traverse further if possible.'''
        if hasattr (obj, 'Traverse'):
            return obj.Traverse (self, ctx)

class DebugPrintVisitor(DefaultVisitor):
    def Visit(self, root):
        print ()
        print ('{} {} (Start) {}'.format ('=' * 16, self.__class__.__name__, '=' * 16))
        self.v_Generic (root, self.GetContext ())
        print ()
        print ('{} {} (End) {}'.format ('=' * 16, self.__class__.__name__, '=' * 16))
        print ()
