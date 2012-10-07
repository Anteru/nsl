from nsl import types

class Node:
    def _GetChildFields (self):
        return []

    def Clone(self):
        import copy
        return copy.deepcopy(self)

    def Traverse(self, visitor, ctx=None):
        '''Traverse all child members in the tree. A child member can be a
        dictionary (also OrderedDict), a list, a set or the child node itself.'''
        fields = self._GetChildFields()
        for field in fields:
            e = getattr (self, field)
            if e is None:
                continue
            cn = e.__class__.__name__
            if cn == 'list' or cn == 'set':
                for c in e:
                    assert isinstance (c, Node), 'Node child must be of type Node but was of type {}'.format (c.__class__.__name__)
                    visitor.v_Generic (c, ctx)
            elif cn == 'dict' or cn == 'OrderedDict':
                for c in e.values ():
                    assert isinstance (c, Node), 'Node child must be of type Node'
                    visitor.v_Generic (c, ctx)
            else:
                assert isinstance (e, Node), 'Node child must be of type Node'
                visitor.v_Generic (e, ctx)

class Program (Node):
    '''Program container, keeping everything together.'''
    def __init__(self):
        self.variables = list ()
        self.functions = list ()
        self.types = dict ()

    def _GetChildFields (self):
        return ['variables', 'functions', 'types']

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

class Expression(Node):
    def __init__(self, children=[]):
        self.children = children

    def _GetChildFields(self):
        return ['children']

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
        self.type = targetType
        self.implicit = implicit

    def GetTargetType (self):
        return self.type

class ConstructPrimitiveExpression(UnaryExpression):
    '''Expression of the type primitive_type (expr, ...).'''
    def __init__(self, targetType, expressions):
        Expression.__init__(self, expressions)
        self.type = targetType
        self.expressions = expressions

    def __str__(self):
        r = self.type.GetName () + ' ('
        r += ', '.join(['{0}'.format(str(expr)) for expr in self.expressions])
        return r + ')'

    def GetArguments(self):
        return self.expressions

class CallExpression(UnaryExpression):
    '''Expression of the type ID ([expr], ...). ID references
    an unresolved function type at first.'''
    def __init__(self, function, expressions):
        Expression.__init__(self, expressions)
        self.function = function
        self.expressions = expressions

    def __str__(self):
        r = self.function.GetName () + ' ('
        r += ', '.join(['{0}'.format(str(expr)) for expr in self.expressions])
        return r + ')'

    def GetArguments(self):
        return self.expressions

    def ResolveType(self, scope):
        self.function = types.ResolveFunction(self.function,
                                              scope, [expr.type for expr in self.expressions])

class VariableAccessExpression(UnaryExpression):
    pass

class ArrayExpression(VariableAccessExpression):
    '''Expression of the form 'id[expr]', where id can be a nested
    access expression itself.'''
    def __init__(self, id, expression):
        Expression.__init__(self, [expression])
        self.id = id
        self.expression = expression

    def GetParent(self):
        return self.id

    def __str__(self):
        return str(self.id) + ' [' + str(self.expression) + ']'

class MemberAccessExpression(VariableAccessExpression):
    '''Expression of the form 'id.member', where id can be a
    access nested expression itself.'''
    def __init__(self, id, member):
        Expression.__init__(self)
        self.id = id
        self.member = member

    def GetMember(self):
        return self.member

    def GetParent(self):
        return self.id

    def __str__(self):
        return str(self.id) + '.' + str(self.member)

class Operation:
    ASSIGN = 1
    ADD = 102
    SUB = 103
    MUL = 104
    DIV = 105
    MOD = 106

    UA_ADD = 120,
    UA_SUB = 121,

    CMP_GT = 200
    CMP_LT = 201
    CMP_LE = 202
    CMP_GE = 203
    CMP_NE = 204
    CMP_EQ = 205
    LG_OR   = 300
    LG_AND  = 301
    LG_NOT  = 302
    BIT_OR  = 400
    BIT_AND = 401
    BIT_XOR = 402
    BIT_NOT = 403

def IsComparison(op):
    return op > 200 and op < 210

_op_str_map = {
    '='   : Operation.ASSIGN,

    '+'   : Operation.ADD,
    '-'   : Operation.SUB,
    '/'   : Operation.DIV,
    '*'   : Operation.MUL,
    '%'   : Operation.MOD,

    '++'  : Operation.UA_ADD,
    '--'  : Operation.UA_SUB,

    '&&'  : Operation.LG_AND,
    '||'  : Operation.LG_OR,
    '!'   : Operation.LG_NOT,

    '>'   : Operation.CMP_GT,
    '<'   : Operation.CMP_LT,
    '>='  : Operation.CMP_GE,
    '<='  : Operation.CMP_LE,
    '=='  : Operation.CMP_EQ,
    '!='  : Operation.CMP_NE,

    '|'   : Operation.BIT_OR,
    '&'   : Operation.BIT_AND,
    '~'   : Operation.BIT_NOT,
    '^'   : Operation.BIT_XOR
}

_str_op_map = {v : k for (k, v) in _op_str_map.items ()}

def StrToOp(op):
    assert op in _op_str_map, "Unknown operation: '{}".format (op)
    return _op_str_map [op]

def OpToStr(s):
    assert s in _str_op_map, "Unknown operation ID: '{}'".format (s)
    return _str_op_map [s]

class BinaryExpression(Expression):
    def __init__(self, op, left, right):
        Expression.__init__(self, [left, right])
        self.op = op
        self.left = left
        self.right = right

    def GetLeft(self):
        return self.left

    def GetRight(self):
        return self.right

    def GetOperation(self):
        return self.op

    def __str__(self):
        r = ''
        if (isinstance (self.left, BinaryExpression)):
            r += '(' + str (self.left) + ')'
        else:
            r += str (self.left)

        r += ' ' + OpToStr(self.op) +  ' '

        if (isinstance (self.right, BinaryExpression)):
            r += '(' + str (self.right) + ')'
        else:
            r += str (self.right)

        return r
class AssignmentExpression(BinaryExpression):
    def __init__(self, left, right):
        BinaryExpression.__init__(self, Operation.ASSIGN, left, right)

class Affix:
    PRE = 1
    POST = 2

class AffixExpression(UnaryExpression):
    def __init__(self, op, expr, affix):
        Expression.__init__(self, [expr])
        self.op = op
        self.expr = expr
        self.affix = affix

    def IsPostfix (self):
        return self.affix == Affix.POST

    def IsPrefix (self):
        return self.affix == Affix.PRE

    def GetOperation (self):
        return self.op

class LiteralExpression(UnaryExpression):
    def __init__(self, value, type):
        Expression.__init__(self)
        self.value = value
        self.type = type

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

    def _GetChildFields(self):
        return ['elements']

    def GetName(self):
        return self.name

    def __str__(self):
        return 'struct {0} ({1} fields)'.format (self.GetName (), len (self.elements))

    def GetElements (self):
        return self.elements

class InterfaceDefinition(Node):
    def __init__(self, name, functions = list ()):
        self.name = name
        self.functions = functions

    def _GetChildFields (self):
        return ['functions']

    def GetFunctions (self):
        return self.functions

    def GetName (self):
        return self.name

class Semantic:
    def __init__(self, semantic, slot = None):
        self.semantic = semantic
        self.slot = slot

    def GetName(self):
        return self.semantic

    def GetSlot (self, default = None):
        if self.slot is not None:
            return self.slot
        else:
            return default

    def __str__ (self):
        if self.slot is not None:
            return '{}[{}]'.format (self.semantic, self.slot)
        else:
            return self.semantic

    def __repr__(self):
        return 'Semantic ({}, {})'.format (repr(self.semantic), repr (self.slot))

class VariableDeclaration(Node):
    def __init__(self, elementType, symbol,
                 semantic = None,
                 initExpression = None,
                 arraySize = None):
        self.symbol = symbol
        self.semantic = semantic
        self.initExpression = initExpression

        if arraySize is not None:
            self.type = types.ArrayType (elementType, arraySize)
        else:
            self.type = elementType

    def ResolveType(self, scope):
        self.type = types.Resolve(self.type, scope)

        if self.HasSemantic():
            self.type.SetSemantic (self.semantic)

        return self.type

    def _GetChildFields(self):
        return ['initExpression']

    def __str__(self):
        result = str(self.type) + ' ' + str(self.GetName ())

        if (self.HasInitializerExpression ()):
            result += '= ' + str(self.initExpression)

        if (self.HasSemantic()):
            result += ' : ' + str(self.semantic)

        return result

    def GetType(self):
        assert not isinstance (self.type, types.UnresolvedType)
        return self.type

    def GetName(self):
        return self.symbol

    def HasSemantic(self):
        return self.semantic is not None

    def GetSemantic(self):
        return self.semantic

    def HasInitializerExpression(self):
        return self.initExpression is not None

    def GetInitializerExpression(self):
        return self.initExpression

class ArgumentModifier:
    NONE = 0
    OPTIONAL = 1

class Argument(Node):
    '''Function argument. Captures the type (potentially a Type or
    UnresolvedType) and the name of the argument.'''
    def __init__(self, type, name = None, modifiers = []):
        self.type = type
        self.name = name
        self.modifiers = modifiers

    def ResolveType(self, scope):
        self.type = types.Resolve(self.type, scope)
        return self.type

    def GetType(self):
        return self.type

    def GetName(self):
        return self.name

    def HasName (self):
        return self.name is not None

    def __str__(self):
        if self.name is not None:
            return '{} {}'.format (self.type.GetName (), self.name)
        else:
            return '{} <unnamed>'.format (self.type.GetName ())

class Function(Node):
    def __init__(self, name, arguments = list (),
                 returnType = types.Void (), body = None,
                 isForwardDeclaration = False):
        self.name = name
        self.body = body
        self.type = types.Function (name, returnType, arguments)
        self.arguments = arguments
        self.isForwardDeclaration = isForwardDeclaration

    def ResolveType(self, scope):
        for arg in self.arguments:
            arg.ResolveType (scope)

    def _GetChildFields(self):
        return ['arguments', 'body']

    def GetName(self):
        return self.name

    def GetType(self):
        return self.type

    def GetArguments(self):
        return self.arguments

    def GetBody(self):
        return self.body

class ShaderType:
    VERTEX      = 0
    HULL        = 1
    DOMAIN      = 2
    GEOMETRY    = 3
    PIXEL       = 4

def ShaderTypeToString(st):
    if st == ShaderType.VERTEX:
        return 'vertex'
    elif st == ShaderType.PIXEL:
        return 'pixel'
    elif st == ShaderType.HULL:
        return 'hull'
    elif st == ShaderType.GEOMETRY:
        return 'geometry'
    elif st == ShaderType.DOMAIN:
        return 'domain'

def StringToShaderType(s):
    if s == 'vertex':
        return ShaderType.VERTEX
    elif s == 'pixel':
        return ShaderType.PIXEL
    elif s == 'geometry':
        return ShaderType.GEOMETRY
    elif s == 'hull':
        return ShaderType.HULL
    elif s == 'domain':
        return ShaderType.DOMAIN

class Shader(Function):
    '''Shaders are a subclass of functions. The function name is
    autogenerated from the shader type.'''
    def __init__(self, type, returnType, arguments = list (),
                 body = None):
        self.shaderType = StringToShaderType (type)
        if self.shaderType == ShaderType.PIXEL:
            self.name = 'PS_main'
        elif self.shaderType == ShaderType.VERTEX:
            self.name = 'VS_main'
        elif self.shaderType == ShaderType.DOMAIN:
            self.name = 'DS_main'
        elif self.shaderType == ShaderType.HULL:
            self.name = 'HS_main'
        elif self.shaderType == ShaderType.GEOMETRY:
            self.name = 'GS_main'

        Function.__init__(self, self.name, arguments, returnType, body)

    def GetShaderType(self):
        return self.shaderType

class Statement(Node):
    pass

class FlowStatement(Statement):
    pass

class EmptyStatement(Statement):
    def Accept(self, visitor, ctx=None):
        pass

class ExpressionStatement(Statement):
    def __init__(self, expr):
        self.expression = expr

    def _GetChildFields(self):
        return ['expression']

    def GetExpression(self):
        return self.expression

class CompoundStatement(Statement):
    '''Compound statement consisting of zero or more statements.
    Compound statements also create a new visibility block.'''
    def __init__(self, stmts):
        self.statements = stmts

    def GetStatements(self):
        return self.statements

    def _GetChildFields(self):
        return ['statements']

    def __len__(self):
        return len(self.statements)

    def __iter__(self):
        '''Iterate over the statements.'''
        return self.statements.__iter__()

class ReturnStatement(FlowStatement):
    def __init__(self, expression):
        self.expression = expression

    def _GetChildFields(self):
        return ['expression']

    def GetExpression(self):
        return self.expression

class DeclarationStatement(Statement):
    def __init__(self, variableDeclarations):
        self.declarations = variableDeclarations

    def GetDeclarations(self):
        return self.declarations

    def _GetChildFields(self):
        return ['declarations']

class IfStatement(FlowStatement):
    def __init__(self, cond, true_path, else_path=None):
        self.cond = cond
        self.true_path = true_path
        self.else_path = else_path

    def _GetChildFields(self):
        return ['cond', 'true_path', 'else_path']

    def GetCondition(self):
        return self.cond

    def GetTruePath(self):
        return self.true_path

    def GetElsePath(self):
        return self.else_path

    def HasElsePath(self):
        return self.else_path is not None

class ContinueStatement(FlowStatement):
    def __init__(self):
        pass

class BreakStatement(FlowStatement):
    def __init__(self):
        pass

class ForStatement(FlowStatement):
    def __init__(self, init, cond, next, body):
        self.init = init
        self.cond = cond
        self.next = next
        self.body = body

    def _GetField(self):
        return ['init', 'next', 'body']

    def GetBody (self):
        return self.body

    def GetInitialization (self):
        return self.init

    def GetCondition(self):
        return self.cond

    def GetNext (self):
        return self.next

class DoStatement(FlowStatement):
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body

    def _GetChildFields(self):
        return ['body', 'cond']

    def GetCondition(self):
        return self.body

    def GetBody (self):
        return self.body

class WhileStatement(FlowStatement):
    def __init__(self, cond, body):
        self.cond = cond
        self.body = body

    def _GetChildFields(self):
        return ['body', 'cond']

    def GetCondition(self):
        return self.body

    def GetBody (self):
        return self.body

class Visitor:
    def SetErrorHandler (self, errorHandler):
        self.errorHandler = errorHandler

    def v_Generic (self, obj, ctx=None):
        fname = 'v_{}'.format (obj.__class__.__name__)

        if hasattr(self, fname):
            func = getattr (self, fname)
            return func (obj, ctx)
        else:
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
        self.v_Generic (root, self.GetContext ())

class DefaultVisitor(Visitor):
    def v_Default(self, obj, ctx=None):
        '''Traverse further if possible.'''
        if hasattr (obj, 'Traverse'):
            obj.Traverse (self, ctx)

class DebugVisitor(Visitor):
    def GetContext (self):
        return 0

    def v_Generic(self, obj, ctx=None):
        Visitor.v_Generic (self, obj, ctx)
        if hasattr (obj, 'Traverse'):
            obj.Traverse (self, ctx + 1)

    def v_Default(self, obj, ctx):
        print (' '*ctx*2, obj.__class__.__name__,)
        print (' '*(ctx*2 + 4), str (obj))