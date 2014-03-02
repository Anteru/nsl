from collections import OrderedDict
from nsl import ast, types, Errors

def ParseSwizzleMask(mask):
    '''Parse a swizzle mask into a list of element indices, starting
    at 0.
    '''

    mapping = { 'x' : 0,
     'y' : 1,
     'z' : 2,
     'w' : 3,
     'r' : 0,
     'g' : 1,
     'b' : 2,
     'a' : 3
    }

    return [mapping [e] for e in mask]

def ComputeSwizzleType(inType, mask):
    '''Compute the resulting type of a swizzle operation.
    @param inType: Must be a PrimitiveType
    @param mask: A valid swizzle mask
    '''
    outComponentCount = len (mask)

    swizzleType = None
    if isinstance (inType, types.VectorType):
        swizzleType = inType.GetType ()
    else:
        swizzleType = inType

    if outComponentCount == 1:
        return swizzleType
    else:
        result = types.VectorType (swizzleType, outComponentCount)
        # Copy semantic if needed
        if inType.HasSemantic ():
            result.SetSemantic (inType.GetSemantic ())
        return result

class ComputeTypeVisitor(ast.DefaultVisitor):
    def GetContext(self):
        return [self.scope]

    def __init__(self):
        self.ok = True
        self.scope = types.Scope ()

    def v_StructureDefinition(self, decl, ctx):
        scope = ctx[-1]
        elements = OrderedDict ()
        for t in decl.GetElements ():
            # Resolve here allows for nested types
            elements [t.GetName ()] = types.Resolve (t.GetType (), scope)
        scope.RegisterVariable (decl.GetName (), types.StructType(decl.GetName (), elements))

    def v_InterfaceDefinition (self, decl, ctx):
        scope = ctx[-1]
        functions = []
        for f in decl.GetFunctions ():
            # Resolve here allows for nested types
            functions.append (types.Resolve (f.GetType (), scope))
        scope.RegisterVariable (decl.GetName (),
                                types.ClassType(decl.GetName (), dict(), functions))


    def v_CompoundStatement(self, stmt, ctx):
        ctx.append (types.Scope (ctx[-1]))
        for s in stmt:
            self.v_Visit (s, ctx)
        ctx.pop()

    def _ProcessExpression(self, expr, scope):
        assert isinstance(expr, ast.Expression), 'Expression {1} has type {0} which is not an expression type'.format(type(expr), expr)
        # We type-cast here so we can process access trees separately
        if isinstance(expr, ast.VariableAccessExpression):
            p = expr.GetParent ()
            # Figure out the parent type
            self._ProcessExpression(p, scope)
            if isinstance (expr, ast.MemberAccessExpression):
                if p.type.IsPrimitive ():
                    if p.type.IsVector () or p.type.IsScalar ():
                        # We allow both swizzling of vector and scalar types
                        expr.type = ComputeSwizzleType(p.type, expr.GetMember ())
                    else:
                        Errors.ERROR_CANNOT_SWIZZLE_PRIMITIVE_TYPE.Raise ()
                elif isinstance (p.type, types.StructType):
                    expr.type = p.type.GetMembers ().GetVariableType (expr.GetMember ())
                else:
                    Errors.ERROR_CANNOT_SWIZZLE_TYPE.Raise (p.type)
            elif isinstance (expr, ast.ArrayExpression):
                expr.type = p.type.GetType ()
        elif isinstance(expr, ast.PrimaryExpression):
            # Simply check the name
            expr.type = scope.GetVariableType (expr.GetName ())
        else:
            # Walk through all children
            for c in expr:
                self._ProcessExpression(c, scope)

            # during the walking up, we can compute the expression
            # type as well

            if isinstance(expr, ast.CallExpression):
                # As we know the parameter types now, we can finally resolve
                # overloaded functions
                expr.ResolveType(scope)
                expr.type = expr.function.GetReturnType()
            elif isinstance (expr, ast.AssignmentExpression):
                expr.type = expr.GetLeft ().type
            elif isinstance (expr, ast.BinaryExpression):
                expr.type = types.GetExpressionType (expr,
                    expr.GetLeft ().type,
                    expr.GetRight ().type)

        return expr.type

    def v_IfStatement(self, stmt, ctx):
        self._ProcessExpression(stmt.GetCondition(), ctx[-1])
        self.v_Visit (stmt.GetTruePath(), ctx)
        if stmt.HasElsePath():
            self.v_Visit (stmt.GetElsePath (), ctx)

    def v_DeclarationStatement(self, stmt, ctx):
        scope = ctx[-1]
        for decl in stmt.GetDeclarations():
            scope.RegisterVariable (decl.GetName (),
                            decl.ResolveType (scope))
            if decl.HasInitializerExpression():
                self._ProcessExpression(decl.GetInitializerExpression (),
                                        scope)

    def v_ExpressionStatement(self, stmt, ctx):
        self.type = self._ProcessExpression(stmt.GetExpression(), ctx[-1])

    def v_ReturnStatement(self, stmt, ctx):
        self.type = self._ProcessExpression(stmt.GetExpression(), ctx[-1])

    def v_Function(self, func, ctx):
        '''Computes the function type and processes all statements.'''
        func.ResolveType (ctx [-1])
        func.GetType ().Resolve (ctx[-1])
        if not isinstance (func, ast.Shader):
            ctx[-1].RegisterFunction (func.GetType ().GetName (), func.GetType ())
        scope = types.Scope (ctx[-1])
        ctx.append (scope)
        for (name, argType) in func.GetType ().GetArguments().items ():
            scope.RegisterVariable (name, argType)

        self.v_Visit (func.GetBody(), ctx)
        ctx.pop ()

    def v_Shader(self, shd, ctx=None):
        self.v_Function(shd, ctx)

    def v_Program(self, prog, ctx):
        # Must visit types first
        for programType in prog.GetTypes ():
            self.v_Visit (programType, ctx)
        for decl in prog.GetDeclarations ():
            self.v_Visit (decl, ctx)
        for func in prog.GetFunctions ():
            self.v_Visit (func, ctx)

    def v_Generic(self, node, ctx):
        ast.Visitor.v_Generic (self, node, ctx)

import nsl.Pass
class ComputeTypesPass(nsl.Pass.Pass):
    def __init__(self):
        import os, pickle, nsl.parser
        # register default functions and types
        self.visitor = ComputeTypeVisitor ()
        stdlib = None
        if not os.path.exists ('stdlib.cache') or True:
            # generate the std lib first
            stdlib_vis = ComputeTypeVisitor ()
            p = nsl.parser.NslParser ()
            stdlib_ast = p.Parse (open ('nsl/stdlib.nsl').read ())
            stdlib_vis.Visit(stdlib_ast)
            stdlib = stdlib_vis.scope
            pickle.dump(stdlib, open ('stdlib.cache', 'wb'))
        else:
            stdlib = pickle.load(open ('stdlib.cache', 'rb'))
        self.visitor.scope = stdlib

    def GetName (self):
        return 'compute-types'

    def Process (self, ast, ctx=None):
        self.visitor.Visit (ast)

        return self.visitor.ok

def GetPass():
    return ComputeTypesPass ()