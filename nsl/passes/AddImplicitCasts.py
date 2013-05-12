from collections import OrderedDict
from nsl import ast, types, Errors

class AddImplicitCastVisitor (ast.DefaultVisitor):
    def _GetTargetType (self, sourceType, componentType):
        if isinstance (sourceType, types.VectorType):
            return types.VectorType (componentType, sourceType.GetSize ())
        elif isinstance (sourceType, types.MatrixType):
            return types.MatrixType (componentType,
                sourceType.GetRowCount (), sourceType.GetColumnCount (),
                sourceType.GetOrder ())
        else:
            # Must be a scalar
            return componentType

    def v_BinaryExpression (self, node, ctx):
        self.v_Generic (node.GetLeft (), ctx)
        self.v_Generic (node.GetRight (), ctx)

        if node.GetLeft().type != node.GetRight().type:
            # possibly, we have a mixed vector/scalar expression
            if (node.GetLeft().type.IsVector () or node.GetLeft().type.IsMatrix ()) and node.GetRight().type.IsScalar ():
                # Vector/Matrix and scalars can be combined
                return
            elif node.GetLeft().type.IsScalar () and (node.GetRight().type.IsVector () or node.GetRight().type.IsMatrix ()):
                # Scalar and vector/matrix can be combined
                return
            else:
                if node.GetLeft().type != node.type:
                    node.SetLeft(ast.CastExpression (node.GetLeft (),
                        node.type, True))
                if node.GetRight().type != node.type:
                    node.SetRight(ast.CastExpression (node.GetRight (),
                        node.type, True))

    def v_ConstructPrimitiveExpression(self, node, ctx):
        # The primitive type of each argument must be the same as the result
        resultType = node.type.GetType ()

        arguments = []
        for p in node.GetArguments ():
            if p.type != resultType:
                arguments.append (ast.CastExpression (p,
                    self._GetTargetType (p.type, resultType),
                    True))
            else:
                arguments.append (p)

        node.SetArguments (arguments)

def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return True
    return Pass.MakePassFromVisitor(AddImplicitCastVisitor (),
        'add-implicit-casts', validator = IsValid)