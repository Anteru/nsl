from nsl import ast, types

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

        if node.GetLeft ().type != node.operator.GetOperandType (0):
            node.SetLeft (ast.CastExpression (node.GetLeft (),
                node.operator.GetOperandType (0), True))

        if node.GetRight ().type != node.operator.GetOperandType (1):
            node.SetRight (ast.CastExpression (node.GetRight (),
                node.operator.GetOperandType (1), True))

    def v_ConstructPrimitiveExpression(self, node, ctx):
        # The primitive type of each argument must be the same as the result
        resultType = node.type.GetElementType ()

        arguments = []
        for p in node.GetArguments ():
            # If this is something like float4 (float2, int, int), we want to
            # cast int->float but float2 should not be casted
            argumentType = p.type.GetElementType()
            if argumentType != resultType:
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