from nsl import ast, types, Visitor


class AddImplicitCastVisitor(Visitor.DefaultVisitor):
    def _GetTargetType(self, sourceType: types.Type, componentType: types.Type):
        assert isinstance(sourceType, types.Type)
        assert isinstance(componentType, types.Type)

        if sourceType.IsVector() or sourceType.IsMatrix():
            return sourceType.WithComponentType(componentType)
        else:
            # Must be a scalar
            return componentType

    def v_ArrayExpression(self, node, ctx=None):
        assert isinstance(node, ast.ArrayExpression)
        node.GetExpression().AcceptVisitor(self, ctx)

        # We allow Integer or UnsignedInteger as the index
        exprType = node.GetExpression().GetType()
        if exprType != types.Integer() and exprType != types.UnsignedInteger():
            # Non-integer means it must be float, which is signed -- cast to
            # integer
            assert isinstance(exprType, types.Float)
            node.SetExpression(
                ast.CastExpression(node.GetExpression(), types.Integer(), True)
            )

    def v_BinaryExpression(self, node, ctx=None):
        assert isinstance(node, ast.BinaryExpression)

        self.v_Generic(node.GetLeft(), ctx)
        self.v_Generic(node.GetRight(), ctx)

        if node.GetLeft().GetType() != node.GetOperator().GetOperandType(0):
            node.SetLeft(
                ast.CastExpression(
                    node.GetLeft(), node.GetOperator().GetOperandType(0), True
                )
            )

        if node.GetRight().GetType() != node.GetOperator().GetOperandType(1):
            node.SetRight(
                ast.CastExpression(
                    node.GetRight(), node.GetOperator().GetOperandType(1), True
                )
            )

    def v_ConstructPrimitiveExpression(self, node, ctx=None):
        assert isinstance(node, ast.ConstructPrimitiveExpression)

        # The primitive type of each argument must be the same as the result
        resultType = node.GetType().GetComponentType()

        arguments = []
        for p in node.GetArguments():
            # If this is something like float4 (float2, int, int), we want to
            # cast int->float but float2 should not be casted
            argumentType = p.GetType().GetComponentType()
            if argumentType != resultType:
                arguments.append(
                    ast.CastExpression(
                        p, self._GetTargetType(p.GetType(), resultType), True
                    )
                )
            else:
                arguments.append(p)

        node.SetArguments(arguments)

    def v_CallExpression(self, node, ctx=None):
        assert isinstance(node, ast.CallExpression)

        # The primitive type of each argument must be the same as the argument type
        argumentTypes = node.function.GetArgumentTypes().values()

        arguments = []
        for arg, expectedType in zip(node.GetArguments(), argumentTypes):
            # This works for calls (same as above for construct primitive) as
            # non-primitive types return themselves in GetComponentType()
            argumentType = arg.GetType().GetComponentType()
            if argumentType != expectedType.GetComponentType():
                arguments.append(
                    ast.CastExpression(
                        arg,
                        self._GetTargetType(
                            arg.GetType(), expectedType.GetComponentType()
                        ),
                        True,
                    )
                )
            else:
                arguments.append(arg)

        node.SetArguments(arguments)


def GetPass():
    from nsl import Pass

    def IsValid(visitor):
        return True

    return Pass.MakePassFromVisitor(
        AddImplicitCastVisitor(), "add-implicit-casts", validator=IsValid
    )
