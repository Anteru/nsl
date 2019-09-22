from nsl import ast, op, types

class RewriteAssignEqualVisitor (ast.DefaultVisitor):        
    def __ProcessStatements(self, statements):
        for s in statements:
            if not isinstance(s, ast.ExpressionStatement):
                yield s
                continue

            # We have to iterate into those
            assert not isinstance(s, ast.CompoundStatement)

            expr = s.GetExpression ()
            if not isinstance(expr, ast.AssignmentExpression):
                yield s
                continue

            yield ast.ExpressionStatement(
                self.__ProcessAssignmentExpression(s.GetExpression ()))

    def __ProcessAssignmentExpression(self, node):
        assert isinstance(node, ast.AssignmentExpression)
        operation = node.GetOperation ()
        if operation != op.Operation.ASSIGN:
            # Rewrite x <op-equal> y to
            # x = x <op> y
            left = node.GetLeft ()
            right = node.GetRight ()

            opMap = {
                op.Operation.ASSIGN_ADD_EQUAL : op.Operation.ADD,
                op.Operation.ASSIGN_SUB_EQUAL : op.Operation.SUB,
                op.Operation.ASSIGN_MUL_EQUAL : op.Operation.MUL,
                op.Operation.ASSIGN_DIV_EQUAL : op.Operation.DIV
            }

            operation = opMap [operation]
            
            return ast.AssignmentExpression(left,
                ast.BinaryExpression(operation, left, right))
        else:
            return node

    def v_CompoundStatement(self, node, ctx):
        node.SetStatements(list (self.__ProcessStatements(node.GetStatements ())))

def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return True
    return Pass.MakePassFromVisitor(RewriteAssignEqualVisitor (),
        'rewrite-assign-equal', validator = IsValid)