from nsl import ast, op, types

class RewriteAssignEqualVisitor (ast.DefaultVisitor):  
    '''Translate a <op-equal> b to a = a <op> b.'''      
    def v_AssignmentExpression(self, node, ctx=None):
        operation = node.GetOperation ()
        if operation == op.Operation.ASSIGN:
            return node
        
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

def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return True
    return Pass.MakePassFromVisitor(RewriteAssignEqualVisitor (),
        'rewrite-assign-equal', validator = IsValid)