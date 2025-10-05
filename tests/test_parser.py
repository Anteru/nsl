from nsl import parser, ast, op, types
import pytest


@pytest.fixture
def ExprParser():
    return parser.NslParser(parser.ParseEntryPoint.Expression)


class TestExpressionParsing:
    def testParseBinaryExpression(self, ExprParser):
        expr = "23 + 48 * 18"
        exprNode = ExprParser.Parse(expr)

        assert isinstance(exprNode, ast.BinaryExpression)
        l0, r0 = exprNode.GetLeft(), exprNode.GetRight()
        assert exprNode.GetOperation() == op.Operation.ADD
        assert isinstance(l0, ast.LiteralExpression)
        assert isinstance(r0, ast.BinaryExpression)

        l1, r1 = r0.GetLeft(), r0.GetRight()
        assert r0.GetOperation() == op.Operation.MUL
        assert isinstance(l1, ast.LiteralExpression)
        assert isinstance(r1, ast.LiteralExpression)

        assert l0.GetValue() == 23
        assert l0.GetType() == types.Integer()
        assert l1.GetValue() == 48
        assert l1.GetType() == types.Integer()
        assert r1.GetValue() == 18
        assert r1.GetType() == types.Integer()

    def testMemberAccessExpression(self, ExprParser):
        expr = "foo.bar"
        exprNode = ExprParser.Parse(expr)

        assert isinstance(exprNode, ast.MemberAccessExpression)
        p, m = exprNode.GetParent(), exprNode.GetMember()
        assert isinstance(p, ast.PrimaryExpression)
        assert isinstance(m, ast.PrimaryExpression)
        assert p.GetName() == "foo"
        assert m.GetName() == "bar"

    def testImportStatement(self):
        p = parser.NslParser()
        root = p.Parse("""import "std";""")

        assert isinstance(root, ast.Module)

        imports = root.GetImports()
        assert "std" in imports
