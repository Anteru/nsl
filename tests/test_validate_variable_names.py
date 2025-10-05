from nsl.passes.ValidateVariableNames import ValidateVariableNamesVisitor

import nsl.ast
import nsl.types
import nsl.Errors


class TestValidateVariableNames:
    def testReusingArgumentNameForLocalVariableFails(self):
        f = nsl.ast.Function(
            "foo",
            [nsl.ast.Argument(nsl.types.Integer(), "a")],
            body=nsl.ast.VariableDeclaration(nsl.types.Integer(), "a"),
        )

        v = ValidateVariableNamesVisitor()
        v.Visit(f)

        assert not v.valid
