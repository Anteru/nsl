from nsl.passes.ValidateExportedFunctions import (
    ValidateExportedFunctionsVisitor,
)

import nsl.ast
import nsl.types
import nsl.Errors


class TestValidateExportedFunctions:
    def testOverloadExportedFails(self):
        p = nsl.ast.Module()
        f1 = nsl.ast.Function("f", [nsl.types.Integer()], isExported=True)
        f2 = nsl.ast.Function("f", [nsl.types.Float()], isExported=True)
        p.AddFunction(f1)
        p.AddFunction(f2)

        validatePass = ValidateExportedFunctionsVisitor()
        validatePass.Visit(p)
        assert validatePass.valid == False

    def testOverloadNonExportedWorks(self):
        p = nsl.ast.Module()
        f1 = nsl.ast.Function("f", [nsl.types.Integer()])
        f2 = nsl.ast.Function("f", [nsl.types.Float()])
        p.AddFunction(f1)
        p.AddFunction(f2)

        validatePass = ValidateExportedFunctionsVisitor()
        validatePass.Visit(p)
        assert validatePass.valid == True
