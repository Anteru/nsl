from nsl import ast, Errors, types

class ValidateExportedFunctionsVisitor(ast.DefaultVisitor):
    '''Validate that exported functions have no overloads.'''
    def __init__(self):
        super().__init__()
        self.valid = True
        self._exportedFunctions = set()

    def _ValidateFunction(self, f):
        if f.isExported:
            name = f.GetName()
            if name in self._exportedFunctions:
                self.valid = False
                Errors.ERROR_OVERLOADED_EXPORTED_FUNCTION.Raise(name)
            else:
                self._exportedFunctions.add(name)

    def v_Function(self, f, ctx=None):
        with Errors.CompileExceptionToErrorHandler (self.errorHandler):
            self._ValidateFunction(f)
            
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateExportedFunctionsVisitor(),
                                    'validate-exported-functions',
                                    validator = IsValid)