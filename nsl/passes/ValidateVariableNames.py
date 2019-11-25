from typing import Optional
from nsl import ast, Errors, Visitor

class ValidateVariableNamesVisitor(Visitor.DefaultVisitor):
    '''Validate that variable names are unique in their scope. We don't allow
    shadowing a variable with another (if we wanted to, we have to generate
    unique variable names per scope.)'''

    class Context:
        def __init__(self, parent=None):
            self.__variables = dict()
            self.__parent = parent
        
        def Add(self, name, location) -> None:
            assert location is not None

            existingLocation = self.Get(name)
            if existingLocation is None:
                self.__variables[name] = location 
            else:
                Errors.ERROR_VARIABLE_NAME_ALREADY_USED.Raise (
                    name, location, existingLocation
                )

        def Get(self, name) -> Optional[ast.Location]:
            if name in self.__variables:
                return self.__variables [name]

            if self.__parent is not None:
                return self.__parent.Get(name)
            else:
                return None

    def __init__(self):
        super().__init__()
        self.valid = True
        self.__context = self.Context()
        def OnError():
            self.valid = False
        self.__onError = OnError

    def GetContext(self):
        return self.__context

    # List all nodes here which create new scopes.
    def v_StructureDefinition(self, sd, ctx=None):
        ctx = self.Context()
        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            sd.AcceptVisitor(self, ctx)

    def v_Function(self, func, ctx=None):
        ctx = self.Context(ctx)
        for arg in func.GetArguments():
            ctx.Add(arg.GetName(), arg.GetLocation ())
        
        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            self.v_Visit(func.GetBody (), ctx)

    def v_CompoundStatement(self, forStatement, ctx=None):
        ctx = self.Context(ctx)
        
        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            forStatement.AcceptVisitor(self, ctx)

    def v_ForStatement(self, forStatement, ctx=None):
        ctx = self.Context(ctx)
        
        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            forStatement.AcceptVisitor(self, ctx)

    def v_DoStatement(self, forStatement, ctx=None):
        ctx = self.Context(ctx)
        
        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            forStatement.AcceptVisitor(self, ctx)

    def v_WhileStatement(self, forStatement, ctx=None):
        ctx = self.Context(ctx)
        
        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            forStatement.AcceptVisitor(self, ctx)

    def v_IfStatement(self, ifStatement, ctx=None):
        ctx = self.Context(ctx)

        with Errors.CompileExceptionToErrorHandler(self.errorHandler,
            self.__onError):
            ifStatement.AcceptVisitor(self, ctx)

    def v_VariableDeclaration(self, decl, ctx):
        ctx.Add(decl.GetName(), decl.GetLocation())
            
def GetPass():
    from nsl import Pass
    def IsValid (visitor):
        return visitor.valid
    return Pass.MakePassFromVisitor(ValidateVariableNamesVisitor(),
                                    'validate-variable-names',
                                    validator = IsValid)