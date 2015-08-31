class Pass:
    def GetName(self):
        pass

    def Process(self, ast, ctx=None):
        return False

def MakePassFromVisitor(visitor, name, validator=None):
    class VisitorPass (Pass):
        def __init__(self):
            self.visitor = visitor

        def Process(self, ast, ctx=None):
            import nsl.Errors
            errorHandler = nsl.Errors.ErrorHandler ()
            self.visitor.SetErrorHandler (errorHandler)
            self.visitor.Visit (ast)

            for message in errorHandler.messages:
                print (message)

            if not validator:
                return True
            else:
                return validator (self.visitor)

        def GetName (self):
            return name

    return VisitorPass ()
