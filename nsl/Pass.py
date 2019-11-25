from io import StringIO
import enum

class PassFlags(enum.IntFlag):
    Default = 0
    IsOptimization = 0b1

class Pass:
    @property
    def Name(self):
        return self.__class__.__name__

    @property
    def Flags(self):
        return PassFlags.Default

    def Process(self, ast, ctx=None, output=StringIO()):
        return False

def MakePassFromVisitor(visitor, name, validator=None,
    *, flags = PassFlags.Default):
    class VisitorPass (Pass):
        def __init__(self):
            self.__visitor = visitor
            self.__flags = flags

        @property
        def Flags(self):
            return self.__flags

        def Process(self, ast, ctx=None, output=StringIO()):
            import nsl.Errors
            errorHandler = nsl.Errors.ErrorHandler ()
            self.__visitor.SetErrorHandler (errorHandler)
            self.__visitor.SetOutput (output)
            self.__visitor.Visit (ast)

            for message in errorHandler.messages:
                print (message)

            if not validator:
                return True
            else:
                return validator (self.__visitor)

        @property
        def Name (self):
            return name

        @property
        def Visitor(self):
            return self.__visitor

    return VisitorPass ()