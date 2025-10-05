from io import StringIO
import enum
from typing import Callable
from .Visitor import Visitor


class PassFlags(enum.IntFlag):
    Default = 0
    IsOptimization = 0b1


class Pass:
    @property
    def Name(self):
        return self.__class__.__name__

    @property
    def Flags(self) -> PassFlags:
        return PassFlags.Default

    def Process(self, root, ctx=None, output=StringIO()) -> bool:
        return False


def MakePassFromVisitor(
    visitor, name, validator=None | Callable[[Visitor], bool], *,
    flags=PassFlags.Default
):
    class VisitorPass(Pass):
        def __init__(self):
            self.__visitor = visitor
            self.__flags = flags

        @property
        def Flags(self):
            return self.__flags

        def Process(self, root, ctx=None, output=StringIO()):
            import nsl.Errors

            errorHandler = nsl.Errors.ErrorHandler()
            self.__visitor.SetErrorHandler(errorHandler)
            self.__visitor.SetOutput(output)
            self.__visitor.Visit(root)

            for message in errorHandler.messages:
                print(message)

            if validator is None:
                return True
            else:
                return validator(self.__visitor)

        @property
        def Name(self):
            return name

        @property
        def Visitor(self):
            return self.__visitor

    return VisitorPass()
