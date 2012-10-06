class ErrorMessage:
    def __init__ (self, code, severity, message):
        self.code = code
        self.severity = severity
        self.message = message

    def Print(self, *args):
        print (self.message.format (args))

    def Raise(self, *args):
        raise CompileException (self, *args)

class ErrorHandler:
    def __init__(self):
        self.errors = 0
        self.warnings = 0
        self.messages = []

    def Log (self, messageText, message):
        if message.severity == Severity.ERROR:
            self.errors += 1
        elif message.severity == Severity.WARNING:
            self.warnings += 1
        self.messages.append (messageText)

class CompileExceptionToErrorHandler:
    def __init__(self, errorHandler):
        self.errorHandler = errorHandler

    def __enter__(self):
        pass

    def __exit__(self, exc_type, exc_val, exc_tb):
        if exc_type == None:
            return True
        if issubclass(exc_type, CompileException):
            self.errorHandler.Log (exc_val.messageText, exc_val.message)
            return True
        return False

class CompileException(Exception):
    def __init__(self, message, *args):
        self.message = message
        self.messageText = message.message.format (*args)

    def __str__(self):
        return self.messageText

class Severity:
    ERROR = 1
    WARNING = 2
    INFO = 3

ERROR_INVALID_SWIZZLE_MASK = ErrorMessage (2001, Severity.ERROR,
    '''Invalid swizzle mask. A swizzle mask may contain only 'rgba' or 'xyzw' selectors.''')
ERROR_MIXED_SWIZZLE_MASK = ErrorMessage (2002, Severity.ERROR,
    'Invalid swizzle mask. A swizzle mask may not contain mixed selectors.')
ERROR_CONTINUE_OUTSIDE_FLOW = ErrorMessage (2011, Severity.ERROR,
    'continue statements must be inside a do/while/for statement.')
ERROR_BREAK_OUTSIDE_FLOW_SWITCH = ErrorMessage (2012, Severity.ERROR,
    'break statements must be inside a do/while/for or switch statement.')
ERROR_CANNOT_SWIZZLE_PRIMITIVE_TYPE = ErrorMessage (2003, Severity.ERROR,
    '''Cannot swizzle primitive type which is not scalar/vector.''')
ERROR_CANNOT_SWIZZLE_TYPE = ErrorMessage (2004, Severity.ERROR,
    '''Type '{0}' does not support swizzle.''')