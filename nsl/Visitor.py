import collections

class InvalidNodeType(Exception):
    def __init__(self, actualType):
        self.actualType = actualType
        
class Node:
    def _Traverse(self, function):
        pass
    
    def ForEachChild(self, f, ctx=None):
        def Function(n):
            if not isinstance (n, Node):
                raise InvalidNodeType (type(n))

            r = f(n, ctx)
            return r if r is not None else n

        def _ProcessSequence(s):
            return [Function(e) for e in s]

        def _ProcessSet(s):
            return {Function(e) for e in s}
        
        def _ProcessMapping(m):
            r = collections.OrderedDict()
            for k,v in m.items():
                r[k] = Function (v)
            return r

        def Wrapper(e):
            if e is None:
                return None

            if isinstance(e, collections.abc.Sequence):
                return _ProcessSequence(e)
            elif isinstance(e, collections.abc.Set):
                return _ProcessSet(e)
            elif isinstance(e, collections.abc.Mapping):
                return _ProcessMapping(e)
            else:
                if not isinstance (e, Node):
                    raise InvalidNodeType (type(e))
                
                return Function (e)

        self._Traverse(Wrapper)

    def AcceptVisitor(self, visitor, ctx=None):
        '''Traverse all children of this node.

        By default, this calls `_Traverse` to process all children'''
        def Visit(c, ctx):
            return visitor.v_Generic (c, ctx)
        self.ForEachChild (Visit, ctx)

class Visitor:
    def __init__(self):
        from nsl.Errors import NullErrorHandler
        self.errorHandler = NullErrorHandler()
    
    def SetErrorHandler (self, errorHandler):
        self.errorHandler = errorHandler
        
    def SetOutput(self, output):
        self.output = output
        
    def Print(self, *args, end='\n'):
        print (*args, end=end, file=self.output)

    def v_Generic (self, obj, ctx=None):
        '''The default visitation function.
        
        As Python doesn't support function overloading per type, this simulates
        the resolve that would happen by obtaining a list of all parent classes
        of the object. For each class, a function v_ClassName is called. This
        makes it possible for instance to have a generic handler for all
        ``Expression`` classes yet keep an overload for ``BinaryExpression``.'''
        import inspect
        
        # This includes the class itself
        baseClasses = list (inspect.getmro (obj.__class__))
        
        for baseClass in baseClasses:
            if baseClass is object:
                break
            
            fname = 'v_{}'.format (baseClass.__name__)
            
            if hasattr(self, fname):
                func = getattr (self, fname)
                return func (obj, ctx)
        
        return self.v_Default (obj, ctx)

    def v_Default(self, obj, ctx):
        print ('Missing visit method: "{}.v_{}"'.format (
            self.__class__.__name__,
            obj.__class__.__name__))
        return None

    def GetContext (self):
        return None

    def v_Visit (self, obj, ctx=None):
        return self.v_Generic (obj, ctx)

    def Visit(self, root):
        return self.v_Generic (root, self.GetContext ())

class DefaultVisitor(Visitor):
    def __init__(self):
        super().__init__()
    
    def v_Default(self, obj, ctx=None):
        '''Traverse further if possible.'''
        super().__init__()
        if hasattr (obj, 'AcceptVisitor'):
            return obj.AcceptVisitor (self, ctx)