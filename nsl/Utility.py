def ContainsAnyOf(iterable, what):
    search = set(what)
    for i in iterable:
        if i in search:
            return True
    return False
