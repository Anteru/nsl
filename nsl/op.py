from enum import Enum

class Operation(Enum):
    ASSIGN = 1

    # Binary
    ADD = 102
    SUB = 103
    MUL = 104
    DIV = 105
    MOD = 106

    # Unary
    UA_ADD = 120
    UA_SUB = 121

    # comparison
    CMP_GT = 200
    CMP_LT = 201
    CMP_LE = 202
    CMP_GE = 203
    CMP_NE = 204
    CMP_EQ = 205

    # logic
    LG_OR   = 300
    LG_AND  = 301
    LG_NOT  = 302

    BIT_OR  = 400
    BIT_AND = 401
    BIT_XOR = 402
    BIT_NOT = 403

def IsComparison(op):
    return op.value > 200 and op.value < 210

_op_str_map = {
    '='   : Operation.ASSIGN,

    '+'   : Operation.ADD,
    '-'   : Operation.SUB,
    '/'   : Operation.DIV,
    '*'   : Operation.MUL,
    '%'   : Operation.MOD,

    '++'  : Operation.UA_ADD,
    '--'  : Operation.UA_SUB,

    '&&'  : Operation.LG_AND,
    '||'  : Operation.LG_OR,
    '!'   : Operation.LG_NOT,

    '>'   : Operation.CMP_GT,
    '<'   : Operation.CMP_LT,
    '>='  : Operation.CMP_GE,
    '<='  : Operation.CMP_LE,
    '=='  : Operation.CMP_EQ,
    '!='  : Operation.CMP_NE,

    '|'   : Operation.BIT_OR,
    '&'   : Operation.BIT_AND,
    '~'   : Operation.BIT_NOT,
    '^'   : Operation.BIT_XOR
}

_str_op_map = {v : k for (k, v) in _op_str_map.items ()}

def StrToOp(op):
    assert op in _op_str_map, "Unknown operation: '{}".format (op)
    return _op_str_map [op]

def OpToStr(s):
    assert s in _str_op_map, "Unknown operation ID: '{}'".format (s)
    return _str_op_map [s]
