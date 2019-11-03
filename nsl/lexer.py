import ply.lex as lex
from ply.lex import TOKEN

class NslLexer:    
    t_ignore = ' \t'

    # valid C identifiers (K&R2: A.2.3)
    identifier = r'[a-zA-Z_][0-9a-zA-Z_]*'

    # integer constants (K&R2: A.2.5.1)
    integer_suffix_opt = r'(u?ll|U?LL|([uU][lL])|([lL][uU])|[uU]|[lL])?'
    decimal_constant = '(0'+integer_suffix_opt+')|([+-]?[1-9][0-9]*'+integer_suffix_opt+')'
    octal_constant = '0[0-7]*'+integer_suffix_opt
    hex_constant = '0[xX][0-9a-fA-F]+'+integer_suffix_opt

    # floating constants (K&R2: A.2.5.3)
    exponent_part = r"""([eE][-+]?[0-9]+)"""
    fractional_constant = r"""([0-9]*\.[0-9]+)|([0-9]+\.)"""
    floating_constant = '(((('+fractional_constant+')'+exponent_part+'?)|([0-9]+'+exponent_part+'))[FfLl]?)'

    literals = [';', '{', '}', '(', ')', '.' ,'[', ']', ',', ':']

    types = ['VOID',
             'FLOAT', 'FLOAT2', 'FLOAT3', 'FLOAT4',
             'INT', 'INT2', 'INT3', 'INT4',
             'UINT', 'UINT2', 'UINT3', 'UINT4',
             'MATRIX3X3', 'MATRIX4X4']
    reserved_tokens = ['FUNCTION',
                       'IF', 'ELSE', 'STRUCT', 'RETURN',
                       'FOR', 'CONTINUE', 'BREAK', 'SWITCH',
                       'DO', 'WHILE', 'CASE',

                       'EXPORT',

                       '__OPTIONAL',

                       # Reserve for future use
                       'TEMPLATE', 'CLASS', 'INTERFACE',

                       #
                       'CONST']
    tokens = types + reserved_tokens + ['ID',
            'INT_CONST_DEC', 'INT_CONST_OCT', 'INT_CONST_HEX',
            'FLOAT_CONST',

             # Operators
            'PLUS', 'MINUS', 'TIMES', 'DIVIDE', 'MOD',
            'OR', 'AND', 'NOT', 'XOR', 'LSHIFT', 'RSHIFT',
            'LOR', 'LAND', 'LNOT',
            'LT', 'LE', 'GT', 'GE', 'EQ', 'NE',

            # Assignment
            'EQUALS', 'TIMESEQUAL', 'DIVEQUAL', 'MODEQUAL',
            'PLUSEQUAL', 'MINUSEQUAL',
            'LSHIFTEQUAL','RSHIFTEQUAL', 'ANDEQUAL', 'XOREQUAL',
            'OREQUAL',

            # Increment/decrement
            'PLUSPLUS', 'MINUSMINUS',

            # Conditional operator (?)
            'CONDOP',

            'RARROW']

    t_RARROW = r'->'
    keywords = {t.lower () : t for t in (types+reserved_tokens)}

    def t_newline(self, t):
        r'\n+'
        t.lexer.lineno += t.value.count('\n')

    def t_error(self, t):
        print("Illegal character '%s'" % t.value[0])
        t.lexer.skip(1)

    @TOKEN(identifier)
    def t_ID(self, t):
        if t.value in self.keywords:
            t.type = self.keywords [t.value]

        return t

    @TOKEN(floating_constant)
    def t_FLOAT_CONST(self, t):
        return t

    @TOKEN(hex_constant)
    def t_INT_CONST_HEX(self, t):
        return t

    @TOKEN(octal_constant)
    def t_INT_CONST_OCT(self, t):
        return t

    @TOKEN(decimal_constant)
    def t_INT_CONST_DEC(self, t):
        return t

    # Operators
    t_PLUS              = r'\+'
    t_MINUS             = r'-'
    t_TIMES             = r'\*'
    t_DIVIDE            = r'/'
    t_MOD               = r'%'
    t_OR                = r'\|'
    t_AND               = r'&'
    t_NOT               = r'~'
    t_XOR               = r'\^'
    t_LSHIFT            = r'<<'
    t_RSHIFT            = r'>>'
    t_LOR               = r'\|\|'
    t_LAND              = r'&&'
    t_LNOT              = r'!'
    t_LT                = r'<'
    t_GT                = r'>'
    t_LE                = r'<='
    t_GE                = r'>='
    t_EQ                = r'=='
    t_NE                = r'!='

    # Assignment operators
    t_EQUALS            = r'='
    t_TIMESEQUAL        = r'\*='
    t_DIVEQUAL          = r'/='
    t_MODEQUAL          = r'%='
    t_PLUSEQUAL         = r'\+='
    t_MINUSEQUAL        = r'-='
    t_LSHIFTEQUAL       = r'<<='
    t_RSHIFTEQUAL       = r'>>='
    t_ANDEQUAL          = r'&='
    t_OREQUAL           = r'\|='
    t_XOREQUAL          = r'\^='

    # Increment/decrement
    t_PLUSPLUS          = r'\+\+'
    t_MINUSMINUS        = r'--'

    def Build(self, **kwargs):
        self.lexer = lex.lex(module=self,**kwargs)

    def reset_lineno(self):
        """ Resets the internal line number counter of the lexer."""
        self.lexer.lineno = 1

    def input(self, text):
        self.lexer.input(text)

    def token(self):
        g = self.lexer.token()
        return g
