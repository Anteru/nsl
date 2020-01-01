import ply.yacc
from nsl import ast, types, lexer, op
from enum import Enum

class ParseEntryPoint(Enum):
    Module = 'module'
    Expression = 'expression'
    Statement = 'statement'

class NslParser:
    def __init__(self, parseEntryPoint = ParseEntryPoint.Module):
        self.lexer = lexer.NslLexer ()
        self.lexer.Build()
        self.tokens = self.lexer.tokens
        
        self.parser = ply.yacc.yacc(module=self, 
            start=parseEntryPoint.value
            # errorlog=ply.yacc.NullLogger())
        )

    def __GetLocation(self, p, which):
        return ast.Location(
            (p.lexpos (which),
            p.lexpos (which) + len (p[which]),),
            self.__sourceMapping)

    def Parse(self, text, **kwargs):
        self.lexer.reset_lineno()
        self.__sourceMapping = ast.SourceMapping (text)
        return self.parser.parse(text, lexer=self.lexer, **kwargs)

    precedence = (
        ('left', 'LOR'),
        ('left', 'LAND'),
        ('left', 'OR'),
        ('left', 'XOR'),
        ('left', 'AND'),
        ('left', 'EQ', 'NE'),
        ('left', 'GT', 'GE', 'LT', 'LE'),
        ('left', 'RSHIFT', 'LSHIFT'),
        ('left', 'PLUS', 'MINUS'),
        ('left', 'TIMES', 'DIVIDE', 'MOD')
    )

    def p_module_1(self, p):
        '''module : function'''
        p[0] = ast.Module ()
        p[0].AddFunction (p[1])

    def p_module_2(self, p):
        '''module : module function'''
        p[0] = p[1]
        p[0].AddFunction (p[2])

    def p_module_3(self, p):
        '''module : declaration_statement'''
        p[0] = ast.Module ()
        p[0].AddDeclaration (p[1])

    def p_module_4(self, p):
        '''module : module declaration_statement'''
        p[0] = p[1]
        p[0].AddDeclaration (p[2])

    def p_module_5(self, p):
        '''module : type_definition'''
        p[0] = ast.Module ()
        p[0].AddType (p[1])

    def p_module_6(self, p):
        '''module : module type_definition'''
        p[0] = p[1]
        p[0].AddType (p[2])

    def p_module_7(self, p):
        '''module : import_statement'''
        p[0] = ast.Module()
        p[0].AddImport(p[1])

    def p_module_8(self, p):
        '''module : module import_statement'''
        p[0] = p[1]
        p[0].AddImport(p[1])

    def p_string_literal(self, p):
        '''string_literal : STRING_LITERAL'''
        p[0] = p[1][1:-1]

    def p_import_statement(self, p):
        '''import_statement : IMPORT string_literal ';' '''
        p[0] = p[2]

    def p_type_definition(self, p):
        '''type_definition : structure_definition'''
        p[0] = p[1]

    def p_argument_1(self, p):
        '''argument : arg_mod_opt type ID array_size_declaration_list'''
        if not p[4]:
            arraySize = None
        
        if p[1]:
            p[1] = set(p[1])
        else:
            p[1] = set()

        p[0] = ast.Argument (p[2], p[3], modifiers=p[1], arraySize = p[4])
        p[0].SetLocation(self.__GetLocation(p, 3))

    def p_argument_2(self, p):
        '''argument : arg_mod_opt type array_size_declaration_list'''
        if not p[3]:
            arraySize = None
        
        if p[1]:
            p[1] = set(p[1])
        else:
            p[1] = set()

        p[0] = ast.Argument (p[2], modifiers=p[1], arraySize = p[3])

    def p_arg_mod (self, p):
        '''arg_mod : __OPTIONAL'''
        p[0] = ast.ArgumentModifier.Optional

    def p_arg_mod_opt (self, p):
        '''arg_mod_opt : arg_mod
                       | empty'''
        p[0] = p[1]

    def p_arg_list_1(self, p):
        '''arg_list : arg_list ',' argument'''
        p[1].append (p[3])
        p[0] = p[1]

    def p_arg_list_2(self, p):
        '''arg_list : argument'''
        p[0] = [p[1]]

    def p_arg_list_opt_1(self, p):
        '''arg_list_opt : arg_list'''
        p[0] = p[1]

    def p_arg_list_opt_2(self, p):
        '''arg_list_opt : empty'''
        p[0] = list()

    def p_empty(self, p):
        '''empty :'''
        pass

    def p_expression(self, p):
        '''expression : unary_expression
        | binary_expression
        | assignment_expression'''
        p[0] = p[1]

    def p_expression_list_1(self, p):
        '''expression_list : expression '''
        p[0] = [p[1]]

    def p_expression_list_2(self, p):
        '''expression_list : expression_list ',' expression'''
        p[0] = p[1]
        p[0].append(p[3])

    def p_expression_list_opt_1(self, p):
        '''expression_list_opt : expression_list'''
        p[0] = p[1]

    def p_expression_list_opt_2(self, p):
        '''expression_list_opt : empty'''
        p[0] = []

    def p_constant_integer_expression_1(self, p):
        '''constant_integer_expression : INT_CONST_DEC'''
        p[0] = ast.LiteralExpression(int(p[1]), types.Integer())
        p[0].SetLocation (self.__GetLocation (p, 1))

    def p_constant_integer_expression_2(self, p):
        '''constant_integer_expression : INT_CONST_OCT'''
        p[0] = ast.LiteralExpression(int(p[1], 8), types.Integer())
        p[0].SetLocation (self.__GetLocation (p, 1))

    def p_constant_integer_expression_3(self, p):
        '''constant_integer_expression : INT_CONST_HEX'''
        # First two characters are 0x or 0X, so we have to skip them
        p[0] = ast.LiteralExpression(int(p[1][2:], 16), types.Integer())
        p[0].SetLocation (self.__GetLocation (p, 1))

    def _ParseFloat (self, value):
        if value.endswith ('f'):
            return float(value[:-1])
        else:
            return float(value)

    def p_constant_float_expression(self, p):
        '''constant_float_expression : FLOAT_CONST'''
        p[0] = ast.LiteralExpression(self._ParseFloat(p[1]), types.Float ())
        p[0].SetLocation (self.__GetLocation (p, 1))

    def p_unary_expression_1(self, p):
        '''unary_expression : ID'''
        p[0] = ast.PrimaryExpression (p[1])
        p[0].SetLocation (self.__GetLocation (p, 1))

    def p_unary_expression_2(self, p):
        '''unary_expression : constant_integer_expression
        | constant_float_expression
        | array_expression
        | member_access_expression
        | function_call_expression
        | construct_primitive_expression'''
        p[0] = p[1]

    def p_unary_expression_3(self, p):
        '''unary_expression : PLUSPLUS ID
                            | MINUSMINUS ID'''
                            
        p[2] = ast.PrimaryExpression (p[2])
        p[2].SetLocation (self.__GetLocation (p, 1))
        if p[1] == '++':
            p[0] = ast.AffixExpression(op.Operation.ADD, p[2], ast.Affix.PRE)
        elif p[1] == '--':
            p[0] = ast.AffixExpression(op.Operation.SUB, p[2], ast.Affix.PRE)

    def p_unary_expression_4(self, p):
        '''unary_expression : ID PLUSPLUS
                            | ID MINUSMINUS'''

        p[1] = ast.PrimaryExpression (p[1])
        p[1].SetLocation (self.__GetLocation (p, 2))
        if p[2] == '++':
            p[0] = ast.AffixExpression(op.Operation.ADD, p[1], ast.Affix.POST)
        elif p[2] == '--':
            p[0] = ast.AffixExpression(op.Operation.SUB, p[1], ast.Affix.POST)

    def p_expression_opt_1(self, p):
        '''expression_opt : expression'''
        p[0] = p[1]

    def p_expression_opt_2(self, p):
        '''expression_opt : empty'''
        p[0] = ast.EmptyExpression ()

    def p_function_call_expression(self, p):
        '''function_call_expression : ID '(' expression_list_opt ')' '''
        p[0] = ast.CallExpression (types.UnresolvedType (p[1]), p[3])

    def p_binary_expression(self, p):
        '''binary_expression : expression bin_op expression
            | '(' expression bin_op expression ')' '''
        if (len(p) == 4):
            p[0] = ast.BinaryExpression (op.StrToOp (p[2]), p[1], p[3])
        else:
            p[0] = ast.BinaryExpression (op.StrToOp (p[3]), p[2], p[4])

    def p_bin_op(self, p):
        '''bin_op : LT
        | GT
        | PLUS
        | MINUS
        | TIMES
        | DIVIDE
        | MOD
        | GE
        | LE
        | EQ
        | NE
        | LAND
        | LOR'''
        p[0] = p[1]

    def p_access_expression(self, p):
        '''access_expression : array_expression
        | member_access_expression'''
        p[0] = p[1]

    def p_array_expression_1(self, p):
        '''array_expression : ID '[' expression ']' '''
        pe = ast.PrimaryExpression(p[1])
        pe.SetLocation(self.__GetLocation(p, 1))
        p[0] = ast.ArrayExpression (pe, p[3])

    def p_array_expression_2(self, p):
        '''array_expression : access_expression '[' expression ']' '''
        p[0] = ast.ArrayExpression (p[1], p[3])

    def p_member_access_expression_1(self, p):
        '''member_access_expression : ID '.' ID '''
        parent = ast.PrimaryExpression (p[1])
        parent.SetLocation(self.__GetLocation(p, 1))
        member = ast.PrimaryExpression (p[3])
        member.SetLocation(self.__GetLocation(p, 3))
        
        p[0] = ast.MemberAccessExpression (parent, member)

    def p_member_access_expression_2(self, p):
        '''member_access_expression : access_expression '.' ID '''
        member = ast.PrimaryExpression (p[3])
        member.SetLocation(self.__GetLocation(p, 3))
        
        p[0] = ast.MemberAccessExpression (p[1], member)

    def p_assignment_op(self, p):
        '''assignment_op : EQUALS 
        | PLUSEQUAL 
        | MINUSEQUAL 
        | DIVEQUAL 
        | TIMESEQUAL'''
        p[0] = op.StrToOp (p[1])

    def p_assignment_expression(self, p):
        '''assignment_expression : unary_expression assignment_op expression'''
        p[0] = ast.AssignmentExpression (p[1], p[3], operation=p[2])

    def p_function_attr(self, p):
        '''function_attr : EXPORT'''
        if p[1] == 'export':
            p[0] = { 'isExported': True }

    def p_function_attr_opt_1(self, p):
        '''function_attr_opt : function_attr'''
        p[0] = p[1]

    def p_function_attr_opt_2(self, p):
        '''function_attr_opt : empty'''
        p[0] = {}

    def p_function_decl(self, p):
        '''function_decl : function_attr_opt FUNCTION ID '(' arg_list_opt ')' RARROW type'''
        p[0] = {
            'name' : p[3],
            'args' : p[5],
            'return-type' : p[8],
            'attributes': p[1]
        }

    def p_function_1(self, p):
        '''function : function_decl compound_statement '''
        p[0] = ast.Function (p[1]['name'], p[1]['args'], p[1]['return-type'], p[2],
                             **p[1]['attributes'])

    def p_function_2(self, p):
        '''function : function_decl ';' '''
        p[0] = ast.Function (p[1]['name'], p[1]['args'], p[1]['return-type'],
                             **p[1]['attributes'])

    def p_annotation(self, p):
        '''annotation : '[' ID ']' '''
        p[0] = ast.Annotation (p[2])
        
    def p_annotation_list_1(self, p):
        '''annotation_list : annotation_list annotation'''
        p[1].append (p[2])
        p[0] = p[1]

    def p_annotation_list_2(self, p):
        '''annotation_list : empty'''
        p[0] = []

    def p_structure_definition(self, p):
        '''structure_definition : annotation_list STRUCT ID '{' var_decl_list_opt '}' '''
        p[0] = ast.StructureDefinition(p[3], p[5])

        for annotation in p[1]:
            p[0].AddAnnotation (annotation)
    
    def p_var_decl_list_1(self, p):
        '''var_decl_list : var_decl ';' '''
        p[0] = [p[1]]

    def p_var_decl_list_2(self, p):
        '''var_decl_list : var_decl_list var_decl ';' '''
        p[0] = p[1]
        p[0].append (p[2])

    def p_var_decl_list_opt_1(self, p):
        '''var_decl_list_opt : empty'''
        p[0] = list ()

    def p_var_decl_list_opt_2(self, p):
        '''var_decl_list_opt : var_decl_list'''
        p[0] = p[1]

    def p_statement_list_1(self, p):
        '''statement_list : statement_list statement'''
        p[1].append (p[2])
        p[0] = p[1]

    def p_statement_list_2(self, p):
        '''statement_list : statement'''
        p[0] = [p[1]]

    def p_statement_list_opt_1(self, p):
        '''statement_list_opt : statement_list'''
        p[0] = p[1]

    def p_statement_list_opt_2(self, p):
        '''statement_list_opt : empty'''
        p[0] = list()

    def p_statement(self, p):
        '''statement : declaration_statement
        | expression_statement
        | compound_statement
        | return_statement
        | selection_statement
        | iteration_statement'''
        p[0] = p[1]

    def p_statement_opt_1(self, p):
        '''statement_opt : statement'''
        p[0] = p[1]

    def p_statement_opt_2(self, p):
        '''statement_opt : ';' '''
        p[0] = ast.EmptyStatement ()

    def p_declaration_statement(self, p):
        '''declaration_statement : var_decl ';' '''
        p[0] = ast.DeclarationStatement ([p[1]])

    def p_expression_statement(self, p):
        ''' expression_statement : expression ';' '''
        p[0] = ast.ExpressionStatement(p[1])

    def p_selection_statement_1(self, p):
        """ selection_statement : IF '(' expression ')' statement """
        p[0] = ast.IfStatement (p[3], p[5])

    def p_selection_statement_2(self, p):
        """ selection_statement : IF '(' expression ')' statement ELSE statement """
        p[0] = ast.IfStatement (p[3], p[5], p[7])

    def p_iteration_statement_1(self, p):
        '''iteration_statement : FOR '(' var_decl_opt ';' expression_opt ';' expression_opt ')' statement'''
        p[0] = ast.ForStatement (p[3], p[5], p[7], p[9])

    def p_iteration_statement_2(self, p):
        '''iteration_statement : WHILE '(' expression ')' statement_opt'''
        p[0] = ast.WhileStatement (p[3], p[5])

    def p_iteration_statement_3(self, p):
        '''iteration_statement : DO compound_statement WHILE '(' expression ')' '''
        p[0] = ast.DoStatement (p[5], p[2])

    def p_iteration_statement_4(self, p):
        '''iteration_statement : CONTINUE ';' '''
        p[0] = ast.ContinueStatement ()

    def p_iteration_statement_5(self, p):
        '''iteration_statement : BREAK ';' '''
        p[0] = ast.BreakStatement ()

    def p_compound_statement(self, p):
        '''compound_statement : '{' statement_list_opt '}' '''
        p[0] = ast.CompoundStatement (p[2])

    def p_return_statement(self, p):
        '''return_statement : RETURN expression ';' '''
        p[0] = ast.ReturnStatement(p[2])

    def p_var_decl_1(self, p):
        '''var_decl : type ID array_size_declaration_list semantic_decl_opt'''

        if p[3]:
            p[0] = ast.VariableDeclaration (p[1], p[2], p[4], arraySize=p[3])
        else:
            p[0] = ast.VariableDeclaration (p[1], p[2], p[4])

        p[0].SetLocation(self.__GetLocation(p, 2))

    def p_var_decl_2(self, p):
        '''var_decl : type ID EQUALS expression'''
        p[0] = ast.VariableDeclaration(p[1], p[2], None, p[4])
        p[0].SetLocation(self.__GetLocation(p, 2))

    def p_array_size_declaration (self, p):
        '''array_size_declaration : '[' constant_integer_expression ']' '''
        p[0] = p[2].GetValue ()
        
    def p_array_size_declaration_list_1(self, p):
        '''array_size_declaration_list : array_size_declaration_list array_size_declaration'''
        p[1].append (p[2])
        p[0] = p[1]

    def p_array_size_declaration_list_2(self, p):
        '''array_size_declaration_list : empty'''
        p[0] = []

    def p_var_decl_opt_1(self, p):
        '''var_decl_opt : var_decl'''
        p[0] = p[1]

    def p_var_decl_opt_2(self, p):
        '''var_decl_opt : empty'''
        p[0] = None

    def p_semantic_decl_1(self, p):
        '''semantic_decl : ':' ID'''
        p[0] = ast.Semantic (p[2])

    def p_semantic_decl_2(self, p):
        '''semantic_decl : ':' ID array_size_declaration '''
        p[0] = ast.Semantic (p[2], p[3])

    def p_semantic_decl_opt_1(self, p):
        ''' semantic_decl_opt : semantic_decl'''
        p[0] = p[1]

    def p_semantic_decl_opt_2(self, p):
        ''' semantic_decl_opt : empty'''
        p[0] = None

    def p_type_1(self, p):
        '''type : primitive_type'''
        p[0] = p[1]

    def p_type_2(self, p):
        '''type : ID'''
        p[0] = types.UnresolvedType (p[1])

    def p_type_3(self, p):
        '''type : VOID'''
        p[0] = types.Void()

    def p_primitive_type(self, p):
        '''primitive_type : FLOAT
            | FLOAT2
            | FLOAT3
            | FLOAT4
            | INT
            | INT2
            | INT3
            | INT4
            | UINT
            | UINT2
            | UINT3
            | UINT4
            | MATRIX3X3
            | MATRIX4X4
            | FLOAT3X3
            | FLOAT4X4'''
        p[0] = types.BuiltinTypeFactory(p[1])

    def p_construct_primitive_expression(self, p):
        '''construct_primitive_expression : primitive_type '(' expression_list ')' '''
        p[0] = ast.ConstructPrimitiveExpression (p[1], p[3])

    def p_error(self, t):
        import sys
        print("Syntax error at '%s'" % t.value)
        sys.exit (1)
