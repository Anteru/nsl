import ply.yacc
from nsl import ast, types, lexer, op

class NslParser:
    def __init__(self):
        self.lexer = lexer.NslLexer ()
        self.lexer.Build()
        self.tokens = self.lexer.tokens
        self.parser = ply.yacc.yacc(module=self, start='program',errorlog=ply.yacc.NullLogger())

    def __GetLocation(self, p, which):
        return ast.Location(
            p.lineno (which),
            (p.lexpos (which), len (p[which]),))

    def Parse(self, text, **kwargs):
        self.lexer.reset_lineno()
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

    def p_program_1(self, p):
        '''program : shader_or_function'''
        p[0] = ast.Program ()
        p[0].AddFunction (p[1])

    def p_program_2(self, p):
        '''program : declaration_statement'''
        p[0] = ast.Program ()
        p[0].AddDeclaration (p [1])

    def p_program_3(self, p):
        '''program : program declaration_statement'''
        p [0] = p [1]
        p[0].AddDeclaration (p [2])

    def p_program_4(self, p):
        '''program : type_definition'''
        p[0] = ast.Program ()
        p[0].AddType (p[1])

    def p_program_5(self, p):
        '''program : program shader_or_function'''
        p[0] = p[1]
        p[0].AddFunction (p[2])

    def p_program_6(self, p):
        '''program : program type_definition'''
        p[0] = p[1]
        p[0].AddType (p[2])

    def p_shader_or_function(self, p):
        '''shader_or_function : shader
        | function'''
        p[0] = p[1]

    def p_type_definition(self, p):
        '''type_definition : structure_definition
        | interface_definition'''
        p[0] = p[1]

    def p_argument_1(self, p):
        '''argument : type ID'''
        p [0] = ast.Argument (p [1], p[2])

    def p_argument_2(self, p):
        '''argument : type'''
        p [0] = ast.Argument (p[1])

    def p_argument_3 (self, p):
        '''argument : arg_mod type ID'''
        p [0] = ast.Argument (p[2], p[3], modifiers = set ([p[1]]))

    def p_argument_4 (self, p):
        '''argument : arg_mod type'''
        p [0] = ast.Argument (p[2], modifiers = set ([p[1]]))

    def p_arg_mod (self, p):
        '''arg_mod : __OPTIONAL'''
        p [0] = ast.ArgumentModifier.Optional

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

    def p_expression_opt_1(self, p):
        '''expression_opt : expression'''
        p[0] = p[1]

    def p_expression_opt_2(self, p):
        '''expression_opt : empty'''
        p[0] = ast.EmptyExpression ()

    def p_function_call_expression_1(self, p):
        '''function_call_expression : ID '(' expression_list_opt ')' '''
        p[0] = ast.CallExpression (types.UnresolvedType (p[1]), p[3])

    def p_function_call_expression_2(self, p):
        '''function_call_expression : member_access_expression '(' expression_list_opt ')' '''
        p[0] = ast.MethodCallExpression (p[1], p[3])

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
        | NE'''
        p[0] = p[1]

    def p_access_expression(self, p):
        '''access_expression : array_expression
        | member_access_expression'''
        p[0] = p[1]

    def p_array_expression_1(self, p):
        '''array_expression : ID '[' expression ']' '''
        p [0] = ast.ArrayExpression (ast.PrimaryExpression(p[1]), p[3])

    def p_array_expression_2(self, p):
        '''array_expression : access_expression '[' expression ']' '''
        p [0] = ast.ArrayExpression (p[1], p[3])

    def p_member_access_expression_1(self, p):
        '''member_access_expression : ID '.' ID '''
        p [0] = ast.MemberAccessExpression (ast.PrimaryExpression (p[1]), ast.PrimaryExpression (p[3]))

    def p_member_access_expression_2(self, p):
        '''member_access_expression : access_expression '.' ID '''
        p [0] = ast.MemberAccessExpression (p[1], ast.PrimaryExpression (p[3]))

    def p_assignment_expression(self, p):
        '''assignment_expression : unary_expression EQUALS expression'''
        p [0] = ast.AssignmentExpression (p[1], p[3])

    def p_argument(self, p):
        '''argument : var_decl'''

    def p_shader_type(self, p):
        '''shader_type : PIXEL
        | VERTEX'''
        p[0] = {'vertex'    : ast.ShaderType.Vertex,
                'hull'      : ast.ShaderType.Hull,
                'domain'    : ast.ShaderType.Domain,
                'geometry'  : ast.ShaderType.Geometry,
                'pixel'     : ast.ShaderType.Pixel,
                'compute'   : ast.ShaderType.Pixel}[p[1]]

    def p_shader_decl(self, p):
        '''shader_decl : SHADER '(' shader_type ')' '(' arg_list_opt ')' RARROW type'''
        p[0] = { 'type' : p[3], 'args' : p[6], 'return-type' : p[9]}

    def p_function_decl(self, p):
        '''function_decl : FUNCTION ID '(' arg_list_opt ')' RARROW type'''
        p[0] = { 'name' : p[2], 'args' : p[4], 'return-type' : p[7] }

    def p_shader(self, p):
        '''shader : shader_decl compound_statement '''
        p[0] = ast.Shader (p[1]['type'], p[1]['return-type'], p[1]['args'], p[2])

    def p_function_1(self, p):
        '''function : function_decl compound_statement '''
        p[0] = ast.Function (p[1]['name'], p[1]['args'], p[1]['return-type'], p[2])

    def p_function_2(self, p):
        '''function : __DECLARATION function_decl ';' '''
        p[0] = ast.Function (p[2]['name'], p[2]['args'], p[2]['return-type'],
                             isForwardDeclaration = True)

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
    
    def p_interface_definition (self, p):
        '''interface_definition : annotation_list INTERFACE ID '{' function_list '}' '''
        p[0] = ast.InterfaceDefinition(p[3], p[5])

        for annotation in p[1]:
            p[0].AddAnnotation (annotation)

    def p_function_list_1 (self, p):
        '''function_list : empty'''
        p[0] = []

    def p_function_list_2 (self, p):
        '''function_list : function'''
        p[0] = [p[1]]

    def p_function_list_3 (self, p):
        '''function_list : function_list function'''
        p[0] = p[1] + [p[2]]

    def p_declaration (self, p):
        '''declaration : function_decl
        | var_decl'''
        p[0] = None

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
        p[0] = ast.ForStatement (p[3], p[5], p[7])

    def p_iteration_statement_2(self, p):
        '''iteration_statement : WHILE '(' expression ')' statement_opt'''
        p[0] = ast.WhileStatement (p[3], p[5])

    def p_iteration_statement_3(self, p):
        '''iteration_statement : DO compound_statement WHILE '(' expression ')' '''
        p[0] = ast.DoStatement (p[5], p[3])

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

    def p_var_decl(self, p):
        '''var_decl : type ID array_size_declaration_list semantic_decl_opt'''

        if p[3]:
            p[0] = ast.VariableDeclaration (p[1], p[2], p[4], arraySize=p[3])
        else:
            p[0] = ast.VariableDeclaration (p[1], p[2], p[4])

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
            | MATRIX4X4'''
        p[0] = types.BuiltinTypeFactory(p[1])

    def p_construct_primitive_expression(self, p):
        '''construct_primitive_expression : primitive_type '(' expression_list ')' '''
        p[0] = ast.ConstructPrimitiveExpression (p[1], p[3])

    def p_error(self, t):
        import sys
        print("Syntax error at '%s'" % t.value)
        sys.exit (1)
