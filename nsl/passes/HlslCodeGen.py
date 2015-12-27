from nsl import ast, op

class HlslVisitor(ast.DefaultVisitor):
	class Context:
		def __init__(self, printFunc):
			self.__level = 0
			self.__semanticType = [False]
			self.__printFunc = printFunc
			
		def Print (self, v=''):
			self.__printFunc ('\t'*self.__level + v)
			
		def In (self,semanticType=False):
			self.__level += 1
			self.__semanticType.append (semanticType)
			
		def Out (self):
			self.__level -= 1
			self.__semanticType.pop ()

		def IsSemanticType(self):
			'''True if this is a type that uses semantics. Typically a
			In/Out structure.'''
			return self.__semanticType[-1]
			
	def __ConvertSemantic (self, semantic):
		if semantic.Get () == ast.BuiltinSemantic.ColorOutput:
			return 'SV_Target[{}]'.format (semantic.GetSlot ())
		elif semantic.Get () == ast.BuiltinSemantic.Position:
			return 'SV_Position'
		
	def __ConvertType (self, nslType):
		if nslType.IsPrimitive ():
			return str(nslType)
		else:
			return str(nslType.GetName())
	
	def GetContext(self):
		return self.Context (self.Print)
		
	def __FormatArgumentList(self, args):
		return ', '.join(['{0} {1}'.format(arg.GetType().GetName(), arg.GetName()) for arg in args])
	
	def __v_FunctionOrShaderBody(self, funcOrShader, ctx):
		ctx.Print ('{')
		ctx.In ()
		funcOrShader.GetBody().AcceptVisitor(self, ctx)
		ctx.Out ()
		ctx.Print ('}')
		ctx.Print ()
		
	def v_Function(self, func, ctx):
		ctx.Print ('{2} {0} ({1})'.format(func.GetName (),
									  self.__FormatArgumentList(func.GetArguments()),
									  func.GetType().GetReturnType().GetName ()))
		self.__v_FunctionOrShaderBody(func, ctx)

	def v_Shader(self, shd, ctx=None):
		ctx.Print ('{2} {0} ({1})'.format(shd.GetName(),
									  self.__FormatArgumentList(shd.GetArguments()),
									  shd.GetType().GetReturnType ().GetName()))
		self.__v_FunctionOrShaderBody(shd, ctx)
	
	def v_StructureDefinition(self, decl, ctx):
		ctx.Print ('struct {0}'.format (decl.GetName ()))
		ctx.Print ('{')
		containsSemantics = False
		for annotation in decl.GetAnnotations ():
			if annotation.GetValue () == 'InOut':
				containsSemantics = True
				break
		ctx.In (containsSemantics)
		decl.AcceptVisitor(self, ctx)
		ctx.Out ()
		ctx.Print ('}')
		ctx.Print ()
		
	def v_VariableDeclaration(self, decl, ctx):
		elementType = decl.GetType ()
		if elementType.IsArray ():
			elementType = elementType.GetElementType ()
			arraySizeDecl = ''.join (['[{}]'.format (s) for s in decl.GetType().GetSize()])
		else:
			arraySizeDecl = ''

		if decl.HasSemantic () or ctx.IsSemanticType ():
			if decl.HasSemantic ():
				semantic = self.__ConvertSemantic (decl.GetSemantic ())
			else:
				semantic = decl.GetName ().upper ()

			ctx.Print ('{} {}{} : {};'.format (self.__ConvertType(elementType), 
											 decl.GetName (),
											 arraySizeDecl,
											 semantic))
		else:
			ctx.Print ('{} {}{};'.format (self.__ConvertType(elementType),
										decl.GetName (),
										arraySizeDecl))

	def v_CompoundStatement(self, cs, ctx):
		ctx.Print ('{')
		ctx.In ()
		for s in cs.GetStatements():
			s.AcceptVisitor(self, ctx+1)
		ctx.Out ()
		ctx.Print ('}')
		
	def v_ConstructPrimitiveExpression(self, expr, ctx):
		return '{}({})'.format (self.__ConvertType(expr.GetType ()), ', '.join ([self.v_Visit(e) for e in expr]))
		
	def v_Expression(self,expr, ctx):
		return str(expr)
		
	def v_CastExpression(self, ce, ctx):
		if ce.IsImplicit():
			return self.v_Visit (ce.GetArgument ())
		else:
			return ('({})({})'.format (self.__ConvertType(ce.GetType ()),
									   ce.AcceptVisitor(self, ctx)))
			
	def v_BinaryExpression(self, be, ctx):
		return '{} {} {}'.format (self.v_Visit (be.GetLeft ()),
									op.OpToStr (be.GetOperation ()),
									self.v_Visit (be.GetRight ()))
		
	def v_ExpressionStatement(self, es, ctx):
		ctx.Print (self.v_Visit (es.GetExpression (), ctx) + ';')
					
	def v_ReturnStatement (self, stmt, ctx):
		ctx.Print ('return {0};'.format(str(stmt.GetExpression())))
		
def GetPass():
	import nsl.Pass
	return nsl.Pass.MakePassFromVisitor(HlslVisitor (), 'hlsl-code-gen')