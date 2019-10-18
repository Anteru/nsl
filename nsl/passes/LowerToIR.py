from nsl import ast, LinearIR, op

class LowerToIRVisitor(ast.DefaultVisitor):
	def __init__(self, ctx):
		self.__ctx = ctx
	
	class Context:
		def __init__(self):
			self.__program = LinearIR.Program()
			self.__functions = []
			self.__startNewBlock = False

		def OnEnterProgram(self):
			pass

		def OnLeaveProgram(self):
			ip = LinearIR.InstructionPrinter()
			for function in self.__functions:
				ip.Print(function)

		def OnEnterFunction(self, name, functionType):
			self.__function = self.__program.CreateFunction(name, functionType)
			self.__startNewBlock = True

		def OnLeaveFunction(self):
			self.__functions.append(self.__function)
			self.__function = None

		@property
		def Function(self):
			return self.__function

		@property
		def BasicBlock(self):
			if self.__startNewBlock:
				self.__function.CreateBasicBlock()
				self.__startNewBlock = False
			return self.__function.BasicBlocks[-1]

		def CreateBasicBlock(self):
			self.__startNewBlock = False
			return self.__function.CreateBasicBlock()

		def EndBasicBlock(self):
			self.__startNewBlock = True
	
	def GetContext(self):
		return self.Context ()
		
	def __FormatArgumentList(self, args):
		return ', '.join(['{0} {1}'.format(arg.GetType().GetName(), arg.GetName()) for arg in args])
	
	def __v_FunctionOrShaderBody(self, funcOrShader, ctx):
		ctx.OnEnterFunction(funcOrShader.GetName(), funcOrShader.GetType())
		funcOrShader.GetBody().AcceptVisitor(self, ctx)
		ctx.OnLeaveFunction()
		
	def v_Function(self, func, ctx):
		self.__v_FunctionOrShaderBody(func, ctx)

	def v_Shader(self, shd, ctx=None):
		self.__v_FunctionOrShaderBody(shd, ctx)

	def v_CompoundStatement(self, cs, ctx):
		for s in cs.GetStatements():
			self.v_Visit(s, ctx)

	def v_AffixExpression(self, expr, ctx):
		constOne = LinearIR.ConstantValue(expr.GetType(), 1)
		ctx.Function.RegisterValue(constOne)
		initialValue = self.v_Visit(expr.GetExpression(), ctx)
		returnValue = None

		if expr.IsPostfix():
			# Return the value before
			if expr.GetOperation() == op.Operation.ADD:
				# increment, save again
				addInstruction = LinearIR.BinaryInstruction(
					LinearIR.OpCode.ADD,
					expr.GetType (),
					initialValue,
				)
				ctx.BasicBlock.AddInstruction(addInstruction)
				returnValue = initialValue
		elif expr.IsPrefix():
			# Return the value after 
			if expr.GetOperation() == op.Operation.ADD:
				# increment, save again
				addInstruction = LinearIR.BinaryInstruction(
					LinearIR.OpCode.ADD,
					expr.GetType (),
					initialValue,
					constOne
				)
				returnValue = ctx.BasicBlock.AddInstruction(addInstruction)
		destination = self.v_Visit(expr.GetExpression(), ctx)
		destination.SetStore(addInstruction)
		return initialValue


	def v_ForStatement(self, expr, ctx):
		init = self.v_Visit(expr.GetInitialization (), ctx)
		# We translate a loop as following
		# condition: evaluate condition
		# conditional branch exit
		# evaluate body
		# evaluate next
		# unconditional branch condition
		# exit block
		condBB = ctx.CreateBasicBlock ()
		cond = self.v_Visit(expr.GetCondition(), ctx)
		condBranch = LinearIR.BranchInstruction(None, None, cond)
		ctx.BasicBlock.AddInstruction(condBranch)
		bodyBB = ctx.CreateBasicBlock ()
		self.v_Visit(expr.GetBody(), ctx)
		self.v_Visit(expr.GetNext (), ctx)
		backBranch = LinearIR.BranchInstruction(condBB)
		ctx.BasicBlock.AddInstruction(backBranch)
		exitBB = ctx.CreateBasicBlock ()
		condBranch.SetTrueBlock(bodyBB)
		condBranch.SetFalseBlock(exitBB)

	def v_IfStatement(self, expr, ctx):
		# Evaluate condition
		condition = self.v_Visit(expr.GetCondition(), ctx)

		# Insert branch, targets will be set later
		branch = LinearIR.BranchInstruction(None, None, condition)
		ctx.BasicBlock.AddInstruction(branch)

		# Process the "true" side
		trueBlock = ctx.CreateBasicBlock()
		self.v_Visit(expr.GetTruePath(), ctx)
		branch.SetTrueBlock(trueBlock)

		# Add branch to exit
		if expr.HasElsePath():
			exitBranch = LinearIR.BranchInstruction(None)
			ctx.BasicBlock.AddInstruction(exitBranch)
			falseBlock = ctx.CreateBasicBlock()
			self.v_Visit(expr.GetElsePath(), ctx)
			branch.SetFalseBlock(falseBlock)
		bb = ctx.CreateBasicBlock()

		if expr.HasElsePath():
			exitBranch.SetTrueBlock(bb)
		else:
			branch.SetFalseBlock(bb)
		return bb
		
	def v_ConstructPrimitiveExpression(self, expr, ctx):
		values = [self.v_Visit(e, ctx) for e in expr]
		cpi = LinearIR.ConstructPrimitiveInstruction(expr.GetType(), values)
		ctx.BasicBlock.AddInstruction(cpi)
		return cpi

	def v_LiteralExpression(self, expr, ctx):
		cv = LinearIR.ConstantValue(expr.GetType (), expr.GetValue())
		ctx.Function.RegisterValue(cv)
		return cv

	def v_MemberAccessExpression(self, expr, ctx):
		parent = expr.GetParent ()
		member = expr.GetMember ()
		if parent:
			value = self.v_Visit(parent, ctx)
			mai = LinearIR.MemberAccessInstruction(member.GetType (),
				value, member)
			ctx.BasicBlock.AddInstruction(mai)
			return mai

	def v_PrimaryExpression(self, expr, ctx):
		li = LinearIR.VariableAccessInstruction(expr.GetType(), expr.GetName ())
		ctx.BasicBlock.AddInstruction(li)
		return li
		
	def v_Expression(self, expr, ctx):
		return None
		
	def v_CastExpression(self, ce, ctx):
		castValue = self.v_Visit(ce.GetArgument(), ctx)
		instruction = LinearIR.CastInstruction(castValue, ce.GetType())
		ctx.BasicBlock.AddInstruction(instruction)
		return instruction
			
	def v_BinaryExpression(self, be, ctx):
		left = self.v_Visit (be.GetLeft (), ctx)
		right = self.v_Visit (be.GetRight (), ctx)
		
		instruction = LinearIR.BinaryInstruction.FromOperation (be.GetOperation (),
			be.GetType(), left, right)
		ctx.BasicBlock.AddInstruction(instruction)
		return instruction
		
	def v_ExpressionStatement(self, es, ctx):
		return self.v_Visit(es.GetExpression(), ctx)
					
	def v_ReturnStatement (self, stmt, ctx):
		v = self.v_Visit(stmt.GetExpression(), ctx)
		ri = LinearIR.ReturnInstruction(v)
		ctx.BasicBlock.AddInstruction(ri)
		ctx.EndBasicBlock()

		return ri

	def v_VariableDeclaration(self, vd, ctx):
		if vd.HasInitializerExpression():
			initValue = self.v_Visit(vd.GetInitializerExpression(), ctx)
			store = LinearIR.VariableAccessInstruction(vd.GetType (), vd.GetName())
			ctx.BasicBlock.AddInstruction(store)
			store.SetStore(initValue)

	def v_Program (self, program, ctx):
		ctx.OnEnterProgram()
		for function in program.GetFunctions():
			self.v_Visit(function, ctx)
		ctx.OnLeaveProgram()

	def v_MethodCallExpression(self, expr, ctx):
		obj = self.v_Visit(expr.GetMemberAccess().GetParent(), ctx)
		args = [self.v_Visit(arg, ctx) for arg in expr]
		ci = LinearIR.CallInstruction(expr.GetType(),
			expr.GetMemberAccess().member.GetName(),
			args,
			obj)
		ctx.BasicBlock.AddInstruction(ci)
		return ci

	def v_CallExpression(self, expr, ctx):
		args = [self.v_Visit(arg, ctx) for arg in expr]
		ci = LinearIR.CallInstruction(expr.GetType(),
			expr.GetFunction().GetName(),
			args)
		ctx.BasicBlock.AddInstruction(ci)
		return ci
	
	def v_AssignmentExpression(self, expr, ctx):
		value = self.v_Visit(expr.GetRight(), ctx)
		destination = self.v_Visit(expr.GetLeft(), ctx)
		destination.SetStore(value)
		return destination

	def v_ArrayExpression(self, expr, ctx):
		array = self.v_Visit(expr.GetParent(), ctx)
		index = self.v_Visit(expr.GetExpression(), ctx)
		ai = LinearIR.ArrayAccessInstruction(expr.GetType(), array, index)
		ctx.BasicBlock.AddInstruction(ai)
		return ai
		
def GetPass():
	import nsl.Pass
		
	ctx = LowerToIRVisitor.Context()

	return nsl.Pass.MakePassFromVisitor(LowerToIRVisitor (ctx), 'lower-to-linear-LinearIR')