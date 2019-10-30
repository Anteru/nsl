from nsl import ast, LinearIR, op, types
import collections

class LowerToIRVisitor(ast.DefaultVisitor):
	def __init__(self, ctx):
		self.__ctx = ctx

	@property
	def Program(self):
		return self.__ctx.Program
	
	class Context:
		def __init__(self):
			self.__program = LinearIR.Program()
			self.__functions = []
			self.__startNewBlock = False
			self.__globals = {}
			self.__args = {}
			self.__locals = {}
			self.__variables = {}

		def OnEnterProgram(self, program):
			for g in program.GetDeclarations():
				for decl in g.GetDeclarations():
					self.__globals[decl.GetName()] = LinearIR.VariableAccessScope.GLOBAL

		def OnLeaveProgram(self):
			return self.__program

		@property
		def Program(self):
			return self.__program

		def OnEnterFunction(self, name, functionType):
			self.__function = self.__program.CreateFunction(name, functionType)
			self.__locals = {}
			self.__args = {}
			for arg in functionType.GetArguments():
				self.__args[arg.GetName()] = LinearIR.VariableAccessScope.FUNCTION_ARGUMENT
			self.__variables = collections.ChainMap(
				self.__globals,
				self.__args,
				self.__locals)
			self.__startNewBlock = True

		def OnLeaveFunction(self):
			self.__functions.append(self.__function)
			self.__function = None

		def RegisterFunctionLocalVariable(self, name):
			self.__locals[name] = LinearIR.VariableAccessScope.FUNCTION_LOCAL

		def LookupVariableScope(self, name) -> LinearIR.VariableAccessScope:
			return self.__variables[name]

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
		return self.__ctx
		
	def __FormatArgumentList(self, args):
		return ', '.join(['{0} {1}'.format(arg.GetType().GetName(), arg.GetName()) for arg in args])
	
	def __GetFunctionName(self, functionType):
		"""Exported functions use their raw name, everything else uses the
		mangled name."""
		if functionType.exported:
			return functionType.GetName()
		else:
			return functionType.GetMangledName()

	def v_Function(self, function, ctx):
		functionType = function.GetType()
		name = self.__GetFunctionName(functionType)
		ctx.OnEnterFunction(name, function.GetType())
		function.GetBody().AcceptVisitor(self, ctx)
		ctx.OnLeaveFunction()

	def v_CompoundStatement(self, cs, ctx):
		for s in cs.GetStatements():
			self.v_Visit(s, ctx)

	def v_AffixExpression(self, expr, ctx):
		constOne = LinearIR.ConstantValue(expr.GetType(), 1)
		ctx.Function.RegisterConstant(constOne)
		initialValue = self.v_Visit(expr.GetExpression(), ctx)
		returnValue = None

		opMap = {
			op.Operation.ADD: LinearIR.OpCode.ADD,
			op.Operation.SUB: LinearIR.OpCode.SUB
		}

		operation = opMap[expr.GetOperation()]

		if expr.IsPostfix():
			# Return the value before
			# increment, save again
			instruction = LinearIR.BinaryInstruction(
				operation,
				expr.GetType (),
				initialValue,
				constOne
			)
			ctx.BasicBlock.AddInstruction(instruction)
			returnValue = initialValue
		elif expr.IsPrefix():
			# Return the value after 
			# increment, save again
			instruction = LinearIR.BinaryInstruction(
				operation,
				expr.GetType (),
				initialValue,
				constOne
			)
			returnValue = ctx.BasicBlock.AddInstruction(instruction)
		destination = self.v_Visit(expr.GetExpression(), ctx)

		destination.SetStore(instruction)
		return returnValue

	def v_ForStatement(self, expr, ctx):
		# We split a loop as following: We always run the initializer, then
		# we emit the conditional test (which either continues to the body, or
		# the exit node.) After the body, we place the increment, and an 
		# unconditional branch back to the conditional test
		
		init = self.v_Visit(expr.GetInitialization (), ctx)

		# Basic block containing the conditional test
		condBB = ctx.CreateBasicBlock ()
		cond = self.v_Visit(expr.GetCondition(), ctx)
		condBranch = LinearIR.BranchInstruction(None, None, cond)
		ctx.BasicBlock.AddInstruction(condBranch)

		# Basic block containing the body
		bodyBB = ctx.CreateBasicBlock ()
		self.v_Visit(expr.GetBody(), ctx)

		# Increment operation
		self.v_Visit(expr.GetNext (), ctx)

		# Unconditional branch back to the conditional test
		backBranch = LinearIR.BranchInstruction(condBB)
		ctx.BasicBlock.AddInstruction(backBranch)
		exitBB = ctx.CreateBasicBlock ()

		# Fix up the condition to either continue with the body, or exit
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
		dvi = LinearIR.DeclareVariableInstruction(expr.GetType(),
			scope = LinearIR.VariableAccessScope.FUNCTION_LOCAL)
		ctx.BasicBlock.AddInstruction(dvi)

		# Set the individual members
		offset = 0
		for value in values:
			cv = LinearIR.ConstantValue(types.Integer(), offset)
			ctx.Function.RegisterConstant(cv)

			cai = LinearIR.ComponentAccessInstruction(
				dvi.Type, value, cv
			)
			cai.SetStore(dvi)

			assert value.Type.IsPrimitive()
			assert not value.Type.IsMatrix()
			if value.Type.IsVector():
				offset += value.Type.GetComponentCount()
			else:
				offset += 1
			ctx.BasicBlock.AddInstruction(cai)
		return cai

	def v_LiteralExpression(self, expr, ctx):
		cv = LinearIR.ConstantValue(expr.GetType (), expr.GetValue())
		ctx.Function.RegisterConstant(cv)
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
		accessScope = ctx.LookupVariableScope(expr.GetName())
		li = LinearIR.VariableAccessInstruction(expr.GetType(),
			expr.GetName (),
			accessScope)
		ctx.BasicBlock.AddInstruction(li)
		return li
		
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
					
	def v_ReturnStatement (self, stmt, ctx):
		v = self.v_Visit(stmt.GetExpression(), ctx)
		ri = LinearIR.ReturnInstruction(v)
		ctx.BasicBlock.AddInstruction(ri)
		ctx.EndBasicBlock()

		return ri

	def v_VariableDeclaration(self, vd, ctx):
		ctx.RegisterFunctionLocalVariable(vd.GetName())
		dvi = LinearIR.DeclareVariableInstruction(vd.GetType (),
			vd.GetName(), LinearIR.VariableAccessScope.FUNCTION_LOCAL)
		ctx.BasicBlock.AddInstruction(dvi)
	
		if vd.HasInitializerExpression():
			initValue = self.v_Visit(vd.GetInitializerExpression(), ctx)
			store = LinearIR.VariableAccessInstruction(vd.GetType (), vd.GetName(),
				LinearIR.VariableAccessScope.FUNCTION_LOCAL)
			ctx.BasicBlock.AddInstruction(store)
			store.SetStore(initValue)

	def v_Program (self, program, ctx):
		ctx.OnEnterProgram(program)
		for globalDeclaration in program.GetDeclarations():
			for decl in globalDeclaration.GetDeclarations():
				ctx.Program.CreateGlobalVariable(decl.GetName(), decl.GetType())
		for function in program.GetFunctions():
			self.v_Visit(function, ctx)
		return ctx.OnLeaveProgram()

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
		name = self.__GetFunctionName(expr.GetFunction())
		ci = LinearIR.CallInstruction(expr.GetType(),
			name,
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

	return nsl.Pass.MakePassFromVisitor(LowerToIRVisitor (ctx),
		'lower-to-linear-LinearIR')