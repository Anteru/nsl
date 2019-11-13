from nsl import ast, LinearIR, op, types, Errors, Visitor
import collections

class LowerToIRVisitor(Visitor.DefaultVisitor):
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
			self.__assignmentValue = []

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

		def OnEnterNode(self):
			self.__assignmentValue.append (None)

		def OnLeaveNode(self):
			self.__assignmentValue.pop ()

		def BeginAssignment(self, value):
			self.__assignmentValue.append(value)

		def EndAssignment(self):
			self.__assignmentValue.pop()

		@property
		def InAssignment(self):
			return len(self.__assignmentValue) >= 2 and self.__assignmentValue[-2] is not None

		@property
		def AssignmentValue(self):
			return self.__assignmentValue[-2]

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
		constOne = ctx.Function.CreateConstant(expr.GetType(), 1)
		initialValue = self.v_Visit(expr.GetExpression(), ctx)
		assert isinstance(initialValue, LinearIR.Value)

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
		else:
			Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
				"Invalid affix expression -- must be either prefix or postfix"
			)

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
		condBranch = LinearIR.BranchInstruction(
			condBB, # We will replace this later
			None,
			cond)
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
		branch = LinearIR.BranchInstruction(
			ctx.BasicBlock, # We will replace this later
			None,
			condition)
		ctx.BasicBlock.AddInstruction(branch)

		# Process the "true" side
		trueBlock = ctx.CreateBasicBlock()
		self.v_Visit(expr.GetTruePath(), ctx)
		branch.SetTrueBlock(trueBlock)

		# Add branch to exit
		if expr.HasElsePath():
			exitBranch = LinearIR.BranchInstruction(
				trueBlock # We will replace this later
			)
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
		
		cpi = LinearIR.ConstructPrimitiveInstruction(expr.GetType(),
			values)
		ctx.BasicBlock.AddInstruction(cpi)

		return cpi

	def v_LiteralExpression(self, expr, ctx):
		return ctx.Function.CreateConstant(expr.GetType (), expr.GetValue())

	def v_MemberAccessExpression(self, expr, ctx):
		parent = expr.GetParent ()
		member = expr.GetMember ()
		if parent:
			value = self.v_Visit(parent, ctx)
		else:
			# ICE
			return

		if expr.isSwizzle:
			last = value

			swizzleComponentToIndex = {
				'r' : 0,
				'g' : 1,
				'b' : 2,
				'a' : 3,
				'x' : 0,
				'y' : 1,
				'z' : 2,
				'w' : 3,
			}

			# If it's a load, it's simple -- we just shuffle the elements in
			# value. For a store, we combine the parent and the value as
			# specified
			if not ctx.InAssignment:
				indices = []
				for c in member.GetName():
					indices.append(swizzleComponentToIndex[c])

				assert isinstance(value, LinearIR.Value)

				si = LinearIR.ShuffleInstruction(expr.GetType(),
					value, value, indices)
				ctx.BasicBlock.AddInstruction(si)
				return si
			else:
				leftComponentCount = value.Type.GetComponentCount()
				indices = list(range(leftComponentCount))

				for i, c in enumerate(member.GetName()):
					writeIndex = swizzleComponentToIndex[c]
					readIndex = leftComponentCount + i
					indices[writeIndex] = readIndex

				assert isinstance(value, LinearIR.Value)

				si = LinearIR.ShuffleInstruction(expr.GetType(),
					value, ctx.AssignmentValue, indices)
				ctx.BasicBlock.AddInstruction(si)

				ctx.BeginAssignment(si)
				result = self.v_Visit(parent, ctx)
				ctx.EndAssignment()

				return result
		else:
			mai = LinearIR.MemberAccessInstruction(member.GetType (),
				value, member)
			ctx.BasicBlock.AddInstruction(mai)
			return mai

	def v_PrimaryExpression(self, expr, ctx):
		accessScope = ctx.LookupVariableScope(expr.GetName())
		li = LinearIR.VariableAccessInstruction(expr.GetType(),
			expr.GetName (),
			accessScope)

		if ctx.InAssignment:
			li.SetStore(ctx.AssignmentValue)
		
		ctx.BasicBlock.AddInstruction(li)
		return li
		
	def v_CastExpression(self, ce, ctx):
		castValue = self.v_Visit(ce.GetArgument(), ctx)

		assert isinstance(castValue, LinearIR.Value)

		instruction = LinearIR.CastInstruction(castValue, ce.GetType())
		ctx.BasicBlock.AddInstruction(instruction)
		return instruction
			
	def v_BinaryExpression(self, be, ctx):
		left = self.v_Visit (be.GetLeft (), ctx)
		right = self.v_Visit (be.GetRight (), ctx)

		assert isinstance(left, LinearIR.Value)
		assert isinstance(right, LinearIR.Value)
		
		instruction = LinearIR.BinaryInstruction.FromOperation (be.GetOperation (),
			be.GetType(), left, right)
		ctx.BasicBlock.AddInstruction(instruction)
		return instruction
					
	def v_ReturnStatement (self, stmt, ctx):
		v = self.v_Visit(stmt.GetExpression(), ctx)

		assert isinstance(v, LinearIR.Value)

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
		args = [self.v_Visit(arg, ctx) for arg in expr]
		ci = LinearIR.CallInstruction(expr.GetType(),
			expr.GetMemberAccess().member.GetName(),
			args)
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
		ctx.BeginAssignment(value)
		destination = self.v_Visit(expr.GetLeft(), ctx)
		ctx.EndAssignment()

		return destination

	def v_ArrayExpression(self, expr, ctx):
		array = self.v_Visit(expr.GetParent(), ctx)
		index = self.v_Visit(expr.GetExpression(), ctx)
		ai = LinearIR.ArrayAccessInstruction(expr.GetType(), array, index)

		if ctx.InAssignment:
			ai.SetStore(ctx.AssignmentValue)

		ctx.BasicBlock.AddInstruction(ai)
		return ai

	def OnEnter(self, _, ctx):
		ctx.OnEnterNode()

	def OnLeave(self, _, ctx):
		ctx.OnLeaveNode()
		
def GetPass():
	import nsl.Pass
		
	ctx = LowerToIRVisitor.Context()

	return nsl.Pass.MakePassFromVisitor(LowerToIRVisitor (ctx),
		'lower-to-linear-LinearIR')