from nsl import ast, LinearIR, op, types, Errors, Visitor
import collections
from typing import Optional


def _CreateLinearIRType(t: types.Type):
    match t:
        case types.Integer():
            return LinearIR.IntegerType()
        case types.UnsignedInteger():
            return LinearIR.IntegerType(unsigned=True)
        case types.Float():
            return LinearIR.FloatType()
        case types.VectorType():
            return LinearIR.VectorType(
                _CreateLinearIRType(t.GetComponentType()), t.GetComponentCount()
            )
        case types.MatrixType():
            return LinearIR.MatrixType(
                LinearIR.VectorType(
                    _CreateLinearIRType(t.GetComponentType()),
                    t.GetColumnCount(),
                ),
                t.GetRowCount(),
            )
        case types.Function():
            return LinearIR.FunctionType(
                _CreateLinearIRType(t.GetReturnType()),
                collections.OrderedDict(
                    [
                        (k, _CreateLinearIRType(argType))
                        for k, argType in t.GetArgumentTypes().items()
                    ]
                ),
            )
        case types.ArrayType():
            return LinearIR.ArrayType(
                _CreateLinearIRType(t.GetComponentType()), t.GetSize()
            )
        case types.StructType():
            members = t.GetMembers()
            return LinearIR.StructureType(
                collections.OrderedDict(
                    [
                        (k, _CreateLinearIRType(members.GetFieldType(k)))
                        for k in members.GetSymbolNames()
                    ]
                ),
                name=t.GetName(),
            )
        case types.Void():
            return LinearIR.VoidType()

    raise Exception("Unhandled type: ", t)


class _BreakContinueStatements:
    def __init__(self, breakStatements, continueStatements):
        self.__breakStatements = breakStatements
        self.__continueStatements = continueStatements

    def SetContinueTarget(self, block: LinearIR.BasicBlock):
        for cs in self.__continueStatements:
            cs.SetTrueBlock(block)

    def SetBreakTarget(self, block: LinearIR.BasicBlock):
        for bs in self.__breakStatements:
            bs.SetTrueBlock(block)


class LowerToIRVisitor(Visitor.DefaultVisitor):
    def __init__(self, ctx):
        self.__ctx = ctx

    @property
    def Module(self) -> LinearIR.Module:
        return self.__ctx.Module

    class Context:
        def __init__(self):
            self.__module = LinearIR.Module()
            self.__functions = []
            self.__startNewBlock = False
            self.__globals = {}
            self.__args = {}
            self.__locals = {}
            self.__variables = {}
            self.__assignmentValue = []
            self.__loops = []
            self.__function = None

        def OnEnterModule(self, module):
            for g in module.GetDeclarations():
                for decl in g.GetDeclarations():
                    self.__globals[decl.GetName()] = (
                        LinearIR.VariableAccessScope.GLOBAL
                    )

        def OnLeaveModule(self):
            return self.__module

        @property
        def Module(self):
            return self.__module

        def OnEnterFunction(self, name, functionType):
            self.__function = self.__module.CreateFunction(name, functionType)
            self.__locals = {}
            self.__args = {}
            for arg in functionType.Arguments:
                self.__args[arg] = (
                    LinearIR.VariableAccessScope.FUNCTION_ARGUMENT
                )
            self.__variables = collections.ChainMap(
                self.__globals, self.__args, self.__locals
            )
            self.__startNewBlock = True

        def OnLeaveFunction(self):
            assert self.__function

            self.__function.UpdateUses()
            self.__functions.append(self.__function)
            self.__function = None

        def RegisterFunctionLocalVariable(self, name):
            self.__locals[name] = LinearIR.VariableAccessScope.FUNCTION_LOCAL

        def LookupVariableScope(self, name) -> LinearIR.VariableAccessScope:
            return self.__variables[name]

        def OnEnterNode(self):
            self.__assignmentValue.append(None)

        def OnLeaveNode(self):
            self.__assignmentValue.pop()

        def BeginAssignment(self, value: LinearIR.Value):
            self.__assignmentValue.append(value)

        def EndAssignment(self):
            self.__assignmentValue.pop()

        def BeginLoop(self):
            breakContinueContainer = collections.namedtuple(
                "BreakContinueContainer",
                ["breakStatements", "continueStatements"],
                defaults=[[], []],
            )
            self.__loops.append(breakContinueContainer())

        def EndLoop(self) -> _BreakContinueStatements:
            container = self.__loops.pop()
            return _BreakContinueStatements(
                container.breakStatements, container.continueStatements
            )

        def RegisterLoopContinue(self, branch: LinearIR.BranchInstruction):
            self.__loops[-1].continueStatements.append(branch)

        def RegisterLoopBreak(self, branch: LinearIR.BranchInstruction):
            self.__loops[-1].breakStatements.append(branch)

        @property
        def InAssignment(self) -> bool:
            return (
                len(self.__assignmentValue) >= 2
                and self.__assignmentValue[-2] is not None
            )

        @property
        def AssignmentValue(self) -> Optional[LinearIR.Value]:
            return self.__assignmentValue[-2]

        @property
        def Function(self):
            return self.__function

        @property
        def BasicBlock(self):
            assert self.__function
            if self.__startNewBlock:
                self.__function.CreateBasicBlock()
                self.__startNewBlock = False
            return self.__function.BasicBlocks[-1]

        def CreateBasicBlock(self):
            assert self.__function
            self.__startNewBlock = False
            return self.__function.CreateBasicBlock()

        def EndBasicBlock(self):
            self.__startNewBlock = True

        def AdaptType(self, t: types.Type) -> LinearIR.Type:
            return _CreateLinearIRType(t)

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
        ctx.OnEnterFunction(name, ctx.AdaptType(function.GetType()))
        function.GetBody().AcceptVisitor(self, ctx)
        ctx.OnLeaveFunction()

    def v_CompoundStatement(self, cs, ctx):
        for s in cs.GetStatements():
            self.v_Visit(s, ctx)

    def v_AffixExpression(self, expr, ctx):
        constOne = ctx.Function.CreateConstant(ctx.AdaptType(expr.GetType()), 1)
        initialValue = self.v_Visit(expr.GetExpression(), ctx)
        assert isinstance(initialValue, LinearIR.Value)

        returnValue = None

        opMap = {
            op.Operation.ADD: LinearIR.OpCode.ADD,
            op.Operation.SUB: LinearIR.OpCode.SUB,
        }

        operation = opMap[expr.GetOperation()]

        if expr.IsPostfix():
            # Return the value before
            # increment, save again
            instruction = LinearIR.BinaryInstruction(
                operation, ctx.AdaptType(expr.GetType()), initialValue, constOne
            )
            ctx.BasicBlock.AddInstruction(instruction)
            returnValue = initialValue
        elif expr.IsPrefix():
            # Return the value after
            # increment, save again
            instruction = LinearIR.BinaryInstruction(
                operation, ctx.AdaptType(expr.GetType()), initialValue, constOne
            )
            returnValue = ctx.BasicBlock.AddInstruction(instruction)
        else:
            Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise(
                "Invalid affix expression -- must be either prefix or postfix"
            )

        destination = self.v_Visit(expr.GetExpression(), ctx)

        destination.SetStore(instruction)
        return returnValue

    def v_ForStatement(self, expr: ast.ForStatement, ctx):
        # We split a loop as following: We always run the initializer, then
        # we emit the conditional test (which either continues to the body, or
        # the exit node.) After the body, we place the increment, and an
        # unconditional branch back to the conditional test

        self.v_Visit(expr.GetInitialization(), ctx)

        # Basic block containing the conditional test
        condBB = ctx.CreateBasicBlock()
        cond = self.v_Visit(expr.GetCondition(), ctx)
        condBranch = LinearIR.BranchInstruction(
            # Will be replaced later
            None,
            None,
            cond,
        )
        ctx.BasicBlock.AddInstruction(condBranch)

        # Basic block containing the body
        bodyBB = ctx.CreateBasicBlock()

        # We start our loop here, so we can use break/continue inside the body.
        # Those will get fixed up later
        ctx.BeginLoop()
        self.v_Visit(expr.GetBody(), ctx)
        breakContinueInstructions = ctx.EndLoop()

        # Increment operation
        incrementBB = ctx.CreateBasicBlock()
        self.v_Visit(expr.GetNext(), ctx)

        # Unconditional branch back to the conditional test
        backBranch = LinearIR.BranchInstruction(condBB)
        ctx.BasicBlock.AddInstruction(backBranch)
        endBB = ctx.CreateBasicBlock()

        # Fix up the condition to either continue with the body, or exit
        condBranch.SetTrueBlock(bodyBB)
        condBranch.SetFalseBlock(endBB)

        breakContinueInstructions.SetBreakTarget(endBB)
        breakContinueInstructions.SetContinueTarget(incrementBB)

    def v_DoStatement(self, expr, ctx: Context):
        # We lower this as following:
        # start:
        #   body
        #   if (condition) goto start: else goto end:
        # end:
        startBB = ctx.CreateBasicBlock()

        # Loop body
        ctx.BeginLoop()
        self.v_Visit(expr.GetBody(), ctx)
        breakContinueInstructions = ctx.EndLoop()

        # Conditional jump back to start or to end
        condition = self.v_Visit(expr.GetCondition(), ctx)
        branch = LinearIR.BranchInstruction(startBB, None, condition)
        ctx.BasicBlock.AddInstruction(branch)
        endBB = ctx.CreateBasicBlock()

        branch.SetFalseBlock(endBB)

        breakContinueInstructions.SetBreakTarget(endBB)
        breakContinueInstructions.SetContinueTarget(startBB)

    def v_WhileStatement(self, expr: ast.WhileStatement, ctx: Context):
        # We lower this as following
        # start: if (condition) goto body: else goto end:
        # body
        # goto start:
        # end:
        startBB = ctx.CreateBasicBlock()
        condition = self.v_Visit(expr.GetCondition(), ctx)
        branch = LinearIR.BranchInstruction(None, None, condition)
        ctx.BasicBlock.AddInstruction(branch)
        ctx.BeginLoop()
        bodyBB = ctx.CreateBasicBlock()
        self.v_Visit(expr.GetBody(), ctx)
        breakContinueInstructions = ctx.EndLoop()

        returnBranch = LinearIR.BranchInstruction(startBB)
        ctx.BasicBlock.AddInstruction(returnBranch)
        endBB = ctx.CreateBasicBlock()

        branch.SetTrueBlock(bodyBB)
        branch.SetFalseBlock(endBB)

        breakContinueInstructions.SetBreakTarget(endBB)
        breakContinueInstructions.SetContinueTarget(startBB)

    def v_ContinueStatement(self, _: ast.ContinueStatement, ctx):
        branch = LinearIR.BranchInstruction(None)
        ctx.BasicBlock.AddInstruction(branch)
        ctx.RegisterLoopContinue(branch)

        return branch

    def v_BreakStatement(self, _: ast.BreakStatement, ctx):
        branch = LinearIR.BranchInstruction(None)
        ctx.BasicBlock.AddInstruction(branch)
        ctx.RegisterLoopBreak(branch)

        return branch

    def v_IfStatement(self, expr: ast.IfStatement, ctx):
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
            exitBranch = LinearIR.BranchInstruction(
                trueBlock  # We will replace this later
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

        cpi = LinearIR.ConstructPrimitiveInstruction(
            ctx.AdaptType(expr.GetType()), values
        )
        ctx.BasicBlock.AddInstruction(cpi)

        return cpi

    def v_LiteralExpression(self, expr, ctx):
        return ctx.Function.CreateConstant(
            ctx.AdaptType(expr.GetType()), expr.GetValue()
        )

    def v_MemberAccessExpression(self, expr, ctx):
        parent = expr.GetParent()
        member = expr.GetMember()
        if parent:
            value = self.v_Visit(parent, ctx)
        else:
            Errors.ERROR_INTERNAL_COMPILER_ERROR.Raise("Invalid member access")
            return

        assert isinstance(value, LinearIR.Value)

        if expr.isSwizzle:
            swizzleComponentToIndex = {
                "r": 0,
                "g": 1,
                "b": 2,
                "a": 3,
                "x": 0,
                "y": 1,
                "z": 2,
                "w": 3,
            }

            # If it's a load, it's simple -- we just shuffle the elements in
            # value. For a store, we combine the parent and the value as
            # specified
            if not ctx.InAssignment:
                indices = []
                for c in member.GetName():
                    indices.append(swizzleComponentToIndex[c])

                assert isinstance(value, LinearIR.Value)

                si = LinearIR.ShuffleInstruction(
                    ctx.AdaptType(expr.GetType()), value, value, indices
                )
                ctx.BasicBlock.AddInstruction(si)
                return si
            else:
                leftComponentCount = value.Type.Size
                indices = list(range(leftComponentCount))

                for i, c in enumerate(member.GetName()):
                    writeIndex = swizzleComponentToIndex[c]
                    readIndex = leftComponentCount + i
                    indices[writeIndex] = readIndex

                assert isinstance(value, LinearIR.Value)

                si = LinearIR.ShuffleInstruction(
                    ctx.AdaptType(expr.GetType()),
                    value,
                    ctx.AssignmentValue,
                    indices,
                )
                ctx.BasicBlock.AddInstruction(si)

                ctx.BeginAssignment(si)
                result = self.v_Visit(parent, ctx)
                ctx.EndAssignment()

                return result
        else:
            assert isinstance(member, ast.PrimaryExpression)

            mai = LinearIR.MemberAccessInstruction(
                ctx.AdaptType(member.GetType()), value, member.GetName()
            )
            ctx.BasicBlock.AddInstruction(mai)

            if ctx.InAssignment:
                mai.SetStore(ctx.AssignmentValue)
            return mai

    def v_PrimaryExpression(self, expr, ctx):
        accessScope = ctx.LookupVariableScope(expr.GetName())
        li = LinearIR.VariableAccessInstruction(
            ctx.AdaptType(expr.GetType()), expr.GetName(), accessScope
        )

        if ctx.InAssignment:
            li.SetStore(ctx.AssignmentValue)

        ctx.BasicBlock.AddInstruction(li)
        return li

    def v_CastExpression(self, ce, ctx):
        castValue = self.v_Visit(ce.GetArgument(), ctx)

        assert isinstance(castValue, LinearIR.Value)

        instruction = LinearIR.CastInstruction(
            castValue, ctx.AdaptType(ce.GetType())
        )
        ctx.BasicBlock.AddInstruction(instruction)
        return instruction

    def __GetMatrixRowType(self, m: LinearIR.MatrixType):
        return LinearIR.VectorType(m.ElementType, m.RowCount)

    def v_BinaryExpression(self, be, ctx):
        assert isinstance(be.GetLeft().GetType(), types.PrimitiveType)
        assert isinstance(be.GetRight().GetType(), types.PrimitiveType)

        left = self.v_Visit(be.GetLeft(), ctx)
        right = self.v_Visit(be.GetRight(), ctx)

        assert isinstance(left, LinearIR.Value)
        assert isinstance(right, LinearIR.Value)

        if left.Type.IsMatrix() and right.Type.IsMatrix():
            # M <op> M, needs to get lowered per row
            operation = be.GetOperation()
            if operation == op.Operation.MUL:
                mul = LinearIR.BinaryInstruction(
                    LinearIR.OpCode.MATRIX_MUL_MATRIX,
                    ctx.AdaptType(be.GetType()),
                    left,
                    right,
                )
                ctx.BasicBlock.AddInstruction(mul)
                return mul
            else:
                assert operation != op.Operation.DIV
                # ADD, SUB, or CMP -- these can be executed per row, and then
                # combined back into a matrix result
                leftType = left.Type
                leftRowType = leftType.RowType

                rightType = right.Type
                rightRowType = rightType.RowType

                resultType = ctx.AdaptType(be.GetType())
                resultRowType = resultType.RowType
                rows = []
                for row in range(leftType.RowCount):
                    index = ctx.Function.CreateConstant(
                        LinearIR.IntegerType(), row
                    )
                    leftRow = LinearIR.MatrixAccessInstruction(
                        leftRowType, left, index
                    )
                    ctx.BasicBlock.AddInstruction(leftRow)

                    rightRow = LinearIR.MatrixAccessInstruction(
                        rightRowType, right, index
                    )
                    ctx.BasicBlock.AddInstruction(rightRow)

                    newRow = LinearIR.BinaryInstruction.FromOperation(
                        operation, resultRowType, leftRow, rightRow
                    )
                    ctx.BasicBlock.AddInstruction(newRow)
                    rows.append(newRow)

                result = LinearIR.ConstructPrimitiveInstruction(
                    resultType, rows
                )
                ctx.BasicBlock.AddInstruction(result)
                return result
        elif left.Type.IsMatrix() and right.Type.IsVector():
            # M <op> V, needs to get lowered to matrix-vector multiply
            pass
        elif left.Type.IsMatrix() and right.Type.IsScalar():
            # M <op> S, needs to get lowered to vector-scalar multiply or
            # division
            leftType = left.Type
            leftRowType = leftType.RowType

            rightType = left.Type

            resultType = ctx.AdaptType(be.GetType())
            resultRowType = resultType.RowType
            rows = []
            for row in range(leftType.RowCount):
                leftRow = LinearIR.MatrixAccessInstruction(
                    leftRowType,
                    left,
                    ctx.Function.CreateConstant(LinearIR.IntegerType(), row),
                )
                ctx.BasicBlock.AddInstruction(leftRow)

                newRow = LinearIR.BinaryInstruction.FromOperation(
                    be.GetOperation(), resultRowType, leftRow, right
                )
                ctx.BasicBlock.AddInstruction(newRow)
                rows.append(newRow)

            result = LinearIR.ConstructPrimitiveInstruction(resultType, rows)
            ctx.BasicBlock.AddInstruction(result)
            return result

        instruction = LinearIR.BinaryInstruction.FromOperation(
            be.GetOperation(), ctx.AdaptType(be.GetType()), left, right
        )
        ctx.BasicBlock.AddInstruction(instruction)
        return instruction

    def v_ReturnStatement(self, stmt, ctx):
        expr = stmt.GetExpression()

        if expr:
            v = self.v_Visit(stmt.GetExpression(), ctx)

            assert isinstance(v, LinearIR.Value)

            ri = LinearIR.ReturnInstruction(v)
        else:
            ri = LinearIR.ReturnInstruction()

        ctx.BasicBlock.AddInstruction(ri)
        ctx.EndBasicBlock()

        return ri

    def v_VariableDeclaration(self, vd, ctx):
        ctx.RegisterFunctionLocalVariable(vd.GetName())
        dvi = LinearIR.DeclareVariableInstruction(
            ctx.AdaptType(vd.GetType()),
            vd.GetName(),
            LinearIR.VariableAccessScope.FUNCTION_LOCAL,
        )
        ctx.BasicBlock.AddInstruction(dvi)

        if vd.HasInitializerExpression():
            initValue = self.v_Visit(vd.GetInitializerExpression(), ctx)
            store = LinearIR.VariableAccessInstruction(
                ctx.AdaptType(vd.GetType()),
                vd.GetName(),
                LinearIR.VariableAccessScope.FUNCTION_LOCAL,
            )
            ctx.BasicBlock.AddInstruction(store)
            store.SetStore(initValue)

    def v_Module(self, module: ast.Module, ctx: Context):
        import itertools

        ctx.OnEnterModule(module)

        ctx.Module.Metadata["functions"] = [
            f.GetType() for f in module.GetFunctions()
        ]
        ctx.Module.Metadata["types"] = {
            d.GetName(): d.GetType()
            for d in itertools.chain(
                *[gd.GetDeclarations() for gd in module.GetDeclarations()]
            )
        }

        for importName in module.GetImports():
            ctx.Module.AddImport(importName)

        for globalDeclaration in module.GetDeclarations():
            for decl in globalDeclaration.GetDeclarations():
                ctx.Module.CreateGlobalVariable(decl.GetName(), decl.GetType())
        for function in module.GetFunctions():
            self.v_Visit(function, ctx)
        return ctx.OnLeaveModule()

    def v_MethodCallExpression(self, expr, ctx):
        args = [self.v_Visit(arg, ctx) for arg in expr]
        ci = LinearIR.CallInstruction(
            ctx.AdaptType(expr.GetType()),
            expr.GetMemberAccess().member.GetName(),
            args,
        )
        ctx.BasicBlock.AddInstruction(ci)
        return ci

    def v_CallExpression(self, expr, ctx):
        args = [self.v_Visit(arg, ctx) for arg in expr]
        name = self.__GetFunctionName(expr.GetFunction())
        ci = LinearIR.CallInstruction(ctx.AdaptType(expr.GetType()), name, args)
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

        isPrimitive = array.Type.IsPrimitive()
        if isPrimitive:
            isVector = array.Type.IsVector()
            isMatrix = array.Type.IsMatrix()
        else:
            isVector = False
            isMatrix = False

        if isVector:
            cai = LinearIR.VectorAccessInstruction(
                ctx.AdaptType(expr.GetType()), array, index
            )
            ctx.BasicBlock.AddInstruction(cai)

            if ctx.InAssignment:
                cai.SetStore(ctx.AssignmentValue)

                ctx.BeginAssignment(cai)
                result = self.v_Visit(expr.GetParent(), ctx)
                ctx.EndAssignment()

                return result
            else:
                return cai
        elif isMatrix:
            cai = LinearIR.MatrixAccessInstruction(
                ctx.AdaptType(expr.GetType()), array, index
            )
            ctx.BasicBlock.AddInstruction(cai)

            if ctx.InAssignment:
                cai.SetStore(ctx.AssignmentValue)

                ctx.BeginAssignment(cai)
                result = self.v_Visit(expr.GetParent(), ctx)
                ctx.EndAssignment()

                return result
            else:
                return cai
        else:
            ai = LinearIR.ArrayAccessInstruction(
                ctx.AdaptType(expr.GetType()), array, index
            )

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

    return nsl.Pass.MakePassFromVisitor(
        LowerToIRVisitor(ctx), "lower-to-linear-IR"
    )
