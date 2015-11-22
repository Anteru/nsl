from nsl import types, op
import pytest

class TestTypes:
	def testIsCompatibleFloat2Int2(self):
		f2 = types.VectorType(types.Float(), 2)
		i2 = types.VectorType(types.Integer(), 2)

		assert types.IsCompatible(f2, i2)

	def testIsCompatibleFloat2Int4(self):
		f2 = types.VectorType(types.Float(), 2)
		i4 = types.VectorType(types.Integer(), 4)

		assert not types.IsCompatible(f2, i4)

	def testIsCompatibleIntInt1(self):
		i = types.Integer()
		i1 = types.VectorType(types.Integer(), 1)

		assert types.IsCompatible(i, i1)

	def testResolveBinaryExpressionTypeMV(self):
		m44 = types.MatrixType(types.Float(), 4, 4)
		f4 = types.VectorType(types.Float(), 4)

		r = types.ResolveBinaryExpressionType(op.Operation.MUL, m44, f4)

		assert r.GetReturnType() == f4

	def testResolveBinaryExpressionTypeMVPromotesLeftSide(self):
		m44 = types.MatrixType(types.Integer(), 4, 4)
		f4 = types.VectorType(types.Float(), 4)

		r = types.ResolveBinaryExpressionType(op.Operation.MUL, m44, f4)

		assert r.GetOperandType(0) == types.MatrixType(types.Float(), 4, 4)

	def testResolveBinaryExpressionTypeMVPromotesRightSide(self):
		m44 = types.MatrixType(types.Float(), 4, 4)
		f4 = types.VectorType(types.Integer(), 4)

		r = types.ResolveBinaryExpressionType(op.Operation.MUL, m44, f4)

		assert r.GetOperandType(1) == types.VectorType(types.Float(), 4)

	def testResolveBinaryExpressionMVFailsOnIncompatibleSizes(self):
		with pytest.raises(Exception):
			types.ResolveBinaryExpressionType(op.Operation.MUL,
				types.MatrixType(types.Float(), 2, 4),
				types.VectorType(types.Float(), 2))
			
	def testResolveBinaryExpressionWorksOnCompatibleSizes(self):
		r = types.ResolveBinaryExpressionType(op.Operation.MUL,
				types.MatrixType(types.Float(), 4, 2),
				types.VectorType(types.Float(), 2))
		expectedType = types.VectorType(types.Float(), 4)
		
		assert r.GetReturnType() == expectedType

	def testResolveBinaryExpressionMVFailsForNonMultiply(self):
		invalidOperations = [op.Operation.ADD,
			op.Operation.SUB,
			op.Operation.DIV]

		for operation in invalidOperations:
			with pytest.raises(Exception):
				types.ResolveBinaryExpressionType(operation,
					types.MatrixType(types.Float(), 4, 4),
					types.VectorType(types.Float(), 2))

	def testResolveBinaryExpressionForMM(self):
		mt = types.MatrixType(types.Float(), 4, 4)
		
		for operation in {op.Operation.ADD, op.Operation.SUB, op.Operation.MUL}:
			resultType = types.ResolveBinaryExpressionType(operation, mt, mt)

			assert resultType.GetReturnType() == mt

	def testResolveBinaryExpressionFailsForMMDiv(self):
		mt = types.MatrixType(types.Float (), 4, 4)

		with pytest.raises(Exception):
			types.ResolveBinaryExpressionType (op.Operation.DIV,
				mt, mt)

	def testResolveBinaryExpressionForMMMul(self):
		left = types.MatrixType(types.Float(), 4, 2)
		right = types.MatrixType(types.Float (), 2, 4)

		resultType = types.ResolveBinaryExpressionType (op.Operation.MUL,
			left, right)

		expectedType = types.MatrixType(types.Float (), 2, 2)
		assert resultType.GetReturnType () == expectedType

	def testResolveBinaryExpressionForMMMulOnIncompatibleSizes(self):
		left = types.MatrixType(types.Float(), 4, 2)
		right = types.MatrixType(types.Float (), 3, 4)

		with pytest.raises(Exception):
			resultType = types.ResolveBinaryExpressionType (op.Operation.MUL,
				left, right)
