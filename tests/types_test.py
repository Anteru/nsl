'''
Created on 10.08.2011

@author: Anteru
'''
import unittest
from nsl import types, op

class Test(unittest.TestCase):
	def testIsCompatibleFloat2Int2(self):
		f2 = types.VectorType (types.Float (), 2)
		i2 = types.VectorType (types.Integer (), 2)

		self.assertTrue(types.IsCompatible(f2, i2))

	def testIsCompatibleFloat2Int4(self):
		f2 = types.VectorType (types.Float (), 2)
		i4 = types.VectorType (types.Integer (), 4)

		self.assertFalse (types.IsCompatible(f2, i4))

	def testResolveBinaryExpressionTypeMV (self):
		m44 = types.MatrixType (types.Float (), 4, 4)
		f4 = types.VectorType (types.Float (), 4)

		r = types.ResolveBinaryExpressionType (op.Operation.MUL, m44, f4)

		self.assertEqual(r.GetReturnType (), f4)

	def testResolveBinaryExpressionTypeMVPromotesLeftSide (self):
		m44 = types.MatrixType (types.Integer (), 4, 4)
		f4 = types.VectorType (types.Float (), 4)

		r = types.ResolveBinaryExpressionType (op.Operation.MUL, m44, f4)

		self.assertEqual (r.GetOperandType (0), types.MatrixType (types.Float (), 4, 4))

	def testResolveBinaryExpressionTypeMVPromotesRightSide (self):
		m44 = types.MatrixType (types.Float (), 4, 4)
		f4 = types.VectorType (types.Integer (), 4)

		r = types.ResolveBinaryExpressionType (op.Operation.MUL, m44, f4)

		self.assertEqual (r.GetOperandType (1), types.VectorType (types.Float (), 4))

	def testResolveBinaryExpressionMVFailsOnIncompatibleSizes (self):
		self.assertRaises(Exception, types.ResolveBinaryExpressionType (
			op.Operation.MUL,
			types.MatrixType (types.Float (), 4, 2),
			types.VectorType (types.Float (), 2)
			))

	def testResolveBinaryExpressionMVFailsForNonMultiply (self):
		invalidOperations = [
			op.Operation.ADD,
			op.Operation.SUB,
			op.Operation.DIV
		]

		for operation in invalidOperations:
			self.assertRaises(Exception, types.ResolveBinaryExpressionType (
				operation,
				types.MatrixType (types.Float (), 4, 4),
				types.VectorType (types.Float (), 2)
				))


if __name__ == "__main__":
	unittest.main()