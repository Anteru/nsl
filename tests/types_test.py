'''
Created on 10.08.2011

@author: Anteru
'''
import unittest
from nsl import types
class Test(unittest.TestCase):
    def testIsCompatibleFloat2Int2(self):
        f2 = types.VectorType (types.Float (), 2)
        i2 = types.VectorType (types.Integer (), 2)

        self.assertTrue(types.IsCompatible(f2, i2))

    def testIsCompatibleFloat2Int4(self):
        f2 = types.VectorType (types.Float (), 2)
        i4 = types.VectorType (types.Integer (), 4)

        self.assertFalse (types.IsCompatible(f2, i4))

if __name__ == "__main__":
    unittest.main()