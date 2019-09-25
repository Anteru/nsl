from nsl.ast import SourceMapping

class TestAstSourceMapping:
    def testLineOffsets(self):
        sm = SourceMapping('''0\n11\n\n2''')
        #                     0 123 4 56
        #                     0 011 1 23
        assert sm.GetLineFromOffset(0) == 0
        assert sm.GetLineFromOffset(1) == 0
        assert sm.GetLineFromOffset(2) == 1
        assert sm.GetLineFromOffset(4) == 1
        assert sm.GetLineFromOffset(5) == 2
        assert sm.GetLineFromOffset(6) == 3