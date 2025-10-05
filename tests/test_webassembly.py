import nsl.WebAssembly


class TestWebassembly:
    def testPackLEB128Unsigned(self):
        b = nsl.WebAssembly.PackInteger(624485)
        assert b == bytes([0xE5, 0x8E, 0x26])

    def testPackLEB128Signed(self):
        b = nsl.WebAssembly.PackInteger(-123456)
        assert b == bytes([0xC0, 0xBB, 0x78])

    def testPackLEB128U8(self):
        b = nsl.WebAssembly.PackInteger(3)
        assert b == bytes([0x83, 0x00]) or b == bytes([0x03])
