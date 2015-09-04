from nsl.passes.ValidateSwizzle import ValidateSwizzleMask
import pytest

class TestValidateSwizzlePass:
    def testMixFail(self):
        with pytest.raises(Exception):
            ValidateSwizzleMask('xyrg')