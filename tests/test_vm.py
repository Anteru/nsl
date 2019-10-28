from nsl import (
    Compiler,
    VM
)

import pytest

def testSimpleAddIntFunction():
    code = '''function f (int a, int b) -> int { return a + b; }'''
    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    r = vm.Invoke('f', a=3, b=5)
    assert r == 8

def testSimpleAddIntToGlobalFunction():
    code = '''int g;
    function f (int a) -> int { return a + g; }'''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    vm.SetGlobal('g', 23)

    r = vm.Invoke('f', a = 42)
    assert r == 65

def testSimpleAddIntToGlobalArrayFunction():
    code = '''int g[2];
    function f (int a, int i) -> int { return a + g[i]; }'''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    vm.SetGlobal('g', [5, 7])

    r = vm.Invoke('f', a = 3, i = 1)
    assert r == 10

def testSimpleWriteToGlobalFunction():
    code = '''int g;
    function f(int v) -> void { g = v; }'''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    g = 0
    vm.SetGlobal('g', g)

    r = vm.Invoke('f', v = 5)
    g = vm.GetGlobal('g')
    assert g == 5