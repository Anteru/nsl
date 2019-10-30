from nsl import (
    Compiler,
    VM
)

import pytest

def testSimpleAddInt():
    code = '''export function f (int a, int b) -> int { return a + b; }'''
    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    r = vm.Invoke('f', a=3, b=5)
    assert r == 8

def testSimpleAddIntToGlobal():
    code = '''int g;
    export function f (int a) -> int { return a + g; }'''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    vm.SetGlobal('g', 23)

    r = vm.Invoke('f', a = 42)
    assert r == 65

def testSimpleAddIntToGlobalArray():
    code = '''int g[2];
    export function f (int a, int i) -> int { return a + g[i]; }'''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    vm.SetGlobal('g', [5, 7])

    r = vm.Invoke('f', a = 3, i = 1)
    assert r == 10

def testSimpleWriteToGlobal():
    code = '''int g;
    export function f(int v) -> void { g = v; }'''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    g = 0
    vm.SetGlobal('g', g)

    r = vm.Invoke('f', v = 5)
    g = vm.GetGlobal('g')
    assert g == 5

def testSimpleFunctionCall():
    code = '''
    function f(int v) -> int { return v; }
    export function g(int a) -> int {
        return a + f(a);
    }
    '''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)

    r = vm.Invoke('g', a = 5)
    assert r == 10

def testOverloadedFunctionCall():
    code = '''
    function f(float v) -> float { return v; }
    function f(int v) -> int { return v; }
    export function g_i(int a) -> int {
        return a + f(a);
    }

    export function g_f(float a) -> float {
        return a + f(a);
    }
    '''

    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)

    r = vm.Invoke('g_i', a = 5)
    assert r == 10

    r = vm.Invoke('g_f', a = 2.5)
    assert r == 5

def testPrefixIncrement():
    code = '''export function f(int a) -> int { return ++a; }'''
    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)

    r = vm.Invoke('f', a = 1)
    assert r == 2

def testSimpleBranch():
    code = '''export function f(int a) -> int {
        if(a > 5) return 0; return 1; 
    }'''
    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)

    r = vm.Invoke('f', a = 0)
    assert r == 1

    r = vm.Invoke('f', a = 6)
    assert r == 0
