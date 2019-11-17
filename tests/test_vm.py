from nsl import (
    Compiler,
    VM
)

import pytest

def _compile(code):
    c = Compiler.Compiler()
    result, ir = c.Compile(code)
    assert result == True

    vm = VM.VirtualMachine(ir)
    return vm

def testSimpleAddInt():
    code = '''export function f (int a, int b) -> int { return a + b; }'''
    vm = _compile(code)
    r = vm.Invoke('f', a=3, b=5)
    assert r == 8

def testSimpleAddIntToGlobal():
    code = '''int g;
    export function f (int a) -> int { return a + g; }'''

    vm = _compile(code)
    vm.SetGlobal('g', 23)

    r = vm.Invoke('f', a = 42)
    assert r == 65

def testSimpleAddIntToGlobalArray():
    code = '''int g[2];
    export function f (int a, int i) -> int { return a + g[i]; }'''

    vm = _compile(code)
    vm.SetGlobal('g', [5, 7])

    r = vm.Invoke('f', a = 3, i = 1)
    assert r == 10

def testSimpleWriteToGlobal():
    code = '''int g;
    export function f(int v) -> void { g = v; }'''

    vm = _compile(code)
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

    vm = _compile(code)

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

    vm = _compile(code)

    r = vm.Invoke('g_i', a = 5)
    assert r == 10

    r = vm.Invoke('g_f', a = 2.5)
    assert r == 5

def testPrefixIncrement():
    code = '''export function f(int a) -> int { return ++a; }'''
    vm = _compile(code)

    r = vm.Invoke('f', a = 1)
    assert r == 2

def testSimpleBranch():
    code = '''export function f(int a) -> int {
        if(a > 5) return 0; return 1; 
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', a = 0)
    assert r == 1

    r = vm.Invoke('f', a = 6)
    assert r == 0

def testSimpleIfElseBranch():
    code = '''export function f(int a) -> int {
        if(a > 5) { return 0; } else { return 1; }
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', a = 0)
    assert r == 1

    r = vm.Invoke('f', a = 6)
    assert r == 0

def testSimpleIfElseIfElseBranch():
    code = '''export function f(int a) -> int {
        if(a > 0) { return 1; } else if (a < 0) { return -1; } else { return 0; }
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', a = 0)
    assert r == 0

    r = vm.Invoke('f', a = 2)
    assert r == 1

    r = vm.Invoke('f', a = -2)
    assert r == -1

def testArrayAccessOnVector():
    code = '''export function f(float4 f, int i) -> float {
        return f[i];
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', f = [4, 5, 6, 7], i = 2)
    assert r == 6

def testArrayAccessOnMatrix():
    code = '''export function f(float4x4 f, int i, int j) -> float {
        return f[i][j];
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', f = [
        [4, 5, 6, 7],
        [8, 9, 10, 11],   
        [12, 13, 14, 15],   
        [16, 17, 18, 19]
    ], i = 2, j = 3)
    assert r == 15

def testConstructPrimitiveVector():
    code = '''export function f(float2 a, float b, float c) -> float4 {
        return float4(a, b, c);
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', a = [1, 2], b = 3, c = 4)
    assert r == [1, 2, 3, 4]

def testRunSimpleLoop():
    code = '''export function f(float f, int l) -> float {
        for (int i = 0; i < l; ++i) {
            f *= f;
        }

        return f;
    }'''
    vm = _compile(code)

    r = vm.Invoke('f', f = 2, l = 4)
    assert r == 2**16

def testRunSimpleSwizzleRead():
    code = '''export function f(float4 p) -> float2 {
        return p.xz;
    }'''
    
    vm = _compile(code)

    r = vm.Invoke('f', p = [1, 2, 3, 4])
    assert r == [1, 3]

def testRunSimpleSwizzleWrite():
    code = '''export function f(float4 p) -> float4 {
        p.y = 3;
        return p;
    }'''
    
    vm = _compile(code)

    r = vm.Invoke('f', p = [5, 6, 7, 8])
    assert r == [5, 3, 7, 8]

def testRunSimpleVectorCompare():
    code = '''export function f(int4 a, int4 b) -> int4 {
        return a == b;
    }'''
    
    vm = _compile(code)

    r = vm.Invoke('f', a = [1, 2, 3, 4], b = [1, 0, 3, 0])
    assert r == [1, 0, 1, 0]

def testRunSimpleVectorAdd():
    code = '''export function f(int4 a, int4 b) -> int4 {
        return a + b;
    }'''

    vm = _compile(code)

    r = vm.Invoke('f', a = [1, 2, 3, 4], b = [1, 0, 3, 0])
    assert r == [2, 2, 6, 4]

def testCreateMatrix():
    code = '''export function f() -> float4x4 {
        return float4x4(
            float4(1, 2, 3, 4),
            float4(5, 6, 7, 8),
            float4(9, 10, 11, 12),
            float4(13, 14, 15, 16)
        );
    }'''

    vm = _compile(code)

    r = vm.Invoke('f')
    assert r == [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
    ]

def testVectorTimesScalar():
    code = '''export function f(float4 v, float s) -> float4 {
        return v * s;
    }'''

    
    vm = _compile(code)

    r = vm.Invoke('f', v=[1, 2, 3, 4], s = 2)
    assert r == [2, 4, 6, 8]

def testVectorDividedByScalar():
    code = '''export function f(float4 v, float s) -> float4 {
        return v / s;
    }'''

    
    vm = _compile(code)

    r = vm.Invoke('f', v=[2, 4, 6, 8], s = 2)
    assert r == [1, 2, 3, 4]

def testMatrixScalarMultiply():
    code = '''export function f(float4x4 m, float s) -> float4x4 {
        return m * s;
    }'''

    vm = _compile(code)

    m = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
    ]
    r = vm.Invoke('f', m=m, s=2)
    assert r == [[i * 2 for i in r] for r in m]

def testMatrixMatrixAdd():
    code = '''export function f(float4x4 a, float4x4 b) -> float4x4 {
        return a + b;
    }'''

    vm = _compile(code)

    m = [
        [1, 2, 3, 4],
        [5, 6, 7, 8],
        [9, 10, 11, 12],
        [13, 14, 15, 16]
    ]
    r = vm.Invoke('f', a=m, b=m)
    assert r == [[i * 2 for i in r] for r in m]

def testMatrixMatrixMultiply():
    code = '''export function f(float4x4 a, float4x4 b) -> float4x4 {
        return a * b;
    }'''

    vm = _compile(code)

    a = [
        [5, 1, 4, 4],
        [5, 5, 6, 2],
        [4, 1, 0, 1],
        [5, 0, 4, 4],
    ]
    b = [
        [2, 0, 4, 4],
        [8, 1, 9, 6],
        [4, 3, 5, 7],
        [8, 8, 0, 3],
    ]

    r = vm.Invoke('f', a=a, b=b)
    assert r == [
        [66, 45, 49, 66],
        [90, 39, 95, 98],
        [24,  9, 25, 25],
        [58, 44, 40, 60],
    ]

def testMatrixRowAccess():
    code = '''export function f(float4x4 m, int i) -> float4 {
        return m[i];
    }'''

    vm = _compile(code)

    a = [
        [5, 1, 4, 4],
        [5, 5, 6, 2],
        [4, 1, 0, 1],
        [5, 0, 4, 4],
    ]
    
    r = vm.Invoke('f', m=a, i=2)
    assert r == [4, 1, 0, 1]