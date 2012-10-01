from nsl.Compiler import Compiler

c = Compiler ()

c.Compile ('''
struct VertexToFragment
{
    float4 position      : POSITION;
    float4 color         : COLOR;
}

struct FragmentOutput
{
    float4 color : COLOR[0];
}

function foo (float2 a, float2 b) -> float2
{
    if (a.x > 1)
    {
        return dot (a, b);
    }
    else
    {
        return b;
    }
}

__declaration function foo (float, float) -> float3;

function foo (float2 a, float3) -> float2
{
    return a.xx;
}

function foo (int2 a) -> float2
{
    return a;
}

shader(pixel) (VertexToFragment vtf) -> FragmentOutput
{
    FragmentOutput PS_Out;
    PS_Out.color.xy = foo (vtf.color.xy + (vtf.color.xx + vtf.color.yy));
    PS_Out.color.zw = foo (vtf.color.zw);
}
''')