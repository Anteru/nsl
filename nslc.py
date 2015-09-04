#!/usr/bin/env python3
from nsl.Compiler import Compiler

c = Compiler ()

c.Compile ('''
struct ApplicationToVertex
{
    float2 position : POSITION;
    float4 color    : UV0;
}

matrix4x4 worldViewProjection;
Texture2D tex;

struct VertexToPixel
{
    float4 position : POSITION;
    float4 color    : UV0;
}

shader(vertex) (ApplicationToVertex app2vs) -> VertexToPixel
{
    VertexToPixel result;

    result.position = worldViewProjection * float4 (app2vs.position.xy, 0, 1);
    result.color = app2vs.color;

    return result;
}

struct FragmentOutput
{
    float4 color : COLOR[0];
}

shader(pixel) (VertexToPixel vs2ps) -> FragmentOutput
{
    FragmentOutput out;
    out.color = vs2ps.color;
    return out;
}
''', debugParsing=False)
