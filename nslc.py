#!/usr/bin/env python3
from nsl.Compiler import Compiler
import click

@click.command()
@click.option('--debug-parsing', default=False)
@click.argument('input_file', type=click.File('r'))
def Compile(input_file, debug_parsing):
    c = Compiler ()
    
    c.Compile (input_file.read(), debugParsing = debug_parsing)

if __name__=='__main__':
    Compile()