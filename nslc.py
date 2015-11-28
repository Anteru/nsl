#!/usr/bin/env python3
from nsl.Compiler import Compiler
import click

@click.command()
@click.option('--debug-parsing', default=False)
@click.option('--debug-passes', default=True)
@click.argument('input_file', type=click.File('r'))
def Compile(input_file, debug_parsing, debug_passes):
    c = Compiler ()
    
    c.Compile (input_file.read(),
        options={
                 'debug-parsing' : debug_parsing,
                 'debug-passes' : debug_passes})

if __name__=='__main__':
    Compile()