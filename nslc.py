#!/usr/bin/env python3
from nsl.Compiler import Compiler
from nsl.VM import VirtualMachine
import click
import sys

@click.command()
@click.option('--debug-parsing', default=False, type=bool)
@click.option('--debug-passes', default=True, type=bool)
@click.argument('input_file', type=click.File('r'))
def Compile(input_file, debug_parsing, debug_passes):
    c = Compiler ()
    ok, ir = c.Compile (input_file.read(),
        options={
                 'debug-parsing' : debug_parsing,
                 'debug-passes' : debug_passes})

    vm = VirtualMachine(ir)
    result = vm.Invoke('AddTwoIntegers', a=2, b=3)
    print(result)

    vm.BindGlobal('global_a', 23)
    result = vm.Invoke('AddToGlobal', a=42)
    print(result)

if __name__=='__main__':
    Compile()