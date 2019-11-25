#!/usr/bin/env python3
from nsl.Compiler import Compiler
from nsl.VM import VirtualMachine
import argparse

def Compile(args):
    c = Compiler ()
    ok, ir = c.Compile (args.FILE.read(),
        options={
                 'debug-parsing' : args.debug_parsing,
                 'debug-passes' : args.debug_passes,
                 'optimize': args.opt_level > 0})

    vm = VirtualMachine(ir)
    result = vm.Invoke('AddTwoIntegers', a=2, b=3)
    print(result)

    vm.SetGlobal('global_a', 23)
    result = vm.Invoke('AddToGlobal', a=42)
    print(result)

if __name__=='__main__':
    parser = argparse.ArgumentParser('nslc')
    parser.add_argument('--debug-parsing', action='store_true',
        default=False,
        help='Print output information about the parsing stage')
    parser.add_argument('--debug-passes', action='store_true',
        help='Write debug output about the passes')
    parser.add_argument('-O,--optimization-level',
        type=int, choices=[0, 1],
        default=0,
        dest='opt_level')
    parser.add_argument('FILE', type=argparse.FileType('r'),
        metavar='INPUT')
    args = parser.parse_args()

    Compile(args)