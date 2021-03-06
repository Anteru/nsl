#!/usr/bin/env python3
from nsl.Compiler import Compiler
import argparse
import sys

def Compile(args):
    c = Compiler ()
    return c.Compile (args.FILE.read(),
        options={
                 'debug-parsing' : args.debug_parsing,
                 'debug-passes' : args.debug_passes,
                 'wasm' : args.wasm,
                 'optimize': args.opt_level > 0})

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
    parser.add_argument('--wasm', type=argparse.FileType('wb'))
    parser.add_argument('--verbose', '-v', action='store_true')
    parser.add_argument('FILE', type=argparse.FileType('r'),
        metavar='INPUT')
    parser.add_argument('-o', '--output', type=argparse.FileType('wb'))
    args = parser.parse_args()

    result = Compile(args)

    if result is None:
        if args.verbose:
            print('SUCCESS')
        else:
            print('ERROR')

        sys.exit(1)

    if args.wasm:
        result.WasmModule.WriteTo(args.wasm)

    if args.output:
        import pickle
        pickle.dump(result.IRModule, args.output)

    sys.exit(0)
