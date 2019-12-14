#!/usr/bin/env python3
from nsl.Compiler import Compiler
import argparse
import sys

def Compile(args):
    c = Compiler ()
    ok, module = c.Compile (args.FILE.read(),
        options={
                 'debug-parsing' : args.debug_parsing,
                 'debug-passes' : args.debug_passes,
                 'optimize': args.opt_level > 0})

    return ok, module

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
    parser.add_argument('--verbose', '-v', action='store_true')
    parser.add_argument('FILE', type=argparse.FileType('r'),
        metavar='INPUT')
    parser.add_argument('-o', '--output', type=argparse.FileType('wb'))
    args = parser.parse_args()

    ok, module = Compile(args)

    if args.output:
        import pickle
        pickle.dump(module, args.output)

    if args.verbose:
        if ok:
            print('SUCCESS')
        else:
            print('ERROR')

    if ok:
        sys.exit(0)
    else:
        sys.exit(1)