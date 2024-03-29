#!/usr/bin/env python3
from nsl import LinearIR, VM
import argparse
import wasmer
from wasmer_compiler_cranelift import Compiler

def run(args):
	if args.wasm:
		print(args.MODULE)
		store = wasmer.Store(wasmer.engine.JIT(Compiler))
		module = wasmer.Module(
			store,
			open(args.MODULE, 'rb').read()
		)
		instance = wasmer.Instance(module)
		try:
			f = getattr(instance.exports, args.FUNCTION)
		except:
			print(f'ERROR: Function "{args.FUNCTION}" not found')
		
		def convertArgs(parameterTypes, arguments):
			for param, arg in zip(parameterTypes, arguments):
				match param:
					case wasmer.Type.F32:
						yield float(arg)
					case wasmer.Type.I32:
						yield int(arg)

		invokeArgs = list(convertArgs(f.type.params, args.ARGS))

		print (f'{args.FUNCTION} (' + ', '.join(map(str,invokeArgs)) + ') =',
			f(*invokeArgs))

	else:
		loader = LinearIR.FilesystemModuleLoader()
		linker = LinearIR.Linker()
		module = loader.Load(args.MODULE)

		linker.AddModule(module)
		program = linker.Link()

		vm = VM.VirtualMachine(program)

		entryPoint = program.Functions[args.FUNCTION]
		invokeArgs = {}

		currentArg = 0
		for name, argType in entryPoint.Type.Arguments.items():
			assert isinstance(argType, LinearIR.Type)
			if argType.IsScalar():
				if isinstance(argType, LinearIR.IntegerType):
					invokeArgs[name] = int(args.ARGS[currentArg])
				elif isinstance(argType, LinearIR.FloatType):
					invokeArgs[name] = float(args.ARGS[currentArg])
			currentArg += 1

		print (f'{args.FUNCTION} (' + ', '.join(map(str,invokeArgs.values())) + ') =',
			vm.Invoke(args.FUNCTION, **invokeArgs))

if __name__=='__main__':
	parser = argparse.ArgumentParser('nslr')
	subparsers = parser.add_subparsers()
	runCommand = subparsers.add_parser('run')

	runCommand.add_argument('--wasm', action='store_true')

	runCommand.add_argument('MODULE', type=str)
	runCommand.add_argument('FUNCTION', type=str)
	runCommand.add_argument('ARGS', nargs='*')

	runCommand.set_defaults(func=run)

	args = parser.parse_args()
	args.func(args)