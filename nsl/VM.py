from . import (LinearIR, types)
import collections

class Value:
    def __init__(self, value = None, valueType: types.Type = types.UnknownType()):
        self.__value = value
        self.__type = valueType

    def Get(self):
        return self.__value

    def Set(self, value):
        self.__value = value

class Scope:
    def __init__(self, parent: 'Scope' = None):
        self.__parent = parent
        self.__variables = {}

    def Declare(self, variable, value=None):
        self.__variables[variable] = value

    def __getitem__(self, name):
        if name in self.__variables:
            return self.__variables[name]
        elif self.__parent:
            return self.__parent[name]
        else:
            raise Exception(f"Variable {name} not found")

    def __setitem__(self, key, value):
        self.__variables[key] = value

class ExecutionContext:
    def __init__(self, functions, globalScope: Scope):
        self.__globalScope = globalScope
        self.__functions = functions

    def Invoke(self, functionName, **args):
        functionScope = Scope(self.__globalScope)
        for k,v in args.items():
            functionScope.Declare(k, v)

        return self.__Execute(self.__functions[functionName], functionScope)

    def _Invoke(self, functionName, args):
        '''Similar to _Invoke, but we get a list of args and those are matched
        to the arguments of the function.'''
        functionScope = Scope(self.__globalScope)
        function = self.__functions[functionName]

        functionArgs = function.Type.GetArguments()
        for i, k in enumerate(functionArgs):
            functionScope.Declare(k.GetName(), args[i])

        return self.__Execute(function, functionScope)

    def __Execute(self, function: LinearIR.Function, functionScope):
        currentBB = function.BasicBlocks[0]

        localScope = Scope(functionScope)

        for instruction in currentBB.Instructions:
            opCode = instruction.OpCode

            if opCode == LinearIR.OpCode.LOAD:
                ref = instruction.Reference
                localScope[ref] = localScope[instruction.Variable]
            elif opCode == LinearIR.OpCode.STORE:
                if instruction.Scope == LinearIR.VariableAccessScope.GLOBAL:
                    self.__globalScope[instruction.Variable] = localScope[instruction.Store.Reference]
                else:
                    localScope[instruction.Variable] = localScope[instruction.Store.Reference]
            elif opCode == LinearIR.OpCode.LOAD_ARRAY:
                ref = instruction.Reference
                var = localScope[instruction.Array.Reference][
                    localScope[instruction.Index.Reference]
                ]
                localScope[ref] = var
            elif opCode.value >> 16 == 0x1:
                operation = instruction.OpCode
                op1 = localScope[instruction.Values[0].Reference]
                op2 = localScope[instruction.Values[1].Reference]
                ref = instruction.Reference

                if operation == LinearIR.OpCode.ADD:
                    localScope[ref] = op1 + op2
                elif operation == LinearIR.OpCode.SUB:
                    localScope[ref] = op1 - op2
                elif operation == LinearIR.OpCode.DIV:
                    localScope[ref] = op1 / op2
                elif operation == LinearIR.OpCode.MUL:
                    localScope[ref] = op1 * op2
            elif opCode == LinearIR.OpCode.RETURN:
                if instruction.Value:
                    return localScope[instruction.Value.Reference]
                else:
                    return None
            elif opCode == LinearIR.OpCode.CALL:
                if instruction.Object is None:
                    args = [
                        localScope[arg.Reference] for arg in
                        instruction.Arguments
                    ]
                    localScope[instruction.Reference] = self._Invoke(instruction.Function, args)
            else:
                raise Exception(f"Unhandled opcode: {opCode}")

class VirtualMachine:
    def __init__(self, program):
        self.__globalScope = Scope()
        for decl in program.Globals.keys():
            self.__globalScope.Declare(decl)

        self.__ctx = ExecutionContext(program.Functions, self.__globalScope)

    def SetGlobal(self, globalVariableName, value):
        self.__globalScope.Declare(globalVariableName, value)

    def GetGlobal(self, globalVariableName):
        return self.__globalScope[globalVariableName]

    def Invoke(self, functionName, **args):
        return self.__ctx.Invoke(functionName, **args)