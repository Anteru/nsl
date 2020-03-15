import math
import struct
import io
from typing import Union, List, BinaryIO
from enum import Enum

class Type(Enum):
	i32 = 0x7F
	i64 = 0x7E
	f32 = 0x7D
	f64 = 0x7C
	function = 0x60
	funcref = 0x70

opcodes = {
	'unreachable': 0x00,
	'end': 0x0B,
	'br': 0x0C,
	'return': 0x0F,
	'call': 0x10,
	'call_indirect': 0x11,
	'local.get': 0x20,
	'local.set': 0x21,
	'local.tee': 0x22,
	'global.get': 0x23,
	'global.set': 0x24,
	'i32.load': 0x28,
	'i32.store': 0x36,
	'memory.size': 0x3F,
	'memory.grow': 0x40,
	'i32.eqz': 0x45,
	'i32.eq': 0x46,
	'i32.ne': 0x47,
	'i32.lt_s': 0x48,
	'i32.lt_u': 0x49,
	'i32.gt_s': 0x4A,
	'i32.gt_u': 0x4B,
	'i32.le_s': 0x4C,
	'i32.le_u': 0x4D,
	'i32.ge_s': 0x4E,
	'i32.ge_u': 0x4F,
}

def PackInteger(v):
	blockCount = math.ceil(v.bit_length() / 7)
	output = []
	for i in range(blockCount):
		b = v & 0x7F
		v >>= 7
		if (i + 1) < blockCount:
			b |= 0b1000_0000
		output.append(b)
	return bytes(output)

def WriteInteger(output: BinaryIO, i: int):
	output.write(PackInteger(i))

def PackFloat(v):
	return struct.pack('<f', v)

def WriteFloat(output: BinaryIO, v: float):
	output.write(PackFloat(v))

def PackString(v):
	return v.encode('utf-8')

def WriteString(output: BinaryIO, s: str):
	output.write(PackString(s))

def PackU32(v):
	return struct.pack('<I', v)

def WriteU32(output: BinaryIO, v: int):
	output.write(PackU32(v))

def PackByte(b: int):
	return struct.pack('B', b)

def WriteByte(output: BinaryIO, b: int):
	output.write(PackByte(b))

def WriteVec(output: BinaryIO,
			 data: Union[bytes, bytearray]):

	length = len(data)
	output.write(PackU32(length))
	output.write(data)

class Section:
	def WriteTo(self, output: BinaryIO):
		pass

class FunctionType:
	def __init__(self, argumentTypes: List[Type], returnTypes: List[Type]):
		self.__argumentTypes = argumentTypes
		self.__returnTypes = returnTypes

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, Type.function.value)
		WriteInteger(output, len(self.__argumentTypes))
		for i in self.__argumentTypes:
			WriteByte(output, i.value)
		WriteInteger(output, len(self.__returnTypes))
		for i in self.__returnTypes:
			WriteByte(output, i.value)

class TypeSection(Section):
	sectionId = 1

	def __init__(self):
		self.__types = []

	def AddType(self, functionType: FunctionType) -> int:
		slot = len(self.__types)
		self.__types.append(functionType)
		return slot

	def WriteTo(self, output: BinaryIO):
		contents = io.BytesIO()
		WriteInteger(contents, len(self.__types))
		for t in self.__types:
			t.WriteTo(contents)

		output.write(PackByte(self.sectionId))
		WriteInteger(output, len(contents.getbuffer()))
		output.write(contents.getbuffer())

class ImportSection(Section):
	sectionId = 2

class FunctionSection(Section):
	sectionId = 3
	def __init__(self):
		self.__indices = []

	def AddFunction(self, index: int):
		self.__indices.append(index)

	def WriteTo(self, output: BinaryIO):
		if not self.__indices:
			return

		WriteByte(output, self.sectionId)
		WriteU32(output, len(self.__indices))
		for i in self.__indices:
			WriteU32(output, i)


class TableSection(Section):
	sectionId = 4

class MemorySection(Section):
	sectionId = 5

class GlobalSection(Section):
	sectionId = 6

class ExportSection(Section):
	sectionId = 7

class StartSection(Section):
	sectionId = 8

class ElementSection(Section):
	sectionId = 9

class CodeSection(Section):
	sectionId = 10
	def __init__(self):
		self.__code = []

	def AddFunction(self, localVariables, body):
		self.__code.append ((localVariables, body,))

	def WriteTo(self, output: BinaryIO):
		if not self.__code:
			return

		WriteByte(output, self.sectionId)
		WriteU32(output, len(self.__code))
		for localVariables, body in self.__code:
			pass

class DataSection(Section):
	sectionId = 11

class Module:
	def __init__(self):
		self.__typesec = TypeSection()
		self.__importsec = ImportSection ()
		self.__funcsec = FunctionSection()
		self.__tablesec = TableSection ()
		self.__memsec = MemorySection ()
		self.__globalsec = GlobalSection ()
		self.__exportsec = ExportSection ()
		self.__startsec = StartSection ()
		self.__elemsec = ElementSection ()
		self.__codesec = CodeSection ()
		self.__datasec = DataSection ()

	def AddFunctionType(self, functionType: FunctionType):
		self.__typesec.AddType(functionType)

	def WriteTo(self, output: BinaryIO):
		# magic
		for b in [0x00, 0x61, 0x73, 0x6D]:
			WriteByte(output, b)
		# version
		for b in [0x01, 0x00, 0x00, 0x00]:
			WriteByte(output, b)
		self.__typesec.WriteTo(output)
		self.__importsec.WriteTo(output)
		self.__funcsec.WriteTo(output)
		self.__tablesec.WriteTo(output)
		self.__memsec.WriteTo(output)
		self.__globalsec.WriteTo(output)
		self.__exportsec.WriteTo(output)
		self.__startsec.WriteTo(output)
		self.__elemsec.WriteTo(output)
		self.__codesec.WriteTo(output)
		self.__datasec.WriteTo(output)

