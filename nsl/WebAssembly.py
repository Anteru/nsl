import math
import struct
import io
from typing import Optional, Union, List, BinaryIO
from enum import Enum

class Type:
	def WriteTo(self, output: BinaryIO):
		...

class ValueType(Type, Enum):
	i32 = 0x7F
	i64 = 0x7E
	f32 = 0x7D
	f64 = 0x7C
	function = 0x60
	funcref = 0x70

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, self.value)

class HeapType(Type, Enum):
	none = 0x71

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, self.value)

class StructType:
	def __init__(self, fields: [Type]):
		self.__fields = fields

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, 0x5F)
		WriteInteger(output, len(self.__fields))
		for field in self.__fields:
			field.WriteTo(output)


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
	'i32.const': 0x41,
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
	'i32.add': 0x6A,
	'i32.sub': 0x6B,
	'i32.mul': 0x6C,
	'i32.div_s': 0x6D,
	'i32.div_u': 0x6E,

	'f32.add': 0x92,
	'f32.sub': 0x93,
	'f32.mul': 0x94,
	'f32.div': 0x95
}

def PackInteger(v):
	if v == 0:
		return bytes([0])
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
	b = PackString(s)
	WriteInteger(output, len(b))
	output.write(b)

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

	@property
	def Arguments(self):
		return self.__argumentTypes

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, ValueType.function.value)
		WriteInteger(output, len(self.__argumentTypes))
		for i in self.__argumentTypes:
			i.WriteTo(output)
		WriteInteger(output, len(self.__returnTypes))
		for i in self.__returnTypes:
			i.WriteTo(output)

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

		WriteByte(output, self.sectionId)
		WriteInteger(output, len(contents.getbuffer()))
		output.write(contents.getbuffer())

class ImportSection(Section):
	sectionId = 2

class FunctionSection(Section):
	sectionId = 3
	def __init__(self):
		self.__indices = []

	def AddFunction(self, index: int):
		slot = len(self.__indices)
		self.__indices.append(index)
		return slot

	def WriteTo(self, output: BinaryIO):
		if not self.__indices:
			return

		content = io.BytesIO()
		WriteInteger(content, len(self.__indices))
		for i in self.__indices:
			WriteInteger(content, i)

		WriteByte(output, self.sectionId)
		WriteInteger(output, len(content.getbuffer()))
		output.write(content.getbuffer())

class Table:
	def __init__(self, size: int, valueType: ValueType = ValueType.funcref):
		self.__size = size
		self.__valueType = valueType

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, self.__valueType.value)
		WriteByte(output, 0x00)
		WriteInteger(output, self.__size)

class TableSection(Section):
	sectionId = 4

	def __init__(self):
		self.__tables = []

	def Add(self, table: Table):
		self.__tables.append(table)

	def WriteTo(self, output: BinaryIO):
		if not self.__tables:
			return

		content = io.BytesIO()
		WriteInteger(content, len(self.__tables))
		for table in self.__tables:
			table.WriteTo(content)

		WriteByte(output, self.sectionId)
		WriteInteger(output, len(content.getbuffer()))
		output.write(content.getbuffer())

class Memory:
	def __init__(self, minSize: int = 0, maxSize: Optional[int] = None):
		self.__min = minSize
		self.__max = maxSize

	def WriteTo(self, output: BinaryIO):
		if self.__max:
			WriteByte(output, 0x01)
			WriteInteger(output, self.__min)
			WriteInteger(output, self.__max)
		else:
			WriteByte(output, 0x00)
			WriteInteger(output, self.__min)

class MemorySection(Section):
	sectionId = 5

	def __init__(self):
		self.__memories = []

	def Add(self, memory: Memory):
		self.__memories.append(memory)

	def WriteTo(self, output: BinaryIO):
		if not self.__memories:
			return

		content = io.BytesIO()
		WriteInteger(content, len(self.__memories))
		for memory in self.__memories:
			memory.WriteTo(content)

		WriteByte(output, self.sectionId)
		WriteInteger(output, len(content.getbuffer()))
		output.write(content.getbuffer())

class GlobalSection(Section):
	sectionId = 6

class ExternalKind(Enum):
	Function = 0x00

class Export:
	def __init__(self, index: int, name: str,
				 kind: ExternalKind = ExternalKind.Function):
		self.__index =  index
		self.__name = name
		self.__kind = kind

	def WriteTo(self, output: BinaryIO):
		WriteString(output, self.__name)
		WriteByte(output, self.__kind.value)
		WriteInteger(output, self.__index)

class ExportSection(Section):
	sectionId = 7

	def __init__(self):
		self.__exports = []

	def Add(self, export: Export):
		self.__exports.append(export)

	def WriteTo(self, output: BinaryIO):
		if not self.__exports:
			return

		content = io.BytesIO()
		WriteInteger(content, len(self.__exports))
		for export in self.__exports:
			export.WriteTo(content)

		WriteByte(output, self.sectionId)
		WriteInteger(output, len(content.getbuffer()))
		output.write(content.getbuffer())

class StartSection(Section):
	sectionId = 8

class ElementSection(Section):
	sectionId = 9

class Local:
	def __init__(self, valueType: ValueType, count: int = 1):
		self.__n = count
		self.__valueType = valueType

	def WriteTo(self, output: BinaryIO):
		WriteInteger(output, self.__n)
		self.__valueType.WriteTo(output)

	@property
	def Type(self):
		return self.__valueType

	@property
	def Count(self):
		return self.__n

	def SetCount(self, n):
		assert n > 0
		self.__n = n

class Instruction:
	def __init__(self, opcode: int, args = None):
		self.__opcode = opcode
		self.__args = args

	def WriteTo(self, output: BinaryIO):
		WriteByte(output, self.__opcode)
		# TODO Handle non-integer arguments
		if self.__args:
			for arg in self.__args:
				WriteInteger(output, arg)

class Code:
	def __init__(self):
		self.__locals = []
		self.__instructions = []
		self.__lastLocal = None
		self.__lastLocalIndex = -1

	def AddLocal(self, local: Local):
		# If we're adding another local of the same type, we simply increment
		# the last local so we avoid generating tons of repeated locals
		if self.__lastLocal:
			if self.__lastLocal.Type == local.Type:
				self.__lastLocal.SetCount(
					self.__lastLocal.Count + local.Count
				) 
		else:
			self.__locals.append(local)
			self.__lastLocal = local
			
		self.__lastLocalIndex += local.Count
		return self.__lastLocalIndex

	def AddInstruction(self, instruction: Instruction):
		self.__instructions.append(instruction)

	def Encode(self):
		content = io.BytesIO()
		WriteInteger(content, len(self.__locals))
		for local in self.__locals:
			local.WriteTo(content)

		for instruction in self.__instructions:
			instruction.WriteTo(content)
		WriteByte(content, 0x0b)

		return content.getbuffer()

class CodeSection(Section):
	sectionId = 10
	def __init__(self):
		self.__code = []

	def Add(self, code: Code):
		self.__code.append (code)

	def WriteTo(self, output: BinaryIO):
		if not self.__code:
			return

		content = io.BytesIO()
		WriteInteger(content, len(self.__code))
		for code in self.__code:
			codeContent = code.Encode()
			WriteInteger(content, len(codeContent))
			content.write(codeContent)

		WriteByte(output, self.sectionId)
		WriteInteger(output, len(content.getbuffer()))
		output.write(content.getbuffer())

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

	def AddFunctionType(self, functionType: FunctionType) -> int:
		return self.__typesec.AddType(functionType)

	def AddFunction(self, typeIndex: int) -> int:
		return self.__funcsec.AddFunction(typeIndex)

	def AddExport(self, export: Export):
		self.__exportsec.Add(export)

	def AddTable(self, table: Table):
		self.__tablesec.Add(table)

	def AddCode(self, code: Code):
		self.__codesec.Add(code)

	def AddMemory(self, memory: Memory):
		self.__memsec.Add(memory)

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

