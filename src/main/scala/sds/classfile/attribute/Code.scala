package sds.classfile.attribute

import collection.mutable.{ArrayBuffer => Buffer}
import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo, Utf8Info => Utf8}

class Code(data: Stream, pool: Array[CInfo]) extends AttributeInfo {
	private val maxStack:  Int = data.readShort()
	private val maxLocals: Int = data.readShort()
	private val opcodes: Array[Opcode] = initOpcodes()
	private val exTable: Array[(Array[Int], String)] = (0 until data.readShort()).map((index: Int) => {
		// 0: start, 1: end, 2: handle
		val indexes: Array[Int] = (0 until 3).map((_: Int) => data.readShort()).toArray
		val catchType: Int = data.readShort()
		val target: String = if(catchType == 0) "any" else extract(catchType, pool)
		(indexes, target)
	}).toArray
	private val attributes: Array[AttributeInfo] = (0 until data.readShort()).map((_: Int) => {
		val name: Int = data.readShort()
		val utf8: Utf8 = pool(name - 1).asInstanceOf[Utf8]
		AttributeInfo(utf8.value, data, pool, opcodes)
	}).toArray

	def getMaxStack():  Int = maxStack
	def getMaxLocals(): Int = maxLocals
	def getExTable():    Array[(Array[Int], String)] = exTable
	def getOpcodes():    Array[Opcode]               = opcodes
	def getAttributes(): Array[AttributeInfo]        = attributes

	private def initOpcodes(): Array[Opcode] = {
		val len: Int = data.readInt()
		val pointer: Int = data.getFilePointer().asInstanceOf[Int]
		readOpcode(len, pointer, data, pool, Buffer[Opcode]())
	}

	private def readOpcode(len: Int, pointer: Int, data: Stream, pool: Array[CInfo], buffer: Buffer[Opcode]):
	Array[Opcode] = {
		val index: Int = data.getFilePointer().asInstanceOf[Int]
		if(index >= (len + pointer)) {
			return buffer.toArray
		}
		val pc: Int = (index - pointer)
		val info: Opcode = Opcode(pc, data, pool)
		readOpcode(len, pointer, data, pool, buffer += info)
	}
}