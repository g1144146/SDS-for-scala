package sds.classfile.attribute

import collection.mutable.{ArrayBuffer => Buffer}
import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo, Utf8Info => Utf8}


class Code extends AttributeInfo(AttributeType.Code) {
	private var maxStack:  Int = -1
	private var maxLocals: Int = -1
	private var opcodes: Array[Opcode] = null
	private var exTable: Array[Array[Int]] = null
	private var exTarget: Array[String] = null
	private var attributes: Array[AttributeInfo] = null

	def getMaxStack():  Int = maxStack
	def getMaxLocals(): Int = maxLocals
	def getExTable():    Array[Array[Int]]    = exTable
	def getOpcodes():    Array[Opcode]        = opcodes
	def getExTarget():   Array[String]        = exTarget
	def getAttributes(): Array[AttributeInfo] = attributes

	override def read(data: Stream, pool: Array[CInfo]): Unit = {
		this.maxStack  = data.readShort()
		this.maxLocals = data.readShort()

		val len: Int = data.readInt()
		val pointer: Int = data.getFilePointer().asInstanceOf[Int]
		this.opcodes = readOpcode(len, pointer, data, pool, Buffer[Opcode]())

		val exSize: Int = data.readShort()
		this.exTable = new Array(exSize)
		this.exTarget = new Array(exSize)
		(0 until exSize).foreach((index: Int) => {
			// 0: start, 1: end, 2: handle
			exTable(index) = (0 until 3).map((_: Int) => data.readShort()).toArray
			val catchType: Int = data.readShort()
			exTarget(index) = if(catchType == 0) "any" else extract(catchType, pool)
		})
		
		this.attributes = ((size: Int) => (0 until size).map((_: Int) => {
			val name: Int = data.readShort()
			val utf8: Utf8 = pool(name - 1).asInstanceOf[Utf8]
			val info: AttributeInfo = AttributeInfo(utf8.getValue(), data.readInt())
			if(info.isInstanceOf[StackMapTable]) {
				info.asInstanceOf[StackMapTable].read(data, pool, opcodes) 
			} else {
				info.read(data, pool)
			}
			info
		}).toArray)(data.readShort())
	}

	private def readOpcode(len: Int, pointer: Int, data: Stream, pool: Array[CInfo], buffer: Buffer[Opcode]):
	Array[Opcode] = {
		val index: Int = data.getFilePointer().asInstanceOf[Int]
		if(index >= (len + pointer)) {
			return buffer.toArray
		}
		val pc: Int = (index - pointer)
		val info: Opcode = Opcode(data.readByte() & 0xff, pc)
		info.read(data, pool)
		readOpcode(len, pointer, data, pool, buffer += info)
	}
}