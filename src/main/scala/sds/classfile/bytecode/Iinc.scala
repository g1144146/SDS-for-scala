package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class Iinc(data: ClassfileStream, pc: Int) extends OpcodeInfo("iinc", pc) {
	private val index: Int = data.readUnsignedByte()
	private val const: Int = data.readByte()

	def getIndex(): Int = index
	def getConst(): Int = const
	override def toString(): String = super.toString() + ": " + index + ", " + const
}