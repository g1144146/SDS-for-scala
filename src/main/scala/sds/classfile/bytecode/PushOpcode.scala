package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class PushOpcode(data: ClassfileStream, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
	private val value: Int = if(_type.equals("bipush")) data.readByte() else data.readShort()

	def getValue(): Int = value
	override def toString(): String = super.toString() + ": " + value
}