package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.bytecode.MnemonicTable.bipush

class PushOpcode(data: ClassfileStream, _type: MnemonicTable.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	private val value: Int = if(_type == bipush) data.readByte() else data.readShort()

	def getValue(): Int = value

	override def toString(): String = super.toString() + ": " + value
}