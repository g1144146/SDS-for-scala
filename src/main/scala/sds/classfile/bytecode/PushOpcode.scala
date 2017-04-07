package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.bytecode.MnemonicTable.bipush

class PushOpcode(_type: MnemonicTable.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	private var value: Int = -1

	def getValue(): Int = value

	override def read(data: ClassfileStream): Unit = {
		this.value = if(_type == bipush) data.readByte() else data.readShort()
	}
	override def toString(): String = super.toString() + ": " + value
}