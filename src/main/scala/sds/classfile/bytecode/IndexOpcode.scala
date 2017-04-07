package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class IndexOpcode(_type: MnemonicTable.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	private var index: Int = -1

	def getIndex(): Int = index

	override def read(data: ClassfileStream): Unit = this.index = data.readUnsignedByte()
	override def toString(): String = super.toString() + ": " + index
}