package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class IndexOpcode(data: ClassfileStream, _type: MnemonicTable.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	private val index: Int = data.readUnsignedByte()

	def getIndex(): Int = index

	override def toString(): String = super.toString() + ": " + index
}