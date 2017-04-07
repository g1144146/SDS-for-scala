package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class Iinc(pc: Int) extends OpcodeInfo(MnemonicTable.iinc, pc) {
	private var index: Int = -1
	private var const: Int = -1

	def getIndex(): Int = index
	def getConst(): Int = const

	override def read(data: ClassfileStream): Unit = {
		this.index = data.readUnsignedByte()
		this.const = data.readByte()
	}

	override def toString(): String = super.toString() + ": " + index + ", " + const
}