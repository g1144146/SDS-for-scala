package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class BranchOpcode(_type: MnemonicTable.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	protected var branch: Int = -1

	def getBranch(): Int = branch

	override def read(data: ClassfileStream): Unit = this.branch = data.readShort()
	override def toString(): String = super.toString() + ": " + (branch + pc)
}