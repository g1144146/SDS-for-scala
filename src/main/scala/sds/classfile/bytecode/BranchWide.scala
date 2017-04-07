package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class BranchWide(_type: MnemonicTable.Value, pc: Int) extends BranchOpcode(_type, pc) {
	override def read(data: ClassfileStream): Unit = this.branch = data.readInt()
}