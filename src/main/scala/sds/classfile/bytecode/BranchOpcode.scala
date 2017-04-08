package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class BranchOpcode(data: ClassfileStream, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
	protected var branch: Int = if(this.isInstanceOf[BranchWide]) data.readInt() else data.readShort()

	def getBranch(): Int = branch
	override def toString(): String = super.toString() + ": " + (branch + pc)
}