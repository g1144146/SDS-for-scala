package sds.classfile.bytecode

class BranchOpcode(_branch: Int, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
	def branch: Int = _branch
	override def toString(): String = super.toString() + ": " + (branch + pc)
}