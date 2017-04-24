package sds.classfile.constant_pool

class MemberInfo(tag: Int, _classIndex: Int, _nameAndType: Int) extends ConstantInfo(tag) {
	def classIndex:  Int = _classIndex
	def nameAndType: Int = _nameAndType
	override def toString(): String = super.toString() + "\t#" + classIndex + ".#" + nameAndType
}