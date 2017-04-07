package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class MemberInfo(tag: Int, data: ClassfileStream) extends ConstantInfo(tag) {
	private val classIndex:  Int = data.readShort()
	private val nameAndType: Int = data.readShort()

	def getClassIndex():  Int = classIndex
	def getNameAndType(): Int = nameAndType
	override def toString(): String = super.toString() + "\t#" + classIndex + ".#" + nameAndType
}