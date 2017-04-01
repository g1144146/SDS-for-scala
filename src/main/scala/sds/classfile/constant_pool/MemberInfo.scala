package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class MemberInfo(private val tag: Int) extends ConstantInfo(tag) {
	private var classIndex:  Int = -1
	private var nameAndType: Int = -1

	def getClassIndex():  Int = classIndex
	def getNameAndType(): Int = nameAndType

	override def read(stream: ClassfileStream): Unit = {
		this.classIndex  = stream.readShort()
		this.nameAndType = stream.readShort()
	}

	override def toString(): String = super.toString() + "\t#" + classIndex + ".#" + nameAndType
}