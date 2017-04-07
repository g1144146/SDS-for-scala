package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class ClassInfo(stream: ClassfileStream) extends ConstantInfo(ConstantType.CLASS) {
	private val index: Int = stream.readShort()

	def getIndex(): Int = index
	override def toString(): String = super.toString() + "\t#" + index
}