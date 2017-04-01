package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class ClassInfo extends ConstantInfo(ConstantType.CLASS) {
	private var index: Int = -1

	def getIndex(): Int = index

	override def read(stream: ClassfileStream): Unit = this.index = stream.readShort()
	override def toString(): String = super.toString() + "\t#" + index
}