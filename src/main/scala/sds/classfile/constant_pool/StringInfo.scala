package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class StringInfo(data: ClassfileStream) extends ConstantInfo(ConstantType.STRING) {
	private val string: Int = data.readShort()

	def getString(): Int = string
	override def toString(): String = super.toString + "\t#" + string
}