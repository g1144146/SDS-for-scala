package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class StringInfo extends ConstantInfo(ConstantType.STRING) {
	private var string: Int = -1

	def getString(): Int = string
	override def read(stream: ClassfileStream): Unit = this.string = stream.readShort()
	override def toString(): String = super.toString + "\t#" + string
}