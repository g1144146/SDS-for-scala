package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class Utf8Info(data: ClassfileStream) extends ConstantInfo(ConstantType.UTF8) {
	private val len: Int = data.readShort()
	private val value: String = new String(data.readFully(new Array[Byte](len)), "utf-8")

	def getLen(): Int = len
	def getValue(): String = value
	override def toString(): String = super.toString() + "\t" + value
}