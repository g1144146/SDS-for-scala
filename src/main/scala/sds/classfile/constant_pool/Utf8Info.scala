package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class Utf8Info extends ConstantInfo(ConstantType.UTF8) {
	private var len: Int = -1
	private var value: String = ""

	def getLen(): Int = len
	def getValue(): String = value

	override def read(stream: ClassfileStream): Unit = {
		this.len = stream.readShort()
		this.value = new String(stream.readFully(new Array[Byte](len)), "utf-8")
	}

	override def toString(): String = super.toString() + "\t" + value
}