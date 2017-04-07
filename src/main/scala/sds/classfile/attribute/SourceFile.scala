package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class SourceFile extends AttributeInfo(AttributeType.SourceFile) {
	private var file: String = ""

	def getFile(): String = file

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.file = extract(data.readShort(), pool)
	}

	override def toString(): String = super.toString() + ": " + file
}