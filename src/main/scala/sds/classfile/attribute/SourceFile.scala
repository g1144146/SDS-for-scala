package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class SourceFile(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
	private val file: String = extract(data.readShort(), pool)

	def getFile(): String = file
	override def toString(): String = super.toString() + ": " + file
}