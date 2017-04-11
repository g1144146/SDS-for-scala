package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.DescriptorParser.parse

class Signature(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
	private val signature: String = parse(extract(data.readShort(), pool), true)

	def getSignature(): String = signature
	override def toString(): String = super.toString() + ": " + signature
}