package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Signature(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo(AttributeType.Signature) {
	private val signature: String = extract(data.readShort(), pool)

	def getSignature(): String = signature
	override def toString(): String = super.toString() + ": " + signature
}