package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Signature extends AttributeInfo(AttributeType.Signature) {
	private var signature: String = ""

	def getSignature(): String = signature

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.signature = extract(data.readShort(), pool)
	}

	override def toString(): String = super.toString() + ": " + signature
}