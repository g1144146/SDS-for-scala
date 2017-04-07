package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class ConstantValue extends AttributeInfo(AttributeType.ConstantValue) {
	private var value: String = null

	def getValue(): String = value

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.value = extract(data.readShort(), pool)
	}

	override def toString(): String = super.toString() + ": " + value
}