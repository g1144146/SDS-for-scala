package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}

class ConstantValue(data: Stream, pool: Array[CInfo]) extends AttributeInfo(AttributeType.ConstantValue) {
	private val value: String = extract(data.readShort(), pool)

	def getValue(): String = value
	override def toString(): String = super.toString() + ": " + value
}