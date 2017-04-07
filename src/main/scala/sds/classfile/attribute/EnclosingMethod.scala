package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.attribute.AttributeType.EnclosingMethod
import sds.classfile.constant_pool.ConstantInfo

class EnclosingMethod(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo(EnclosingMethod) {
	private val _class: String = extract(data.readShort(), pool)
	private val method: String = ((i: Int) => if(i - 1 > 0) extract(i, pool) else "")(data.readShort())

	def getEncClass(): String = _class
	def getMethod(): String = method
	override def toString(): String = super.toString() + ": " + _class + " " + method
}