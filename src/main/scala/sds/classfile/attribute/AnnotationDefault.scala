package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.annotation.ElementValue
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.util.{MultiArgsStringBuilder => Builder}
import sds.classfile.attribute.AnnotationGenerator.generateFromElementValue

class AnnotationDefault(data: Stream, pool: Array[CInfo]) extends AttributeInfo {
	private val default: String = generateFromElementValue(new ElementValue(data), pool, new Builder())

	def getDefault(): String = default
	override def toString(): String = super.toString() + ": " + default
}