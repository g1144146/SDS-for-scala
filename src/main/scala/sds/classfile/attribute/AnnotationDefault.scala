package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.attribute.annotation.ElementValue
import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}
import sds.util.AnnotationGenerator.generateFromElementValue

class AnnotationDefault extends AttributeInfo(AttributeType.AnnotationDefault) {
	private var default: String = ""

	def getDefault(): String = default

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.default = generateFromElementValue(new ElementValue(data), pool, new Builder())
	}

	override def toString(): String = super.toString() + ": " + default
}