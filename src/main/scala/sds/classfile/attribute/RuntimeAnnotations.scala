package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.util.AnnotationGenerator.generate

class RuntimeAnnotations(data: Stream, pool: Array[CInfo], t: AttributeType.Value) extends AttributeInfo(t) {
	private val annotations: Array[String] = (0 until data.readShort()).map((_: Int) => generate(data, pool)).toArray

	def getAnnotations(): Array[String] = annotations
	override def toString(): String = super.toString() + ": " + annotations.mkString("[", ", " , "]")
}