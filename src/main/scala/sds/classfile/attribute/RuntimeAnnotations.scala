package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AnnotationGenerator.generate

class RuntimeAnnotations(t: AttributeType.Value) extends AttributeInfo(t) {
	private var annotations: Array[String] = null

	def getAnnotations(): Array[String] = annotations

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.annotations = (0 until data.readShort()).map((_: Int) => generate(data, pool)).toArray
	}

	override def toString(): String = super.toString() + ": " + annotations.mkString("[", ", " , "]")
}