package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AnnotationGenerator.generate

class RuntimeParameterAnnotations(t: AttributeType.Value) extends AttributeInfo(t) {
	private var annotations: Array[Array[String]] = null

	def getAnnotatinos(): Array[Array[String]] = annotations

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.annotations = (0 until data.readByte()).map((x: Int) => {
			(0 until data.readShort()).map((y: Int) => generate(data, pool)).toArray
		}).toArray
	}

	override def toString(): String = {
		super.toString() + ": [" + annotations.map(_.mkString("{", ",", "}")).toArray.mkString("_") + "]"
	}
}