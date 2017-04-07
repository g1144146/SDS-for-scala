package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get

class MethodParameters extends AttributeInfo(AttributeType.MethodParameters) {
	private var params: Array[Array[String]] = null

	def getParams(): Array[Array[String]] = params

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.params = (0 until data.readShort()).map((_: Int) => {
			val name: String = extract(data.readShort(), pool)
			val access: String = get(data.readShort(), "local")
			Array(access, name)
		}).toArray
	}

	override def toString(): String = {
		val b: sds.util.MultiArgsStringBuilder = new sds.util.MultiArgsStringBuilder(super.toString())
		b.append(": [")
		params.foreach((array: Array[String]) => {
			b.append(array(0), array(1), ", ")
		})
		b.toString()
	}
}