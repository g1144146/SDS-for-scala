package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get

class MethodParameters(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
	private val params: Array[Array[String]] = (0 until data.readShort()).map((_: Int) => {
		val name: String = extract(data.readShort(), pool)
		val access: String = get(data.readShort(), "local")
		Array(access, name)
	}).toArray

	def getParams(): Array[Array[String]] = params
	override def toString(): String = {
		val b: sds.util.MultiArgsStringBuilder = new sds.util.MultiArgsStringBuilder(super.toString())
		b.append(": [")
		params.foreach((array: Array[String]) => {
			b.append(array(0), array(1), ", ")
		})
		b.toString()
	}
}