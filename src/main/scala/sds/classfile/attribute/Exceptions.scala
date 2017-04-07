package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Exceptions(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo(AttributeType.Exceptions) {
	private val ex: Array[String] = (0 until data.readShort()).map((_: Int) => {
		extract(data.readShort(), pool).replace("/", ".")
	}).toArray

	def getEx(): Array[String] = ex

	override def toString(): String = super.toString() + ": " + ex.mkString("[", ",", "]")
}