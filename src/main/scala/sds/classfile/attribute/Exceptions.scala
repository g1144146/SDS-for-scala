package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Exceptions extends AttributeInfo(AttributeType.Exceptions) {
	private var ex: Array[String] = null

	def getEx(): Array[String] = ex

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.ex = (0 until data.readShort()).map((_: Int) => {
			extract(data.readShort(), pool).replace("/", ".")
		}).toArray
	}

	override def toString(): String = super.toString() + ": " + ex.mkString("[", ",", "]")
}