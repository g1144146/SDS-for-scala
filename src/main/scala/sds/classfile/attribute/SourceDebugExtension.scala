package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class SourceDebugExtension(private val len: Int) extends AttributeInfo(AttributeType.SourceDebugExtension) {
	private val debug: Array[Int] = new Array(len)

	def getDebug(): Array[Int] = debug

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		(0 until len).foreach(debug(_) = data.readUnsignedByte())
	}

	override def toString(): String = super.toString() + ": " + debug.mkString("[", ", ", "]")
}