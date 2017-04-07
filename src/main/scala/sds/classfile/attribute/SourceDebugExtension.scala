package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.AttributeType.SourceDebugExtension

class SourceDebugExtension(data: Stream, len: Int) extends AttributeInfo(SourceDebugExtension) {
	private val debug: Array[Int] = (0 until len).map((_: Int) => data.readUnsignedByte()).toArray

	def getDebug(): Array[Int] = debug
	override def toString(): String = super.toString() + ": " + debug.mkString("[", ", ", "]")
}