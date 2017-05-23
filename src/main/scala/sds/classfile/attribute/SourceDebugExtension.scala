package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}

class SourceDebugExtension(data: Stream, len: Int) extends AttributeInfo {
    val debug: Array[Int] = (0 until len).map((_: Int) => data.unsignedByte).toArray
    override def toString(): String = "SourceDebugExtension: " + debug.mkString("[", ", ", "]")
}