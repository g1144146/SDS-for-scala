package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}

class SourceDebugExtension(data: Stream, len: Int) extends AttributeInfo {
    private val debug: Array[Int] = (0 until len).map((_: Int) => data.readUnsignedByte()).toArray

    def getDebug(): Array[Int] = debug
    override def toString(): String = super.toString() + ": " + debug.mkString("[", ", ", "]")
}