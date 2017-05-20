package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Exceptions(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
    val ex: Array[String] = (0 until data.short).map((_: Int) => {
        extract(data.short, pool).replace("/", ".")
    }).toArray
    override def toString(): String = super.toString() + ": " + ex.mkString("[", ",", "]")
}