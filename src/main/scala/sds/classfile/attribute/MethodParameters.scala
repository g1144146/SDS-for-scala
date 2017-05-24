package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get

class MethodParameters(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
    val params: Array[Array[String]] = (0 until data.byte).map((_: Int) => {
        val name: String = extract(data.short, pool)
        val access: String = get(data.short, "local")
        Array(access, name)
    }).toArray

    override def toString(): String = s"MethodParameters: [${params.map(_.mkString).mkString(", ")}]"
}