package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get
import sds.util.{MultiArgsStringBuilder => Builder}

class MethodParameters(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
    val params: Array[Array[String]] = (0 until data.short).map((_: Int) => {
        val name: String = extract(data.short, pool)
        val access: String = get(data.short, "local")
        Array(access, name)
    }).toArray

    override def toString(): String = {
        val b: Builder = new Builder("MethodParameters: [")
        params.foreach((array: Array[String]) => {
            b.append(array(0), array(1), ", ")
        })
        b.append("]")
        b.toString()
    }
}