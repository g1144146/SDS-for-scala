package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.AnnotationGenerator.generate

class RuntimeAnnotations(data: Stream, pool: Array[CInfo], name: String) extends AttributeInfo {
    val annotations: Array[String] = (0 until data.short).map((_: Int) => generate(data, pool)).toArray
}