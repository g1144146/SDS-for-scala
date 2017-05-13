package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.AnnotationGenerator.generate

class RuntimeParameterAnnotations(data: Stream, pool: Array[CInfo], private val name: String) extends AttributeInfo {
    private val annotations: Array[Array[String]] = (0 until data.readByte()).map((x: Int) => {
        (0 until data.readShort()).map((y: Int) => generate(data, pool)).toArray
    }).toArray

    def getAnnotations(): Array[Array[String]] = annotations
}