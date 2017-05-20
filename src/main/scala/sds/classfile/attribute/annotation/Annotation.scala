package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream
import sds.classfile.attribute.annotation.{ElementValuePair => Pair}

class Annotation(data: ClassfileStream) {
    val _type: Int = data.short
    val pairs: Array[Pair] = (0 until data.short).map((_: Int) => new Pair(data)).toArray
}