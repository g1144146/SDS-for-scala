package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream
import sds.classfile.attribute.annotation.{ElementValue => EV}

class ArrayValue(data: ClassfileStream) {
    val values: Array[EV] = (0 until data.short).map((_: Int) => new EV(data)).toArray
}