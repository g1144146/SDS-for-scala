package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream
import sds.classfile.attribute.annotation.{ElementValue => EV}

class ArrayValue(data: ClassfileStream) {
	private val values: Array[EV] = (0 until data.readShort()).map((_: Int) => new EV(data)).toArray

	def getValues(): Array[EV] = values
}