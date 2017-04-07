package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream
import sds.classfile.attribute.annotation.{ElementValuePair => Pair}

class Annotation(data: ClassfileStream) {
	val _type: Int = data.readShort()
	val pairs: Array[Pair] = (0 until data.readShort).map((_: Int) => new Pair(data)).toArray

	def getType(): Int = _type
	def getPairs(): Array[Pair] = pairs
}