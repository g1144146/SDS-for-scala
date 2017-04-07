package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream

class EnumConstValue(data: ClassfileStream) {
	val typeName: Int = data.readShort()
	val constName: Int = data.readShort()

	def getTypeName():  Int = typeName
	def getConstName(): Int = constName
}