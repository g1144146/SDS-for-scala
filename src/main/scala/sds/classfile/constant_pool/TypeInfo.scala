package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class TypeInfo extends ConstantInfo(ConstantType.TYPE) {
	private var desc: Int = -1

	def getDesc(): Int = desc
	override def read(stream: ClassfileStream): Unit = desc = stream.readShort()
	override def toString(): String = super.toString + "\t#" + desc
 }