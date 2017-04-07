package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class TypeInfo(data: ClassfileStream) extends ConstantInfo(ConstantType.TYPE) {
	private val desc: Int = data.readShort()

	def getDesc(): Int = desc
	override def toString(): String = super.toString + "\t#" + desc
 }