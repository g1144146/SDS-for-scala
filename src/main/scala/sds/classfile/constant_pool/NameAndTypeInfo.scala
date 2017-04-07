package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class NameAndTypeInfo(data: ClassfileStream) extends ConstantInfo(ConstantType.NAME_AND_TYPE) {
	private val name: Int = data.readShort()
	private val desc: Int = data.readShort()

	def getName(): Int = name
	def getDesc(): Int = desc
	override def toString(): String = super.toString() + "\t#" + name + ":#" + desc
}