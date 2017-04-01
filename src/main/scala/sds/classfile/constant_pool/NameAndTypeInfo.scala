package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class NameAndTypeInfo extends ConstantInfo(ConstantType.NAME_AND_TYPE) {
	private var name: Int = -1
	private var desc: Int = -1

	def getName(): Int = name
	def getDesc(): Int = desc

	override def read(stream: ClassfileStream): Unit = {
		this.name = stream.readShort()
		this.desc = stream.readShort()
	}

	override def toString(): String = super.toString() + "\t#" + name + ":#" + desc
}