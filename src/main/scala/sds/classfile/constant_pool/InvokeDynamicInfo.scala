package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class InvokeDynamicInfo extends ConstantInfo(ConstantType.INVOKE_DYNAMIC) {
	private var bsmAtt: Int = -1
	private var nameAndType: Int = -1

	def getBsmAtt(): Int = bsmAtt
	def getNameAndType(): Int = nameAndType

	override def read(stream: ClassfileStream): Unit = {
		this.bsmAtt = stream.readShort()
		this.nameAndType = stream.readShort()
	}

	override def toString(): String = super.toString() + "\t#" + bsmAtt + ":#" + nameAndType
}