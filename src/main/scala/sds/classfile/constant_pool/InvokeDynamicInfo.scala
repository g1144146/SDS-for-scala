package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class InvokeDynamicInfo(data: ClassfileStream) extends ConstantInfo(ConstantType.INVOKE_DYNAMIC) {
	private val bsmAtt: Int = data.readShort()
	private val nameAndType: Int = data.readShort()

	def getBsmAtt(): Int = bsmAtt
	def getNameAndType(): Int = nameAndType
	override def toString(): String = super.toString() + "\t#" + bsmAtt + ":#" + nameAndType
}