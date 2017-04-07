package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class HandleInfo(data: ClassfileStream) extends ConstantInfo(ConstantType.HANDLE) {
	private val refKind:  Int = data.readByte()
	private val refIndex: Int = data.readShort()

	def getKind():  Int = refKind
	def getIndex(): Int = refIndex
	def getKindValue(): String = refKind match {
		case 1 => "REF_getField"
		case 2 => "REF_getStatic"
		case 3 => "REF_putField"
		case 4 => "REF_putStatic"
		case 5 => "REF_invokeVirtual"
		case 6 => "REF_invokeStatic"
		case 7 => "REF_invokeSpecial"
		case 8 => "REF_newInvokeSpecial"
		case 9 => "REF_invokeInterface"
		case _ => throw new IllegalStateException("reference kind index is invalid value(" + refKind + ")")
	}
	override def toString(): String = super.toString + "\t" + getKindValue() + ":#" + refIndex
}