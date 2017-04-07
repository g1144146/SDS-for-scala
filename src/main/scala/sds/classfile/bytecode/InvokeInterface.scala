package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class InvokeInterface(data: ClassfileStream, pool: Array[ConstantInfo], pc: Int) extends
HasReferenceOpcode(data, pool, MnemonicTable.invokeinterface, pc) {
	private val count: Int = data.readUnsignedByte()
	data.skipBytes(1)

	def getCount(): Int = count

	override def toString(): String = super.toString() + ", " + count
}