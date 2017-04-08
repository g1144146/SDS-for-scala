package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => S}
import sds.classfile.bytecode.MnemonicTable.invokeinterface
import sds.classfile.constant_pool.{ConstantInfo => C}

class InvokeInterface(data: S, pool: Array[C], pc: Int) extends HasReferenceOpcode(data, pool, invokeinterface, pc) {
	private val count: Int = data.readUnsignedByte()
	data.skipBytes(1)

	def getCount(): Int = count
	override def toString(): String = super.toString() + ", " + count
}