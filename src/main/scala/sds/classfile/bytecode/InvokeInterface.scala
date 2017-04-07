package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class InvokeInterface(pc: Int) extends HasReferenceOpcode(MnemonicTable.invokeinterface, pc) {
	private var count: Int = -1

	def getCount(): Int = count

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		super.read(data, pool)
		this.count = data.readUnsignedByte()
		data.skipBytes(1)
	}

	override def toString(): String = super.toString() + ", " + count
}