package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class InvokeDynamic(pc: Int) extends HasReferenceOpcode(MnemonicTable.inovokedynamic, pc) {
	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		super.read(data, pool)
		data.skipBytes(2)
	}
}