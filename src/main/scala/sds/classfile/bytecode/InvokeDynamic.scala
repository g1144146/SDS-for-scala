package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class InvokeDynamic(data: ClassfileStream, pool: Array[ConstantInfo], pc: Int) extends
HasReferenceOpcode(data, pool, MnemonicTable.inovokedynamic, pc) {
	data.skipBytes(2)
}