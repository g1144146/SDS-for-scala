package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.MnemonicTable.invokedynamic
import sds.classfile.constant_pool.{ConstantInfo => C}

class InvokeDynamic(data: Stream, pool: Array[C], pc: Int) extends HasReferenceOpcode(data, pool, invokedynamic, pc) {
	data.skipBytes(2)
}