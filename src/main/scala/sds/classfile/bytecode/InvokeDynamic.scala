package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => C}

class InvokeDynamic(data: Stream, p: Array[C], pc: Int)
extends HasReferenceOpcode(data.readShort(), p, "invokedynamic", pc) {
	data.skipBytes(2)
}