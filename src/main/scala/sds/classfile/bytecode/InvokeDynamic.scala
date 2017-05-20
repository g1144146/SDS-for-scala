package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => C}
import sds.classfile.bytecode.{HasReferenceOpcode => Has}

class InvokeDynamic(data: Stream, p: Array[C], pc: Int) extends Has(data.short, p, "invokedynamic", pc) {
    data.skip(2)
}