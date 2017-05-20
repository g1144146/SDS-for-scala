package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => S}
import sds.classfile.constant_pool.{ConstantInfo => C}
import sds.classfile.bytecode.{HasReferenceOpcode => Has}

class InvokeInterface(data: S, p: Array[C], pc: Int) extends Has(data.short, p, "invokeinterface", pc) {
    val count: Int = data.unsignedByte
    data.skip(1)

    override def toString(): String = s"${super.toString()}, $count"
}