package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.MnemonicTable.OPCODES
import sds.classfile.constant_pool.ConstantInfo

class Wide(data: Stream, pool: Array[ConstantInfo], pc: Int) extends OpcodeInfo("wide", pc) {
    private val tag: Int = data.byte
    private val ref: HasReferenceOpcode = if(OPCODES(tag) == "ldc")
        new HasReferenceOpcode(data.unsignedByte, pool, OPCODES(tag), pc)
    else 
        new HasReferenceOpcode(data.short,        pool, OPCODES(tag), pc)
    // in case of iinc, short.
    val const: Int = if(tag == 0x84) data.short else -1

    def index:      Int    = ref.index
    def operand:    String = ref.operand
    def ldcType:    String = ref.ldcType
    override def toString(): String = s"${super.toString()}${ref.toString()}, $const"
}