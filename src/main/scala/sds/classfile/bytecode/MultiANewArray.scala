package sds.classfile.bytecode

import sds.classfile.bytecode.{HasReferenceOpcode => Has}
import sds.classfile.constant_pool.{ConstantInfo => C}
import sds.util.DescriptorParser.parse

class MultiANewArray(dim: Int, index: Int, pool: Array[C], pc: Int) extends Has(index, pool, "multianewarray", pc) {
    def dimensions: Int = dim
    override def operand: String = parse(super.operand, false)
    override def toString(): String = super.toString() + ", " + dimensions
}