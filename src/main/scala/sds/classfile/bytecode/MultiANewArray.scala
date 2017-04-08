package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => S}
import sds.classfile.bytecode.MnemonicTable.multianewarray
import sds.classfile.constant_pool.{ConstantInfo => C}
import sds.util.DescriptorParser.parse

class MultiANewArray(data: S, pool: Array[C], pc: Int) extends HasReferenceOpcode(data, pool, multianewarray, pc) {
	private val dimensions: Int = data.readByte()

	def getDimensions(): Int = dimensions
	override def getOperand(): String = parse(operand, false)
	override def toString(): String = super.toString() + ", " + dimensions
}