package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.DescriptorParser.parse

class MultiANewArray(data: ClassfileStream, pool: Array[ConstantInfo], pc: Int) extends
HasReferenceOpcode(data, pool, MnemonicTable.invokeinterface, pc) {
	private val dimensions: Int = data.readByte()

	def getDimensions(): Int = dimensions

	override def getOperand(): String = {
		parse(operand, false)
	}

	override def toString(): String = super.toString() + ", " + dimensions
}