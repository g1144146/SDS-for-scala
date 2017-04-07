package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.DescriptorParser.parse

class MultiANewArray(pc: Int) extends HasReferenceOpcode(MnemonicTable.invokeinterface, pc) {
	private var dimensions: Int = -1

	def getDimensions(): Int = dimensions

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		super.read(data, pool)
		this.dimensions = data.readByte()
		this.operand = parse(operand, false)
	}

	override def toString(): String = super.toString() + ", " + dimensions
}