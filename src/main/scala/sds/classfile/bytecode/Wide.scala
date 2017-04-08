package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.bytecode.{MnemonicTable => Table}
import sds.classfile.constant_pool.ConstantInfo

class Wide(data: ClassfileStream, pool: Array[ConstantInfo], pc: Int) extends OpcodeInfo(Table.wide, pc) {
	private val tag: Int = data.readByte()
	private val ref: HasReferenceOpcode = new HasReferenceOpcode(data, pool, Table(tag), pc)
	// in case of iinc, readShort().
	private val const: Int = if(tag == 0x84) data.readShort() else -1

	def getConst(): Int =
		if(_type == Table.iinc) const
		else throw new IllegalStateException("this method must not call because of opcode is not iinc.")
	def getIndex():   Int    = ref.getIndex()
	def getOperand(): String = ref.getOperand()
	def getLdcType(): String = _type match {
		case Table.ldc | Table.ldc_w | Table.ldc2_w => ref.getLdcType()
		case _ => throw new IllegalStateException("this opcode is not ldc(" + _type.toString() + ")")
	}
	override def toString(): String = super.toString() + ref.toString() + ", " + const
}