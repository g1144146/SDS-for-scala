package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.MnemonicTable.OPCODES
import sds.classfile.constant_pool.ConstantInfo

class Wide(data: Stream, pool: Array[ConstantInfo], pc: Int) extends OpcodeInfo("wide", pc) {
	private val tag: Int = data.readByte()
	private val ref: HasReferenceOpcode = new HasReferenceOpcode(data, pool, OPCODES(tag), pc)
	// in case of iinc, readShort().
	private val const: Int = if(tag == 0x84) data.readShort() else -1

	def getConst(): Int =
		if(_type.equals("iinc")) const
		else throw new IllegalStateException("this method must not call because of opcode is not iinc.")
	def getIndex():   Int    = ref.getIndex()
	def getOperand(): String = ref.getOperand()
	def getLdcType(): String = _type match {
		case "ldc"|"ldc_w"|"ldc2_w" => ref.getLdcType()
		case _ => throw new IllegalStateException("this opcode is not ldc(" + _type.toString() + ")")
	}
	override def toString(): String = super.toString() + ref.toString() + ", " + const
}