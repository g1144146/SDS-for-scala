package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.bytecode.{MnemonicTable => Table}
import sds.classfile.constant_pool.ConstantInfo

class Wide(data: ClassfileStream, pool: Array[ConstantInfo], pc: Int) extends OpcodeInfo(MnemonicTable.wide, pc) {
	private var const: Int = -1
	private var ref: HasReferenceOpcode = null
	init()

	def getConst(): Int = const
	def getIndex(): Int = ref.getIndex()
	def getOperand(): String = ref.getOperand()
	def getLdcType(): String = _type match {
		case Table.ldc | Table.ldc_w | Table.ldc2_w => ref.getLdcType()
		case _ => throw new IllegalStateException("this opcode is not ldc(" + _type.toString() + ")")
	}

	def init(): Unit = {
		if(data.readByte() == 0x84) {
			// in case of Iinc
			this.ref = new HasReferenceOpcode(data, pool, Table.iinc, pc)
			return
		}
//		if(OpcodeInfo(data.readByte(), pc).isInstanceOf[Iinc]) {
//			super.read(data, pool)
//			return
//		}
		this.const = data.readShort()
	}

	override def toString(): String = super.toString() + const + ", (" + ref.toString() + ")"
}