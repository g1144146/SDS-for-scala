package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Wide(pc: Int) extends HasReferenceOpcode(MnemonicTable.wide, pc) {
	private var const: Int = -1

	def getConst(): Int = const

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		if(OpcodeInfo(data.readByte(), pc).isInstanceOf[Iinc]) {
			super.read(data, pool)
			return
		}
		this.const = data.readShort()
	}

	override def toString(): String = super.toString() + ", " + const
}