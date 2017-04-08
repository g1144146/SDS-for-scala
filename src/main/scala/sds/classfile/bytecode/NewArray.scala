package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class NewArray(data: ClassfileStream, pc: Int) extends OpcodeInfo("newarray", pc) {
	private val atype: String = data.readUnsignedByte() match {
		case 4  => "boolean"
		case 5  => "char"
		case 6  => "float"
		case 7  => "double"
		case 8  => "byte"
		case 9  => "short"
		case 10 => "int"
		case 11 => "long"
		case _  => throw new RuntimeException("unknown type.")
	}

	def getAType(): String = atype
	override def toString(): String = super.toString() + ": " + atype
}