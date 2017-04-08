package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}

import sds.classfile.bytecode.{MnemonicTable => T}
import sds.classfile.constant_pool.ConstantType.{DOUBLE, FLOAT, INTEGER, LONG, STRING, CLASS}
import sds.util.{MultiArgsStringBuilder => Builder}

class HasReferenceOpcode(data: Stream, pool: Array[CInfo], _type: T.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	protected val index: Int = if(_type == T.ldc) data.readUnsignedByte() else data.readShort()
	private val ldcType: String = if(Set(T.ldc, T.ldc_w, T.ldc2_w).contains(_type)) {
		pool(index - 1).getTag() match {
			case DOUBLE  => "double"
			case FLOAT   => "float"
			case INTEGER => "int"
			case LONG    => "long"
			case STRING  => "String"
			case CLASS   => extract(index, pool)
			case _       => ""
		}
	} else ""
	protected val operand: String = if(ldcType.equals("String")) {
		"\"" + Operand.get(this, pool) + "\""
	} else Operand.get(this, pool)

	def getIndex(): Int = index
	def getOperand(): String = operand
	def getLdcType(): String = _type match {
		case T.ldc | T.ldc_w | T.ldc2_w => ldcType
		case _ => throw new IllegalStateException("this opcode is not ldc(" + _type.toString() + ")")
	}

	override def toString(): String = {
		val b: Builder = new Builder(super.toString());
		b.append(": #", index, "(", getOperand());
		if(ldcType.length > 0) {
			b.append("(", ldcType, ")");
		}
		b.append(")")
		b.toString()
	}
}