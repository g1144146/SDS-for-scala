package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

import sds.classfile.bytecode.{MnemonicTable => Table}
import sds.classfile.constant_pool.ConstantType.{DOUBLE, FLOAT, INTEGER, LONG, STRING, CLASS}
import sds.util.{MultiArgsStringBuilder => Builder}

class HasReferenceOpcode(_type: MnemonicTable.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	protected var index: Int = -1
	protected var operand: String = ""
	private   var ldcType: String = ""

	def getIndex(): Int = index
	def getOperand(): String = operand
	def getLdcType(): String = _type match {
		case Table.ldc | Table.ldc_w | Table.ldc2_w => ldcType
		case _ => throw new IllegalStateException("this opcode is not ldc(" + _type.toString() + ")")
	}

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.index = if(_type == Table.ldc) data.readUnsignedByte() else data.readShort()
		this.operand = Operand.get(this, pool)
		if(Set(Table.ldc, Table.ldc_w, Table.ldc2_w).contains(_type)) {
			this.ldcType = pool(index - 1).getTag() match {
				case DOUBLE  => "double"
				case FLOAT   => "float"
				case INTEGER => "int"
				case LONG    => "long"
				case STRING  =>
					this.operand = "\"" + operand + "\""
					"String"
				case CLASS   => extract(index, pool)
				case _       => ""
			}
		}
	}

	override def toString(): String = {
		val b: Builder = new Builder(super.toString());
		b.append(": #", index, "(", operand);
		if(ldcType.length > 0) {
			b.append("(", ldcType, ")");
		}
		b.append(")")
		b.toString()
	}
}