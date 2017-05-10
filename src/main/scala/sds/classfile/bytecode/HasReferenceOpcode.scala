package sds.classfile.bytecode

import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.constant_pool.ConstantType.{DOUBLE, FLOAT, INTEGER, LONG, STRING, CLASS}
import sds.util.{MultiArgsStringBuilder => Builder}

class HasReferenceOpcode(_index: Int, pool: Array[CInfo], _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    def index: Int = _index
    def ldcType: String = if(Set("ldc", "ldc_w", "ldc2_w").contains(_type)) {
        pool(index - 1).tag match {
            case DOUBLE  => "double"
            case FLOAT   => "float"
            case INTEGER => "int"
            case LONG    => "long"
            case STRING  => "String"
            case CLASS   => extract(_index, pool)
            case _       => ""
        }
    } else ""
    def operand: String = if(ldcType.equals("String")) "\"" + extract(_index, pool) + "\""
                          else                         extract(_index, pool)

    override def toString(): String = {
        val b: Builder = new Builder(super.toString())
        b.append(": #", _index, "(", operand)
        if(ldcType.length > 0) {
            b.append("(", ldcType, ")")
        }
        b.append(")")
        b.toString()
    }
}