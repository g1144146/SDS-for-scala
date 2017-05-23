package sds.classfile.bytecode

import sds.classfile.constant_pool.{
  ConstantInfo => CInfo,
  NumberInfo,
  ClassInfo,
  StringInfo
}

class HasReferenceOpcode(_index: Int, pool: Array[CInfo], _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    def index: Int = _index
    def ldcType: String = if(Set("ldc", "ldc_w", "ldc2_w").contains(_type)) {
        pool(index - 1) match {
            case n: NumberInfo => n.tag match {
                case CInfo.DOUBLE  => "double"
                case CInfo.FLOAT   => "float"
                case CInfo.INTEGER => "int"
                case CInfo.LONG    => "long"
            }
            case s: StringInfo  => "String"
            case c: ClassInfo   => extract(_index, pool)
            case _       => ""
        }
    } else ""
    def operand: String = if(ldcType == "String") "\"" + extract(_index, pool) + "\""
                          else                         extract(_index, pool)

    override def toString(): String = {
        val before: String = s"${super.toString()}: #${_index}($operand"
        val after:  String = if(ldcType.length > 0) s"($ldcType))" else ")"
        s"$before$after"
    }
}