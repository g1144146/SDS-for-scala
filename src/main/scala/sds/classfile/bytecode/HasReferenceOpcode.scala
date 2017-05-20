package sds.classfile.bytecode

import sds.classfile.constant_pool.{
  ConstantInfo => CInfo,
  NumberInfo,
  ClassInfo,
  StringInfo
}
import sds.util.{MultiArgsStringBuilder => Builder}

class HasReferenceOpcode(_index: Int, pool: Array[CInfo], _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    def index: Int = _index
    def ldcType: String = if(Set("ldc", "ldc_w", "ldc2_w").contains(_type)) {
        pool(index - 1) match {
            case n: NumberInfo => n.number match {
                case x if x.isInstanceOf[Double] => "double"
                case x if x.isInstanceOf[Float]  => "float"
                case x if x.isInstanceOf[Int]    => "int"
                case x if x.isInstanceOf[Long]   => "long"
            }
            case s: StringInfo  => "String"
            case c: ClassInfo   => extract(_index, pool)
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