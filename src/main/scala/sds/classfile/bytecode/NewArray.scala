package sds.classfile.bytecode

class NewArray(_atype: Int, pc: Int) extends OpcodeInfo("newarray", pc) {
    def atype: String = _atype match {
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
    override def toString(): String = s"${super.toString()}: $atype"
}