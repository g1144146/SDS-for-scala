package sds.classfile.bytecode

class Iinc(_index: Int, _const: Int, pc: Int) extends OpcodeInfo("iinc", pc) {
    def index: Int = _index
    def const: Int = _const
    override def toString(): String = s"${super.toString()}: $index, $const"
}