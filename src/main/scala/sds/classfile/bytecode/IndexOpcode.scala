package sds.classfile.bytecode

class IndexOpcode(_index: Int, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    def index: Int = _index
    override def toString(): String = super.toString() + ": " + index
}