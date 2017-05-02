package sds.classfile.bytecode

class PushOpcode(_value: Int, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    def value: Int = _value
    override def toString(): String = super.toString() + ": " + _value
}