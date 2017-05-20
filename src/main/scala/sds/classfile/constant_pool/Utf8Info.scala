package sds.classfile.constant_pool

class Utf8Info(_value: String) extends ConstantInfo {
    def value: String = _value
    override def toString(): String = s"Utf8\t$value"
}