package sds.classfile.attribute

class ConstantValue(_value: String) extends AttributeInfo {
    def value: String = _value
    override def toString(): String = super.toString() + ": " + _value
}