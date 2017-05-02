package sds.classfile.attribute

class Signature(_signature: String) extends AttributeInfo {
    def signature(): String = _signature
    override def toString(): String = super.toString() + ": " + _signature
}