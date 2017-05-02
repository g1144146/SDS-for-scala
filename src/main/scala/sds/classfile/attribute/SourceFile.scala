package sds.classfile.attribute

class SourceFile(_file: String) extends AttributeInfo {
    def file: String = _file
    override def toString(): String = super.toString() + ": " + _file
}