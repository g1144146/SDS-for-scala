package sds.classfile.constant_pool

class StringInfo(_string: Int) extends ConstantInfo {
    def string: Int = _string
    override def toString(): String = s"String\t#$string"
}