package sds.classfile.constant_pool

class NameAndTypeInfo(_name: Int, _desc: Int) extends ConstantInfo {
    def name: Int = _name
    def desc: Int = _desc
    override def toString(): String = s"NameAndType\t#$name:#$desc"
}