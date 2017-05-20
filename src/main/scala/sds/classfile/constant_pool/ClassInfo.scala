package sds.classfile.constant_pool

class ClassInfo(_index: Int) extends ConstantInfo {
    def index: Int = _index
    override def toString(): String = s"Class\t#$index"
}