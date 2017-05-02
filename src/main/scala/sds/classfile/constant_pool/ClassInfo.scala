package sds.classfile.constant_pool

class ClassInfo(_index: Int) extends ConstantInfo(ConstantType.CLASS) {
    def index: Int = _index
    override def toString(): String = super.toString() + "\t#" + _index
}