package sds.classfile.constant_pool

class MemberInfo(tag: Int, _classIndex: Int, _nameAndType: Int) extends ConstantInfo {
    def classIndex:  Int = _classIndex
    def nameAndType: Int = _nameAndType
    override def toString(): String = tag match {
        case ConstantType.FIELD     => s"Fieldref\t#$classIndex.#$nameAndType"
        case ConstantType.METHOD    => s"Methodref\t#$classIndex.#$nameAndType"
        case ConstantType.INTERFACE => s"InterfaceMethodref\t#$classIndex.#$nameAndType"
    }
}