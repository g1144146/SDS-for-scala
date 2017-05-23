package sds.classfile.constant_pool

class MemberInfo(tag: Int, _classIndex: Int, _nameAndType: Int) extends ConstantInfo {
    def classIndex:  Int = _classIndex
    def nameAndType: Int = _nameAndType
    override def toString(): String = tag match {
        case ConstantInfo.FIELD     => s"Fieldref\t#$classIndex.#$nameAndType"
        case ConstantInfo.METHOD    => s"Methodref\t#$classIndex.#$nameAndType"
        case ConstantInfo.INTERFACE => s"InterfaceMethodref\t#$classIndex.#$nameAndType"
    }
}