package sds.classfile.constant_pool

class InvokeDynamicInfo(_bsmAtt: Int, _nameAndType: Int) extends ConstantInfo {
    def bsmAtt: Int = _bsmAtt
    def nameAndType: Int = _nameAndType
    override def toString(): String = s"InvokeDynamic\t#$bsmAtt:#$nameAndType"
}