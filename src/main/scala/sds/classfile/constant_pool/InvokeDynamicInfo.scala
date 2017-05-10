package sds.classfile.constant_pool

class InvokeDynamicInfo(_bsmAtt: Int, _nameAndType: Int) extends ConstantInfo(ConstantType.INVOKE_DYNAMIC) {
    def bsmAtt: Int = _bsmAtt
    def nameAndType: Int = _nameAndType
    override def toString(): String = super.toString() + "\t#" + bsmAtt + ":#" + nameAndType
}