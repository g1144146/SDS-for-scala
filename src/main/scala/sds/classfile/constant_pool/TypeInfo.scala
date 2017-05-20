package sds.classfile.constant_pool

class TypeInfo(_desc: Int) extends ConstantInfo {
    def desc: Int = _desc
    override def toString(): String = s"MethodType\t#$desc"
 }