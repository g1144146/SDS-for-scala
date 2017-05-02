package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}

sealed abstract class VerificationTypeInfo(_tag: Int) {
    def tag: Int = _tag
    override def toString(): String = getClass().getSimpleName()
}

object VerificationTypeInfo {
    def apply(data: Stream): VerificationTypeInfo = {
        val tag: Int = data.readUnsignedByte()
        tag match {
            case 0 => new VerificationTypeInfoAdapter(tag, "top");
            case 1 => new VerificationTypeInfoAdapter(tag, "int");
            case 2 => new VerificationTypeInfoAdapter(tag, "float");
            case 3 => new VerificationTypeInfoAdapter(tag, "double");
            case 4 => new VerificationTypeInfoAdapter(tag, "long");
            case 5 => new VerificationTypeInfoAdapter(tag, "null");
            case 6 => new VerificationTypeInfoAdapter(tag, "");
            case 7 => new ObjectVar(tag, data.readShort());
            case 8 => new UninitializedVar(tag, data.readShort());
        }
    }
}

class VerificationTypeInfoAdapter(tag: Int, _type: String) extends VerificationTypeInfo(tag) {
    override def toString(): String = _type
}

class ObjectVar(tag: Int, _cpool: Int) extends VerificationTypeInfo(tag) {
    def cpool: Int = _cpool
}

class UninitializedVar(tag: Int, _offset: Int) extends VerificationTypeInfo(tag) {
    def offset: Int = _offset
}