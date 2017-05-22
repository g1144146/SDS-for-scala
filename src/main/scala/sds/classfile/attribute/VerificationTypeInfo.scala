package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}

sealed abstract class VerificationTypeInfo

object VerificationTypeInfo {
    def apply(data: Stream): VerificationTypeInfo = data.unsignedByte match {
        case 0 => new VerificationTypeInfoAdapter("top");
        case 1 => new VerificationTypeInfoAdapter("int");
        case 2 => new VerificationTypeInfoAdapter("float");
        case 3 => new VerificationTypeInfoAdapter("double");
        case 4 => new VerificationTypeInfoAdapter("long");
        case 5 => new VerificationTypeInfoAdapter("null");
        case 6 => new VerificationTypeInfoAdapter("");
        case 7 => new ObjectVar(data.short);
        case 8 => new UninitializedVar(data.short);
    }
}

class VerificationTypeInfoAdapter(_type: String) extends VerificationTypeInfo {
    override def toString(): String = _type
}

class ObjectVar(_cpool: Int) extends VerificationTypeInfo {
    def cpool: Int = _cpool
}

class UninitializedVar(_offset: Int) extends VerificationTypeInfo {
    def offset: Int = _offset
}