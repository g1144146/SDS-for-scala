package sds.classfile.constant_pool

import sds.classfile.ClassfileStream
import sds.classfile.Information
import sds.classfile.constant_pool.ConstantType._

abstract class ConstantInfo(_tag: Int) extends Information {
    def tag: Int = _tag
    override def toString(): String = ConstantType.get(tag)
}

object ConstantInfo {
    def apply(data: ClassfileStream): ConstantInfo = data.readByte() match {
        case UTF8    => new Utf8Info(new String(data.readFully(new Array[Byte](data.readShort())), "utf-8"))
        case INTEGER => new IntInfo(data.readInt())
        case FLOAT   => new FloatInfo(data.readFloat())
        case LONG    => new LongInfo(data.readLong())
        case DOUBLE  => new DoubleInfo(data.readDouble())
        case CLASS   => new ClassInfo(data.readShort())
        case STRING  => new StringInfo(data.readShort())
        case FIELD          => new MemberInfo(FIELD, data.readShort(), data.readShort())
        case METHOD         => new MemberInfo(METHOD, data.readShort(), data.readShort())
        case INTERFACE      => new MemberInfo(INTERFACE, data.readShort(), data.readShort())
        case NAME_AND_TYPE  => new NameAndTypeInfo(data.readShort(), data.readShort())
        case HANDLE         => new HandleInfo(data.readByte(), data.readShort())
        case TYPE           => new TypeInfo(data.readShort())
        case INVOKE_DYNAMIC => new InvokeDynamicInfo(data.readShort(), data.readShort())
        case _ => new ConstantInfoAdapter()
    }
}