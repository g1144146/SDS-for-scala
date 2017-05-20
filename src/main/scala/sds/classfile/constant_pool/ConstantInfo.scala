package sds.classfile.constant_pool

import sds.classfile.ClassfileStream
import sds.classfile.Information
import sds.classfile.constant_pool.ConstantType._

abstract class ConstantInfo extends Information

object ConstantInfo {
    def apply(data: ClassfileStream): ConstantInfo = {
        val tag: Int = data.readByte()
        tag match {
            case UTF8    => new Utf8Info(new String(data.readFully(new Array[Byte](data.readShort())), "utf-8"))
            case INTEGER => new NumberInfo(tag, data.readInt())
            case FLOAT   => new NumberInfo(tag, data.readFloat())
            case LONG    => new NumberInfo(tag, data.readLong())
            case DOUBLE  => new NumberInfo(tag, data.readDouble())
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
}

object ConstantType {
    val UTF8:    Int = 1
    val INTEGER: Int = 3
    val FLOAT:   Int = 4
    val LONG:    Int = 5
    val DOUBLE:  Int = 6
    val CLASS:   Int = 7
    val STRING:  Int = 8
    val FIELD:     Int = 9
    val METHOD:    Int = 10
    val INTERFACE: Int = 11
    val NAME_AND_TYPE:  Int = 12
    val HANDLE:         Int = 15
    val TYPE:           Int = 16
    val INVOKE_DYNAMIC: Int = 18
}