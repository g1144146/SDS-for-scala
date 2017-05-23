package sds.classfile.constant_pool

import sds.classfile.ClassfileStream
import sds.classfile.ClassfileInformation

abstract class ConstantInfo extends ClassfileInformation

object ConstantInfo {
    val UTF8:           Int = 1
    val INTEGER:        Int = 3
    val FLOAT:          Int = 4
    val LONG:           Int = 5
    val DOUBLE:         Int = 6
    val CLASS:          Int = 7
    val STRING:         Int = 8
    val FIELD:          Int = 9
    val METHOD:         Int = 10
    val INTERFACE:      Int = 11
    val NAME_AND_TYPE:  Int = 12
    val HANDLE:         Int = 15
    val TYPE:           Int = 16
    val INVOKE_DYNAMIC: Int = 18

    def apply(tag: Int, data: ClassfileStream): ConstantInfo = tag match {
        case UTF8                   => new Utf8Info(new String(data.fully(new Array[Byte](data.short)), "utf-8"))
        case INTEGER                => new NumberInfo(tag, data.int)
        case FLOAT                  => new NumberInfo(tag, data.float)
        case LONG                   => new NumberInfo(tag, data.long)
        case DOUBLE                 => new NumberInfo(tag, data.double)
        case CLASS                  => new ClassInfo(data.short)
        case STRING                 => new StringInfo(data.short)
        case FIELD|METHOD|INTERFACE => new MemberInfo(tag, data.short, data.short)
        case NAME_AND_TYPE          => new NameAndTypeInfo(data.short, data.short)
        case HANDLE                 => new HandleInfo(data.byte, data.short)
        case TYPE                   => new TypeInfo(data.short)
        case INVOKE_DYNAMIC         => new InvokeDynamicInfo(data.short, data.short)
        case _                      => new ConstantInfoAdapter()
    }
}