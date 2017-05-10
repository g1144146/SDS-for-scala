package sds.classfile.constant_pool

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

    def get(tag: Int): String = tag match {
        case UTF8           => "Utf8"
        case INTEGER        => "Int"
        case FLOAT          => "Float"
        case LONG           => "Long"
        case DOUBLE         => "Double"
        case CLASS          => "Class"
        case STRING         => "String"
        case FIELD          => "Field_ref"
        case METHOD         => "Method_ref"
        case INTERFACE      => "InterfaceMethod_ref"
        case NAME_AND_TYPE  => "NameAndType"
        case HANDLE         => "MethodHandle"
        case TYPE           => "MethodType"
        case INVOKE_DYNAMIC => "InvokeDynamic"
        case _              => "Unknown"
    }
}