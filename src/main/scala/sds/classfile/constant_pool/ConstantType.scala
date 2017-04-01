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

	def get(tag: Int): String = {
		tag match {
			case UTF8           => "CONSTANT_UTF8"
			case INTEGER        => "CONSTANT_INTEGER"
			case FLOAT          => "CONSTANT_FLOAT"
			case LONG           => "CONSTANT_LONG"
			case DOUBLE         => "CONSTANT_DOUBLE"
			case CLASS          => "CONSTANT_CLASS"
			case STRING         => "CONSTANT_STRING"
			case FIELD          => "CONSTANT_FIELDREF"
			case METHOD         => "CONSTANT_METHODREF"
			case INTERFACE      => "CONSTANT_INTERFACE_METHODREF"
			case NAME_AND_TYPE  => "CONSTANT_NAME_AND_TYPE"
			case HANDLE         => "CONSTANT_METHOD_HANDLE"
			case TYPE           => "CONSTANT_METHOD_TYPE"
			case INVOKE_DYNAMIC => "CONSTANT_INVOKE_DYNAMIC"
			case _ => "UKNOWN"
		}
	}
}