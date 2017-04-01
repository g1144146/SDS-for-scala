package sds.classfile.constant_pool

import sds.classfile.constant_pool.ConstantType._

class ConstantInfoFactory {
	def create(tag: Int): ConstantInfo = {
		tag match {
			case UTF8 => new Utf8Info()
			case INTEGER => new IntInfo()
			case FLOAT   => new FloatInfo()
			case LONG    => new LongInfo()
			case DOUBLE  => new DoubleInfo()
			case CLASS   => new ClassInfo()
			case STRING  => new StringInfo()
			case FIELD   => new MemberInfo(FIELD)
			case METHOD  => new MemberInfo(METHOD)
			case INTERFACE     => new MemberInfo(INTERFACE)
			case NAME_AND_TYPE => new NameAndTypeInfo()
			case HANDLE => new HandleInfo()
			case TYPE   => new TypeInfo()
			case INVOKE_DYNAMIC => new InvokeDynamicInfo()
			case _ => new ConstantInfoAdapter()
		}
	}
}