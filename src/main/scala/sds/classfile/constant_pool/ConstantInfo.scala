package sds.classfile.constant_pool

import sds.classfile.ClassfileStream
import sds.classfile.Information
import sds.classfile.constant_pool.ConstantType._

abstract class ConstantInfo(private val tag: Int) extends Information {
	def getTag(): Int = tag
	override def toString(): String = ConstantType.get(tag)
}

object ConstantInfo {
	def apply(data: ClassfileStream): ConstantInfo = { 
		val tag: Int = data.readByte()
		tag match {
			case UTF8 => new Utf8Info(data)
			case INTEGER => new IntInfo(data)
			case FLOAT   => new FloatInfo(data)
			case LONG    => new LongInfo(data)
			case DOUBLE  => new DoubleInfo(data)
			case CLASS   => new ClassInfo(data)
			case STRING  => new StringInfo(data)
			case FIELD          => new MemberInfo(FIELD, data)
			case METHOD         => new MemberInfo(METHOD, data)
			case INTERFACE      => new MemberInfo(INTERFACE, data)
			case NAME_AND_TYPE  => new NameAndTypeInfo(data)
			case HANDLE         => new HandleInfo(data)
			case TYPE           => new TypeInfo(data)
			case INVOKE_DYNAMIC => new InvokeDynamicInfo(data)
			case _ => new ConstantInfoAdapter()
		}
	}
}