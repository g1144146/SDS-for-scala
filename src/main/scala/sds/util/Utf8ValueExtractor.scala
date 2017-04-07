package sds.util

import sds.classfile.constant_pool.{ConstantInfo, ClassInfo => Class, HandleInfo => Handle, TypeInfo => Type,
                                    IntInfo, FloatInfo, LongInfo, DoubleInfo, StringInfo, Utf8Info => Utf8,
                                    MemberInfo => Member, NameAndTypeInfo => Name, InvokeDynamicInfo => Invoke}
import sds.classfile.constant_pool.ConstantType._
import sds.util.DescriptorParser.{parse, removeLangPrefix}

object Utf8ValueExtractor {
	def extract(index: Int, pool: Array[ConstantInfo]): String = extract(pool(index - 1), pool)
	
	def extract(target: ConstantInfo, pool: Array[ConstantInfo]): String = {
		target.getTag() match {
			case UTF8    => target.asInstanceOf[Utf8].getValue()
			case INTEGER => target.asInstanceOf[IntInfo].getInt().toString()
			case FLOAT   => target.asInstanceOf[FloatInfo].getFloat().toString()
			case LONG    => target.asInstanceOf[LongInfo].getLong.toString()
			case DOUBLE  => target.asInstanceOf[DoubleInfo].getDouble().toString()
			case STRING  => target.asInstanceOf[StringInfo].getString().toString()
			case CLASS   =>
				val c: Class = target.asInstanceOf[Class]
				removeLangPrefix(extract(pool(c.getIndex() - 1), pool).replace("/", "."))
			case FIELD | METHOD | INTERFACE =>
				val m: Member = target.asInstanceOf[Member]
				extract(pool(m.getClassIndex() - 1), pool) + "." + extract(pool(m.getNameAndType() - 1), pool)
			case NAME_AND_TYPE =>
				val name: Name = target.asInstanceOf[Name]
				extract(pool(name.getName() - 1), pool) + "|" + parse(extract(pool(name.getDesc() - 1), pool))
			case HANDLE         => extract(pool(target.asInstanceOf[Handle].getIndex() - 1), pool)
			case TYPE           => parse(extract(pool(target.asInstanceOf[Type].getDesc() - 1), pool))
			case INVOKE_DYNAMIC => extract(pool(target.asInstanceOf[Invoke].getNameAndType() - 1), pool)
			case _ => throw new IllegalArgumentException("unknown constant info tag(" + target.getTag + ").")
		}
	}
}