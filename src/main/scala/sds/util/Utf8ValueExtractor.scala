package sds.util

import sds.classfile.constant_pool.{
  ConstantInfo, IntInfo, FloatInfo,
  LongInfo, DoubleInfo, StringInfo,
  ClassInfo         => Class,
  HandleInfo        => Handle,
  TypeInfo          => Type,
  Utf8Info          => Utf8,
  MemberInfo        => Member,
  NameAndTypeInfo   => Name,
  InvokeDynamicInfo => Invoke
}
import sds.util.DescriptorParser.{parse, removeLangPrefix}

object Utf8ValueExtractor {
	def extract(index: Int, pool: Array[ConstantInfo]): String = extract(pool(index - 1), pool)
	
	def extract(target: ConstantInfo, pool: Array[ConstantInfo]): String = target match {
		case utf8:   Utf8       => utf8.getValue()
		case int:    IntInfo    => int.getInt().toString()
		case float:  FloatInfo  => float.getFloat().toString()
		case long:   LongInfo   => long.getLong.toString()
		case double: DoubleInfo => double.getDouble().toString()
		case str:    StringInfo => str.getString().toString()
		case c: Class  => removeLangPrefix(extract(pool(c.getIndex() - 1), pool).replace("/", "."))
		case m: Member =>
			extract(pool(m.getClassIndex() - 1), pool) + "." + extract(pool(m.getNameAndType() - 1), pool)
		case n: Name   => extract(pool(n.getName() - 1), pool) + "|" + parse(extract(pool(n.getDesc() - 1), pool))
		case handle: Handle => extract(pool(handle.getIndex() - 1), pool)
		case _type:  Type   => parse(extract(pool(_type.getDesc() - 1), pool))
		case invoke: Invoke => extract(pool(invoke.getNameAndType() - 1), pool)
		case _ => throw new IllegalArgumentException("unknown constant info tag(" + target.getTag + ").")
	}
}