package sds.classfile.constant_pool

import sds.classfile.constant_pool.{
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
        case utf8:   Utf8       => utf8.value
        case int:    IntInfo    => int.int.toString()
        case float:  FloatInfo  => float.float.toString()
        case long:   LongInfo   => long.long.toString()
        case double: DoubleInfo => double.double.toString()
        case str:    StringInfo => str.string.toString()
        case c: Class  => removeLangPrefix(extract(pool(c.index - 1), pool).replace("/", "."))
        case m: Member =>
            extract(pool(m.classIndex - 1), pool) + "." + extract(pool(m.nameAndType - 1), pool)
        case n: Name   => extract(pool(n.name - 1), pool) + "|" + parse(extract(pool(n.desc - 1), pool))
        case handle: Handle => extract(pool(handle.index - 1), pool)
        case _type:  Type   => parse(extract(pool(_type.desc - 1), pool))
        case invoke: Invoke => extract(pool(invoke.nameAndType - 1), pool)
        case _ => throw new IllegalArgumentException("unknown constant info tag(" + target.tag + ").")
    }
}