package sds.util

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.annotation.{
  Annotation       => Ann,
  ElementValue     => Element,
  ElementValuePair => Pair,
  EnumConstValue   => Enum
}
import sds.util.{MultiArgsStringBuilder => Builder}
import sds.util.DescriptorParser.parse
import sds.util.Utf8ValueExtractor.extract

object AnnotationGenerator {
	def generate(data: ClassfileStream, pool: Array[CInfo]): String = {
		generate(new Ann(data), pool)
	}

	def generate(ann: Ann, pool: Array[CInfo]): String = {
		val value: String = ann.getPairs().map((pair: Pair) => {
			extract(pair.getName(), pool) + " = " + generateFromElementValue(pair.getValue(), pool, new Builder())
		}).toArray.mkString("(", ",", ")")
		"@" + parse(extract(ann.getType(), pool)) + value
	}

	def generateFromElementValue(element: Element, pool: Array[CInfo], builder: Builder): String = {
		element.getTag() match {
			case 'B'|'D'|'F'|'I'|'J'|'S'|'Z' => builder.append(extract(pool(element.getConstVal() - 1), pool))
			case 'C' => builder.append("'",  extract(element.getConstVal(), pool), "'")
			case 's' => builder.append("\"", extract(element.getConstVal(), pool), "\"")
			case 'c' => builder.append(parse(extract(element.getClassInfo(), pool)), ".class")
			case 'e' => 
				val enum: Enum = element.asInstanceOf[Enum]
				builder.append(parse(extract(enum.getTypeName(), pool)), ".", extract(enum.getConstName(), pool))
			case '@' => builder.append(generate(element.getAnnotation, pool))
			case '[' =>
				val arrayElement: String = element.getArray().getValues().map((ev: Element) => {
					generateFromElementValue(ev, pool, new Builder())
				}).toArray.mkString("{", ",", "}")
				builder.append(arrayElement)
			case _ => throw new RuntimeException("unknown tag(" + element.getTag() + ")")
		}
		builder.toString()
	}
}