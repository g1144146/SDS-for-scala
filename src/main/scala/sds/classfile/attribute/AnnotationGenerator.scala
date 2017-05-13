package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.annotation.{
  Annotation       => Ann,
  ElementValue     => Element,
  ElementValuePair => Pair,
  EnumConstValue   => Enum
}
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.{MultiArgsStringBuilder => Builder}
import sds.util.DescriptorParser.parse

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
            case 'B'|'D'|'F'|'I'|'J'|'S'|'Z' => builder.append(extract(element.getConstVal(), pool))
            case 'C' => builder.append("'",  extract(element.getConstVal().asInstanceOf[Char], pool), "'")
            case 's' => builder.append("\"", extract(element.getConstVal(), pool), "\"")
            case 'c' => builder.append(parse(extract(element.getClassInfo(), pool)), ".class")
            case 'e' => 
                val enum: Enum = element.getEnumConst()
                builder.append(parse(extract(enum.typeName, pool)), ".", extract(enum.constName, pool))
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