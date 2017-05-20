package sds.classfile

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.{AttributeInfo => Attribute}
import sds.classfile.constant_pool.{ConstantInfo => Cons}
import sds.util.AccessFlag.get
import sds.util.DescriptorParser.parse

class MemberInfo(data: Stream, pool: Array[Cons], f: (Stream, Array[Cons]) => Attribute) extends Information {
    private val declaration: Array[String] = (() => {
        val accIndex:  Int = data.short
        val nameIndex: Int = data.short
        val descIndex: Int = data.short
        val name: String = extract(nameIndex, pool)
        val desc: String = parse(extract(descIndex, pool))
        Array(get(accIndex, if(desc.contains("(")) "method" else "field"), name, desc)
    })()
    val attributes: Array[Attribute] = (0 until data.short).map((_: Int) => f(data, pool)).toArray

    def access: String = declaration(0)
    def name:   String = declaration(1)
    def desc:   String = declaration(2)
    def _type:  String = if(declaration(2).contains("(")) "method" else "field"

    override def toString(): String =
        if(_type.equals("field")) declaration(0) + declaration(2) + " " +  declaration(1)
        else                      declaration(0) + declaration(1) + declaration(2)
}