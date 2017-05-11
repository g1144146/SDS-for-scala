package sds.classfile

import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.{ConstantInfo, Utf8Info => Utf8}
import sds.util.AccessFlag.get
import sds.util.DescriptorParser.parse

class MemberInfo(data: ClassfileStream, pool: Array[ConstantInfo]) extends Information {
    private val declaration: Array[String] = (() => {
        val accIndex:  Int = data.readShort()
        val nameIndex: Int = data.readShort()
        val descIndex: Int = data.readShort()
        val name: String = extract(nameIndex, pool)
        val desc: String = parse(extract(descIndex, pool))
        Array(get(accIndex, if(desc.contains("(")) "method" else "field"), name, desc)
    })()
    private val attributes: Array[AttributeInfo] = (0 until data.readShort()).map((_: Int) => ({
        val name: Int = data.readShort()
        val utf8: Utf8 = pool(name - 1).asInstanceOf[Utf8]
        AttributeInfo(utf8.value, data, pool)
    })).toArray

    def getAccess(): String = declaration(0)
    def getName():   String = declaration(1)
    def getDesc():   String = declaration(2)
    def getType():   String = if(declaration(2).contains("(")) "method" else "field"
    def getAttributes(): Array[AttributeInfo] = attributes

    override def toString(): String = {
        if(getType().equals("field")) {
            declaration(2) + declaration(1) + " " +  declaration(0)
        } else {
            declaration(0) + declaration(1) + declaration(2)
        }
    }
}