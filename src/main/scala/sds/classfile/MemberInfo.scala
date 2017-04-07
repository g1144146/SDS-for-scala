package sds.classfile

import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get
import sds.util.DescriptorParser.parse

class MemberInfo extends Information {
	private val declaration: Array[String] = new Array(3)
	var attributes: Array[AttributeInfo] = null

	def getAccess(): String = declaration(0)
	def getName():   String = declaration(1)
	def getDesc():   String = declaration(2)
	def getType():   String = if(declaration(2).contains("(")) "method" else "field"
	def getAttributes(): Array[AttributeInfo] = attributes

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		val acc: Int  = data.readShort()
		val name: Int = data.readShort()
		val desc: Int = data.readShort()
		declaration(1) = extract(name, pool)
		declaration(2) = parse(extract(desc, pool))
		declaration(0) = get(acc, getType())
	}

	override def toString(): String = {
		if(getType().equals("field")) {
			declaration(0) + declaration(2) + " " + declaration(1)
		} else {
			declaration(0) + declaration(1) +  declaration(2)
		}
	}
}