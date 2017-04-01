package sds.classfile

import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get
import sds.util.Utf8ValueExtractor.extract

class MemberInfo extends Information {
	private val declaration: Array[String] = new Array(3)
	var attributes: Array[AttributeInfo] = null

	def getAccess(): String = declaration(0)
	def getName():   String = declaration(1)
	def getDesc():   String = declaration(2)
	def getType():   String = if(declaration(2).contains("(")) "method" else "field"

	override def read(stream: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		val acc: Int = stream.readShort()
		val name: Int = stream.readShort()
		val desc: Int = stream.readShort()
		declaration(1) = extract(pool(name - 1), pool)
		// need to parse
		declaration(2) = extract(pool(desc - 1), pool)
		declaration(0) = get(acc, getType())
	}

	override def toString(): String = declaration(0) + declaration(1) + declaration(2)
}