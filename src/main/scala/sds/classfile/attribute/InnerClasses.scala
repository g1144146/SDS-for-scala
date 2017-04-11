package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get

class InnerClasses(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
	private val classes: Array[Array[String]] = (0 until data.readShort()).map((index: Int) => {
		val f: ((Int) => String) = (index: Int) => if(check(index, pool.length)) extract(index, pool) else ""
		val inIndex: Int = data.readShort()
		val outIndex: Int = data.readShort()
		val nameIndex: Int = data.readShort()
		val access: String = get(data.readShort(), "nested")
		val in: String = f(inIndex)
		val out: String = f(outIndex)
		val name: String = f(nameIndex)
		Array(in, out, name, access)
	}).toArray

	def getClasses(): Array[Array[String]] = classes
	def check(index: Int, size: Int): Boolean = (0 until size).contains(index - 1)
}