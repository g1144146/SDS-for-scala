package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.DescriptorParser.parse

class LocalVariable(private val t: AttributeType.Value) extends AttributeInfo(t) {
	private var table: Array[Array[Int]] = null
	private var nameTable: Array[Array[String]] = null

	def getTable(): Array[Array[Int]] = table
	def getNameTable: Array[Array[String]] = nameTable

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		val size: Int = data.readShort()
		this.table = new Array(size)
		this.nameTable = new Array(size)
		(0 until size).foreach((i: Int) => {
			val start: Int = data.readShort()
			val end:   Int = data.readShort() + start
			val nameIndex: Int = data.readShort()
			val descIndex: Int = data.readShort()
			val index: Int = data.readShort()
			val name: String = extract(nameIndex, pool)
			val desc: String = if(descIndex - 1 > 0) parse(extract(descIndex, pool)) else ""
			table(i) = Array(start, end, index)
			nameTable(i) = Array(name, desc)
		})
	}

	override def toString(): String = {
		val b: sds.util.MultiArgsStringBuilder = new sds.util.MultiArgsStringBuilder(super.toString())
		b.append(": ")
		(0 until table.length).foreach((i: Int) => {
			b.append("[(", table(3), "):", nameTable(1), " ", nameTable(0), "{", table(0), "-", table(1))
		})
		b.toString()
	}
}