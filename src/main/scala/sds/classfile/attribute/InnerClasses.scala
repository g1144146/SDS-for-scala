package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}
import sds.util.AccessFlag.get

class InnerClasses extends AttributeInfo(AttributeType.InnerClasses) {
	private var classes: Array[Array[String]] = null

	def getClasses(): Array[Array[String]] = classes
	
	def check(index: Int, size: Int): Boolean = (0 <= index) && (index < size)

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		val f: ((Int) => String) = (index: Int) => {
			if(check(index, pool.length)) extract(index, pool) else ""
		}
		this.classes = (0 until data.readShort()).map((index: Int) => {
			val inIndex: Int = data.readShort()
			val outIndex: Int = data.readShort()
			val nameIndex: Int = data.readShort()
			val access: String = get(data.readShort(), "nested")
			val in: String = f(inIndex)
			val out: String = f(outIndex)
			val name: String = f(nameIndex)
			Array(in, out, name, access)
		}).toArray
	}

	override def toString(): String = {
		val b: Builder = new Builder(super.toString())
		b.append(": ")
		classes.foreach((array: Array[String]) => b.append("[", array(3), array(0), "{in ", array(1), "}]"))
		b.toString()
	}
}