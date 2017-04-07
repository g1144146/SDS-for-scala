package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}

class LineNumberTable extends AttributeInfo(AttributeType.LineNumberTable) {
	private var table: Array[Array[Int]] = null

	def getTable(): Array[Array[Int]] = table

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		val len: Int = data.readShort()
		table = (0 until len).map((index: Int) => {
			val start: Int = data.readShort()
			val line: Int = data.readShort()
			Array(start, -1, line)
		}).toArray
		(0 until len).foreach((index: Int) => {
			if(index == len - 1) {
				table(index)(1) = table(index)(0)
				if(len > 1) {
					table(index - 1)(1) = table(index)(0)
				}
			} else if(index > 0) {
				table(index - 1)(1) = table(index)(0)
			}
		})
	}

	override def toString(): String = {
		val b: Builder = new Builder(super.toString())
		b.append(": ")
		table.foreach((array: Array[Int]) => {
			b.append("[range:", array(0), "-", array(1), "|line:" + array(2), "]_")
		})
		b.toString()
	}
}