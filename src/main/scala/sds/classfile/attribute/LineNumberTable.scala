package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}

class LineNumberTable(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
	private val table: Array[Array[Int]] = (0 until data.readShort()).map((index: Int) => {
		val start: Int = data.readShort()
		val line: Int = data.readShort()
		Array(start, -1, line)
	}).toArray
	init()
	
	def init(): Unit = {
		val len = table.length
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

	def getTable(): Array[Array[Int]] = table
	override def toString(): String = {
		val b: Builder = new Builder(super.toString())
		b.append(": ")
		table.foreach((array: Array[Int]) => {
			b.append("[range:", array(0), "-", array(1), "|line:" + array(2), "]_")
		})
		b.toString()
	}
}