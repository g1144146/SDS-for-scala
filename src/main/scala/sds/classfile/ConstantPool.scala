package sds.classfile

import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}

class ConstantPool(val n: Int) extends ArrayInformation[ConstantInfo](n: Int) {
	this(n)

	override def toString(): String = {
		val builder: Builder = new Builder()
		val line: String = System.getProperty("line.separator")
		builder.append("<<< Constant Pool >>>", line)
		(0 until array.length).foreach((i: Int) => builder.append("[", i + 1, "]", array(i), line))
		builder.toString()
	}
}