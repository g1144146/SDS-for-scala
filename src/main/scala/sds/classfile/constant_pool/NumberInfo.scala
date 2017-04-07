package sds.classfile.constant_pool

import java.lang.Number
import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantType.{INTEGER, FLOAT, LONG, DOUBLE}

abstract class NumberInfo(tag: Int, data: ClassfileStream) extends ConstantInfo(tag) {
	protected val number: Number = tag match {
		case INTEGER => data.readInt()
		case FLOAT   => data.readFloat()
		case LONG    => data.readLong()
		case DOUBLE  => data.readDouble()
	}

	override def toString(): String = {
		val str: String = super.toString() + "\t"
		tag match {
			case INTEGER => str + number.intValue()
			case FLOAT   => str + number.floatValue()
			case LONG    => str + number.longValue()
			case DOUBLE  => str + number.doubleValue()
		}
	}
}

class IntInfo(data: ClassfileStream) extends NumberInfo(INTEGER, data) {
	def getInt(): Int = number.intValue()
}

class FloatInfo(data: ClassfileStream) extends NumberInfo(FLOAT, data) {
	def getFloat(): Float = number.floatValue()
}

class LongInfo(data: ClassfileStream) extends NumberInfo(LONG, data) {
	def getLong(): Long = number.longValue()
}

class DoubleInfo(data: ClassfileStream) extends NumberInfo(DOUBLE, data) {
	def getDouble(): Double = number.doubleValue()
}