package sds.classfile.constant_pool

import java.lang.Number
import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantType.{INTEGER, FLOAT, LONG, DOUBLE}

abstract class NumberInfo(tag: Int) extends ConstantInfo(tag) {
	protected var number: Number = null

	override def read(stream: ClassfileStream): Unit = {
		tag match {
			case INTEGER => this.number = stream.readInt()
			case FLOAT   => this.number = stream.readFloat()
			case LONG    => this.number = stream.readLong()
			case DOUBLE  => this.number = stream.readDouble()
		}
	}

	override def toString(): String = {
		var str: String = super.toString() + "\t"
		tag match {
			case INTEGER => str + number.intValue()
			case FLOAT   => str + number.floatValue()
			case LONG    => str + number.longValue()
			case DOUBLE  => str + number.doubleValue()
		}
	}
}

class IntInfo extends NumberInfo(INTEGER) {
	def getInt(): Int = number.intValue()
}

class FloatInfo extends NumberInfo(FLOAT) {
	def getFloat(): Float = number.floatValue()
}

class LongInfo extends NumberInfo(LONG) {
	def getLong(): Long = number.longValue()
}

class DoubleInfo extends NumberInfo(DOUBLE) {
	def getDouble(): Double = number.doubleValue()
}