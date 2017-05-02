package sds.classfile.constant_pool

import java.lang.Number
import sds.classfile.constant_pool.ConstantType.{INTEGER, FLOAT, LONG, DOUBLE}

abstract class NumberInfo(tag: Int, _number: Number) extends ConstantInfo(tag) {
    protected def number: Number = _number
    override def toString(): String = {
        val str: String = super.toString() + "\t"
        tag match {
            case INTEGER => str + _number.intValue()
            case FLOAT   => str + _number.floatValue()
            case LONG    => str + _number.longValue()
            case DOUBLE  => str + _number.doubleValue()
        }
    }
}

class IntInfo(i: Int) extends NumberInfo(INTEGER, i) {
    def int: Int = number.intValue()
}

class FloatInfo(f: Float) extends NumberInfo(FLOAT, f) {
    def float: Float = number.floatValue()
}

class LongInfo(l: Long) extends NumberInfo(LONG, l) {
    def long: Long = number.longValue()
}

class DoubleInfo(d: Double) extends NumberInfo(DOUBLE, d) {
    def double: Double = number.doubleValue()
}