package sds.classfile.constant_pool

import sds.classfile.constant_pool.ConstantInfo.{INTEGER, FLOAT, LONG, DOUBLE}

class NumberInfo(_tag: Int, _number: Number) extends ConstantInfo {
    def number: Number = _number
    def tag: Int = _tag
    override def toString(): String = tag match {
        case INTEGER => s"Int\t${number.toString}"
        case FLOAT   => s"Float\t${number.toString}"
        case LONG    => s"Long\t${number.toString}"
        case DOUBLE  => s"Double\t${number.toString}"
    }
}