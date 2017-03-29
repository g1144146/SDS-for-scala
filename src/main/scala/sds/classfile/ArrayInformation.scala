package sds.classfile

import scala.collection.mutable.ArraySeq

abstract class ArrayInformation[T <: Information](val n: Int) extends ArraySeq[Information](n: Int) {
	this(n)
}