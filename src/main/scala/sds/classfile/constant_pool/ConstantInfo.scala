package sds.classfile.constant_pool

import sds.classfile.ClassfileStream
import sds.classfile.Information

abstract class ConstantInfo(private val tag: Int) extends Information {
	def getTag(): Int = tag
	def read(stream: ClassfileStream): Unit;
	override def read(stream: ClassfileStream, pool: Array[ConstantInfo]): Unit = read(stream)
	override def toString(): String = ConstantType.get(tag)
}