package sds.classfile.constant_pool

import sds.classfile.Information

abstract class ConstantInfo(private val tag: Int) extends Information {
	def getTag(): Int = tag
}
