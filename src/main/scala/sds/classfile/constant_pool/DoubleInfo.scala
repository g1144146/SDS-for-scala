package sds.classfile.constant_pool

class DoubleInfo extends NumberInfo(ConstantType.DOUBLE) {
	def getDouble(): Double = number.doubleValue()
}