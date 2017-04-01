package sds.classfile.constant_pool

class FloatInfo extends NumberInfo(ConstantType.FLOAT) {
	def getFloat(): Float = number.floatValue()
}