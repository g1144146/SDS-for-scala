package sds.classfile.constant_pool

class IntInfo extends NumberInfo(ConstantType.INTEGER) {
	def getInt(): Int = number.intValue()
}