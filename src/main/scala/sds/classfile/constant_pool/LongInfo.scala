package sds.classfile.constant_pool

class LongInfo extends NumberInfo(ConstantType.LONG) {
	def getLong(): Long = number.longValue()
}