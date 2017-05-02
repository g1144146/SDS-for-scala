package sds.classfile.constant_pool

class Utf8Info(_value: String) extends ConstantInfo(ConstantType.UTF8) {
	def value: String = _value
	override def toString(): String = super.toString() + "\t" + _value
}