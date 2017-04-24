package sds.classfile.constant_pool

class Utf8Info(_len: Int, _value: String) extends ConstantInfo(ConstantType.UTF8) {
	def len: Int = _len
	def value: String = _value
	override def toString(): String = super.toString() + "\t" + value
}