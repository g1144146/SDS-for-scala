package sds.classfile.constant_pool

class StringInfo(_string: Int) extends ConstantInfo(ConstantType.STRING) {
	def string(): Int = _string
	override def toString(): String = super.toString + "\t#" + string
}