package sds.classfile.constant_pool

class TypeInfo(_desc: Int) extends ConstantInfo(ConstantType.TYPE) {
	def desc: Int = _desc
	override def toString(): String = super.toString + "\t#" + desc
 }