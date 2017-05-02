package sds.classfile.constant_pool

class NameAndTypeInfo(_name: Int, _desc: Int) extends ConstantInfo(ConstantType.NAME_AND_TYPE) {
	def name: Int = _name
	def desc: Int = _desc
	override def toString(): String = super.toString() + "\t#" + _name + ":#" + _desc
}