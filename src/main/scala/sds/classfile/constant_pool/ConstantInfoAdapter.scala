package sds.classfile.constant_pool

import sds.classfile.ClassfileStream

class ConstantInfoAdapter extends ConstantInfo(-1) {
	override def read(stream: ClassfileStream): Unit = {}
	override def toString(): String = "null"
}