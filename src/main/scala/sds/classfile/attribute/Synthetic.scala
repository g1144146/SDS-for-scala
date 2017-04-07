package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Synthetic extends AttributeInfo(AttributeType.Synthetic) {
	override def read(data: ClassfileStream, pool: Array[ConstantInfo]):Unit = {
		// do nothing.
	}
}