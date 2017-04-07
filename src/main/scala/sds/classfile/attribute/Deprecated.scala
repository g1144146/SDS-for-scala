package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class Deprecated extends AttributeInfo(AttributeType.Deprecated) {
	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {}
}