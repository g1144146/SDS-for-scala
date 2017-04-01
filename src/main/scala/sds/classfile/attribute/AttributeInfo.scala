package sds.classfile.attribute

import sds.classfile.{Information, ClassfileStream}
import sds.classfile.constant_pool.ConstantInfo

class AttributeInfo extends Information {
	override def read(stream: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		
	}
}