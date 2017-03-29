package sds.classfile

import java.io.IOException

trait Information {
	@throws(classOf[IOException])
	def read(stream: ClassfileStream, pool: ConstantPool): Unit;
}