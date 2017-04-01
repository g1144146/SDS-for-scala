package sds.classfile

import sds.classfile.constant_pool.ConstantInfo

trait Information {
	def read(stream: ClassfileStream, pool: Array[ConstantInfo]): Unit;
}