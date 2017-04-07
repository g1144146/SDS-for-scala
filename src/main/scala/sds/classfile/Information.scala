package sds.classfile

import sds.classfile.constant_pool.ConstantInfo
import sds.util.Utf8ValueExtractor.extract

trait Information {
	def extract(index: Int, pool: Array[ConstantInfo]): String = {
		sds.util.Utf8ValueExtractor.extract(pool(index - 1), pool)
	}
}