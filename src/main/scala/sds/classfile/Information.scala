package sds.classfile

import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.{Utf8ValueExtractor => Extractor}

trait Information {
    def extract(index: Int, pool: Array[ConstantInfo]): String = Extractor.extract(pool(index - 1), pool)
}