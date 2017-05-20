package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class BootstrapMethods(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
    /**
     * (bsm_reference, bootstrap_args)
     */
    val bsm: Array[(String, Array[String])] = (0 until data.short).map((_: Int) => {
        val bsmRef: String = extract(data.short, pool)
        val args: Array[String] = (0 until data.short).map((i: Int) => extract(data.short, pool)).toArray
        (bsmRef, args)
    }).toArray
}