package sds.classfile.attribute

import sds.classfile.constant_pool.ConstantInfo

class EnclosingMethod(classIndex: Int, methodIndex: Int, pool: Array[ConstantInfo]) extends AttributeInfo {
    def _class: String = extract(classIndex, pool)
    def method: String = if(methodIndex - 1 > 0) extract(methodIndex, pool) else ""
    override def toString(): String = "EnclosingMethod: " + _class + " " + method
}