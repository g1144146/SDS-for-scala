package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class LineNumberTable(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo {
    val table: Array[Array[Int]] = (0 until data.short).map((index: Int) => {
        val start: Int = data.short
        val line: Int = data.short
        Array(start, -1, line)
    }).toArray
    init()
    
    def init(): Unit = {
        val len = table.length
        (0 until len).foreach((index: Int) => {
            if(index == len - 1) {
                table(index)(1) = table(index)(0)
                if(len > 1) {
                    table(index - 1)(1) = table(index)(0)
                }
            } else if(index > 0) {
                table(index - 1)(1) = table(index)(0)
            }
        })
    }

    def getTableStr(): Array[String] = table.map((array: Array[Int]) => 
        "[range:" +  array(0) + "-" + array(1) + "|line:" + array(2) + "]").toArray
}