package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.util.DescriptorParser.parse

class LocalVariable(data: Stream, pool: Array[CInfo], private val name: String) extends AttributeInfo {
    private var table: Array[Array[Int]] = null
    private var nameTable: Array[Array[String]] = null
    init()

    def init(): Unit = {
        val size: Int = data.short
        this.table = new Array(size)
        this.nameTable = new Array(size)
        (0 until size).foreach((i: Int) => {
            val start: Int = data.short
            val end:   Int = data.short + start
            val nameIndex: Int = data.short
            val descIndex: Int = data.short
            val index: Int = data.short
            val name: String = extract(nameIndex, pool)
            val desc: String = if(descIndex - 1 > 0) parse(extract(descIndex, pool)) else ""
            table(i) = Array(start, end, index)
            nameTable(i) = Array(name, desc)
        })
    }

    def getTable(): Array[Array[Int]] = table
    def getNameTable(): Array[Array[String]] = nameTable
}