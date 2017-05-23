package sds.classfile.attribute

import scala.annotation.tailrec
import collection.mutable.{ArrayBuffer => Buffer}
import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo, Utf8Info => Utf8}

class Code(data: Stream, pool: Array[CInfo]) extends AttributeInfo {
    val maxStack:  Int = data.short
    val maxLocals: Int = data.short
    val opcodes: Array[Opcode] = initOpcodes()
    val exTable: Array[(Array[Int], String)] = (0 until data.short).map((_: Int) => {
        // 0: start, 1: end, 2: handle
        val indexes: Array[Int] = (0 until 3).map((_: Int) => data.short).toArray
        val catchType: Int = data.short
        val target: String = if(catchType == 0) "any" else extract(catchType, pool)
        (indexes, target)
    }).toArray
    val attributes: Array[AttributeInfo] = (0 until data.short).map((_: Int) => {
        val name: Int = data.short
        val utf8: Utf8 = pool(name - 1).asInstanceOf[Utf8]
        AttributeInfo(utf8.value, data, pool, opcodes)
    }).toArray

    private def initOpcodes(): Array[Opcode] = {
        val len: Int = data.int
        val pointer: Int = data.pointer
        readOpcode(len, pointer, data, pool, Buffer[Opcode]())
    }

    @tailrec
    private def readOpcode(len: Int, pointer: Int, data: Stream, pool: Array[CInfo], buffer: Buffer[Opcode]):
    Array[Opcode] = {
        val index: Int = data.pointer
        if(index >= (len + pointer)) {
            return buffer.toArray
        }
        val pc: Int = index - pointer
        val info: Opcode = Opcode(pc, data, pool)
        readOpcode(len, pointer, data, pool, buffer += info)
    }
}