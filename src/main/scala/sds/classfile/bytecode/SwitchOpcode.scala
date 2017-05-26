package sds.classfile.bytecode

import scala.annotation.tailrec
import sds.classfile.{ClassfileStream => Stream}

sealed abstract class SwitchOpcode(data: Stream, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    val default: Int = initDefault()
    protected var offset: Array[Int] = null

    def getOffset(): Array[Int] = offset

    private def initDefault(): Int = {
        skip(1 + pc, data)
        data.int + pc
    }

    @tailrec
    private def skip(index: Int, data: Stream): Unit = {
        if((index % 4) == 0) {
            return
        }
        data.byte
        skip(index + 1, data)
    }
}

class TableSwitch(data: Stream, pc: Int) extends SwitchOpcode(data, "tableswitch", pc) {
    this.offset = ((low: Int, high: Int) => {
        (0 until (high - low + 1)).map((_: Int) => data.int + pc).toArray
    })(data.int, data.int)
    override def toString(): String = super.toString() + s": [${getOffset().mkString(", ")}, $default(default)]"
}

class LookupSwitch(data: Stream, pc: Int) extends SwitchOpcode(data, "lookupswitch", pc) {
    private var _match: Array[Int] = null
    initOffset()

    def getMatch(): Array[Int] = _match
    
    private def initOffset(): Unit = {
        val size: Int = data.int
        this._match = new Array(size)
        this.offset = new Array(size)
        (0 until size).foreach((index: Int) => {
            _match(index) = data.int
            offset(index) = data.int + pc
        })
    }

    override def toString(): String = super.toString() + s": [${getMatch().indices.map((i: Int) => {
        getMatch()(i) + ":" + getOffset()(i)}).mkString(", ")}, default:$default]"
}