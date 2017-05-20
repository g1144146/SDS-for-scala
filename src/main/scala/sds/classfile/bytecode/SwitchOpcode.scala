package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}

sealed abstract class SwitchOpcode(data: Stream, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    private val default: Int = initDefault()
    protected var offset: Array[Int] = null
    init()

    def getDefault(): Int = default
    def getOffset(): Array[Int] = offset

    private def initDefault(): Int = {
        skip(1, data)
        data.readInt() + pc
    }

    private def init(): Unit = {
        if(_type.equals("tableswitch")) {
            val low:  Int = data.readInt()
            val high: Int = data.readInt()
            this.offset = (0 until (high - low + 1)).map((_: Int) => data.readInt() + pc).toArray
        }
    }

    private def skip(index: Int, data: Stream): Unit = {
        if(((index + pc) % 4) == 0) {
            return
        }
        data.readByte()
        skip(index + 1, data)
    }

    override def toString(): String = super.toString() + ": "
}

class TableSwitch(data: Stream, pc: Int) extends SwitchOpcode(data, "tableswitch", pc) {
    override def toString(): String = super.toString() + getOffset().mkString("[", ",", "]")
}

class LookupSwitch(data: Stream, pc: Int) extends SwitchOpcode(data, "lookupswitch", pc) {
    private var _match: Array[Int] = null
    initOffset()

    def getMatch(): Array[Int] = _match
    
    private def initOffset(): Unit = {
        val size: Int = data.readInt()
        this._match = new Array(size)
        this.offset = new Array(size)
        (0 until size).foreach((index: Int) => {
            _match(index) = data.readInt()
            offset(index) = data.readInt() + pc
        })
    }

    override def toString(): String = super.toString() +
        getMatch().indices.map((_: Int) => (getMatch()(_), getOffset()(_))).toArray.mkString("[", "_", "]")
}