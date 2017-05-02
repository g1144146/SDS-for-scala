package sds.classfile.bytecode

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.bytecode.{MnemonicTable => Table}

sealed abstract class SwitchOpcode(data: Stream, _type: String, pc: Int) extends OpcodeInfo(_type, pc) {
    private var default: Int = -1
    protected var offset: Array[Int] = null
    init()

    def getDefault(): Int = default
    def getOffset(): Array[Int] = offset

    def init(): Unit = {
        skip(1, data)
        this.default = data.readInt() + pc
        if(_type.equals(Table.OPCODES(0xaa))) {
            // in case of "tableswitch""
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
    override def toString(): String = super.toString() + offset.mkString("[", ",", "]")
}

class LookupSwitch(data: Stream, pc: Int) extends SwitchOpcode(data, "lookupswitch", pc) {
    private var _match: Array[Int] = null
    initOffset()

    def getMatch():  Array[Int] = _match
    
    def initOffset(): Unit = {
        val size: Int = data.readInt()
        this._match = new Array(size)
        this.offset = new Array(size)
        (0 until size).foreach((index: Int) => {
            _match(index) = data.readInt()
            offset(index) = data.readInt() + pc
        })
    }

    override def toString(): String = super.toString() +
        (0 until _match.length).map((_: Int) => (_match(_), offset(_))).toArray.mkString("[", "_", "]")
}