package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.bytecode.{MnemonicTable => Table}

sealed abstract class SwitchOpcode(_type: Table.Value, pc: Int) extends OpcodeInfo(_type, pc) {
	private var default: Int = -1
	protected var offset: Array[Int] = null

	def getDefault(): Int = default
	def getOffset(): Array[Int] = offset

	override def read(data: ClassfileStream): Unit = {
		skip(1, data)
		this.default = data.readInt() + pc
	}

	private def skip(index: Int, data: ClassfileStream): Unit = {
		if(((index + pc) % 4) == 0) {
			return
		}
		data.readByte()
		skip(index + 1, data)
	}

	override def toString(): String = super.toString() + ": "
}

class TableSwitch(pc: Int) extends SwitchOpcode(Table.tableswitch, pc) {
	override def read(data: ClassfileStream): Unit = {
		super.read(data)
		val low:  Int = data.readInt()
		val high: Int = data.readInt()
		this.offset = (0 until (high - low + 1)).map((_: Int) => data.readInt() + pc).toArray
	}

	override def toString(): String = super.toString() + offset.mkString("[", ",", "]")
}

class LookupSwitch(pc: Int) extends SwitchOpcode(Table.lookupswitch, pc) {
	private var _match: Array[Int] = null

	def getMatch():  Array[Int] = _match
	
	override def read(data: ClassfileStream): Unit = {
		super.read(data)
		val size: Int = data.readInt()
		this._match = new Array(size)
		this.offset = new Array(size)
		(0 until size).foreach((index: Int) => {
			_match(index) = data.readInt()
			offset(index) = data.readInt() + pc
		})
	}

	override def toString(): String = {
		super.toString() + (0 until _match.length).map((_: Int) => {
				(_match(_), offset(_))
		}).toArray.mkString("[", "_", "]")
	}
}