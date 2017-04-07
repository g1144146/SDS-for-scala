package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo

class EnclosingMethod extends AttributeInfo(AttributeType.EnclosingMethod) {
	private var _class: String = ""
	private var method: String = ""

	def getEncClass(): String = _class
	def getMethod(): String = method

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this._class = extract(data.readShort(), pool)
		val index: Int = data.readShort()
		this.method = if(index - 1 > 0) extract(index, pool) else ""
	}

	override def toString(): String = super.toString() + ": " + _class + " " + method
}