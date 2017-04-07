package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}

sealed abstract class VerificationTypeInfo(private val tag: Int, private val _type: VerificationType.Value) {
	def getTag(): Int = tag
	def getType(): VerificationType.Value = _type
}

object VerificationType extends Enumeration {
	val
	TopVar,              IntVar,    FloatVar,
	DoubleVar,           LongVar,   NullVar,
	UninitializedThisVar,ObjectVar, UninitializedVar
	= Value
}

object VerificationTypeInfo {
	def apply(data: Stream): VerificationTypeInfo = {
		val tag: Int = data.readUnsignedByte()
		tag match {
			case 0 => new VerificationTypeInfoAdapter(tag, VerificationType.TopVar);
			case 1 => new VerificationTypeInfoAdapter(tag, VerificationType.IntVar);
			case 2 => new VerificationTypeInfoAdapter(tag, VerificationType.FloatVar);
			case 3 => new VerificationTypeInfoAdapter(tag, VerificationType.DoubleVar);
			case 4 => new VerificationTypeInfoAdapter(tag, VerificationType.LongVar);
			case 5 => new VerificationTypeInfoAdapter(tag, VerificationType.NullVar);
			case 6 => new VerificationTypeInfoAdapter(tag, VerificationType.UninitializedThisVar);
			case 7 => new ObjectVar(tag, data.readShort());
			case 8 => new UninitializedVar(tag, data.readShort());
		}
	}
}

class VerificationTypeInfoAdapter(tag: Int, _type: VerificationType.Value) extends
VerificationTypeInfo(tag, _type) {}

class ObjectVar(tag: Int, cpool: Int) extends VerificationTypeInfo(tag, VerificationType.ObjectVar) {
	def getCPool(): Int = cpool
}

class UninitializedVar(tag: Int, offset: Int) extends VerificationTypeInfo(tag, VerificationType.UninitializedVar) {
	def getOffset(): Int = offset
}