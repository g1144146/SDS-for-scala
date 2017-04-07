package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.annotation.{ElementValuePair => Pair}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.util.AnnotationGenerator.generate
import sds.util.{MultiArgsStringBuilder => Builder}

class RuntimeTypeAnnotations(data: Stream, pool: Array[CInfo], t: AttributeType.Value) extends  AttributeInfo(t) {
	private var types: Array[TypeAnnotation] = null
	private var annotations: Array[String] = null
	init()

	def getTypes(): Array[TypeAnnotation] = types
	def getAnnotations(): Array[String] = annotations
	def init(): Unit = {
		val size: Int = data.readShort()
		this.types = new Array(size)
		this.annotations = new Array(size)
		(0 until size).foreach((index: Int) => {
			types(index) = new TypeAnnotation(data)
			annotations(index) = generate(data, pool)
		})
	}

	override def toString(): String = {
		val b: Builder = new Builder()
		val sep: String = System.getProperty("line.separator")
		b.append("   ", super.toString(), ":", sep)
		(0 until types.length).foreach((index: Int) => {
			b.append("      ", index, ".", annotations(index), sep)
			b.append("         ", types(index).getInfo(), sep)
			b.append("         ", types(index).getPath(), sep)
		})
		b.toString()
	}
}

class TypeAnnotation(data: Stream) {
	private val info: TargetInfo = TargetInfo(data)
	private val path: Array[(Int, Int)] = (0 until data.readUnsignedByte()).map((_: Int) => {
		// path_king, arg_index
		(data.readByte(), data.readByte())
	}).toArray
	private val typeIndex: Int = data.readShort()
	private val pairs: Array[Pair] = (0 until data.readShort()).map((_: Int) => new Pair(data)).toArray
	
	def getInfo(): TargetInfo = info
	def getPath(): Array[(Int, Int)] = path
	def getType(): Int = typeIndex
	def getPairs(): Array[Pair] = pairs
}

object TargetType extends Enumeration {
	val
	TypeParamTarget, SuperTypeTarget, TypeParamBoundTarget, EmptyTarget,  MethodFormalParamTarget,
	ThrowsTarget,    LocalVarTarget,  CatchTarget,          OffsetTarget, TypeArgTarget
	= Value
}

abstract class TargetInfo(private val _type: TargetType.Value) {
	def getType(): TargetType.Value = _type
	override def toString(): String = _type.toString() + ": "
}

class TypeParamTarget(private val typeParam: Int) extends TargetInfo(TargetType.TypeParamTarget) {
	def getTypeParam(): Int = typeParam
	override def toString(): String = super.toString() + "index:" + typeParam
}

class SuperTypeTarget(private val superType: Int) extends TargetInfo(TargetType.SuperTypeTarget) {
	def getSuperType(): Int = superType
	override def toString(): String = super.toString() + "index:" + superType
}

class TypeParamBoundTarget(private val typeParam: Int, private val bounds: Int) extends
TargetInfo(TargetType.TypeParamBoundTarget) {
	def getTypeParam(): Int = typeParam
	def getBounds(): Int = bounds
	override def toString(): String = super.toString() + "bounds:" + bounds + ", type_param:" + typeParam
}

class EmptyTarget extends TargetInfo(TargetType.EmptyTarget) { /** has no members **/}

class MethodFormalParamTarget(private val formal: Int) extends TargetInfo(TargetType.MethodFormalParamTarget) {
	def getFormalParam(): Int = formal
	override def toString(): String = super.toString() + "index:" + formal
}

class ThrowsTarget(private val throwsType: Int) extends TargetInfo(TargetType.ThrowsTarget) {
	def getThrowsType(): Int = throwsType
	override def toString(): String = super.toString() + "index:" + throwsType
}

class LocalVarTarget(data: Stream) extends TargetInfo(TargetType.LocalVarTarget) {
	val table: Array[Int] = (0 until data.readShort()).map((_: Int) => data.readShort()).toArray
	
	def getStart(): Int = table(0)
	def getLen():   Int = table(1)
	def getIndex(): Int = table(2)
	override def toString(): String = {
		super.toString() + "index:" + table(2) + ", pc:" + table(0) + "-" + (table(0) + table(1))
	}
}

class CatchTarget(private val exTable: Int) extends TargetInfo(TargetType.CatchTarget) {
	def getExTable(): Int = exTable
	override def toString(): String = super.toString() + "index:" +  exTable
}

class OffsetTarget(private val offset: Int) extends TargetInfo(TargetType.OffsetTarget) {
	def getOffset(): Int = offset
	override def toString(): String = super.toString() + "offset:" + offset
}

class TypeArgTarget(private val offset: Int, private val typeArg: Int) extends
TargetInfo(TargetType.TypeArgTarget) {
	def getOffSet():  Int = offset
	def getTypeArg(): Int = typeArg
	override def toString(): String = super.toString() + "index:" + typeArg + ", offset:" + offset
}

object TargetInfo {
	def apply(data: Stream): TargetInfo = {
		val targetType: Int = data.readUnsignedByte()
		targetType match {
			case 0x00|0x01                => new TypeParamTarget(data.readUnsignedByte())
			case 0x10                     => new SuperTypeTarget(data.readShort())
			case 0x11|0x12      => new TypeParamBoundTarget(data.readUnsignedByte(), data.readUnsignedByte())
			case 0x13|0x14|0x15           => new EmptyTarget()
			case 0x16                     => new MethodFormalParamTarget(data.readUnsignedByte())
			case 0x17                     => new ThrowsTarget(data.readShort())
			case 0x40|0x41                => new LocalVarTarget(data)
			case 0x42                     => new CatchTarget(data.readShort())
			case 0x43|0x44|0x45|0x46      => new OffsetTarget(data.readShort())
			case 0x47|0x48|0x49|0x4A|0x4B => new TypeArgTarget(data.readShort(), data.readUnsignedByte())
			case _ => throw new RuntimeException("unknown target type(" + targetType + ")")
		}
	}
}