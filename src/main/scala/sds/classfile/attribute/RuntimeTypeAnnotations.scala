package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.annotation.{ElementValuePair => Pair}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.AnnotationGenerator.generate
import sds.util.{MultiArgsStringBuilder => Builder}

class RuntimeTypeAnnotations(data: Stream, pool: Array[CInfo], private val name: String) extends AttributeInfo {
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
            println(annotations.mkString("[", ", ", "]"))
        })
    }

    override def toString(): String = {
        val b: Builder = new Builder()
        val sep: String = System.getProperty("line.separator")
        b.append("   ", name, ":", sep)
        types.indices.foreach((index: Int) => {
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

abstract class TargetInfo(__type: TargetType.Value) {
    def _type: TargetType.Value = __type
    override def toString(): String = _type.toString() + ": "
}

object TargetInfo {
    def apply(data: Stream): TargetInfo = {
        val targetType: Int = data.readUnsignedByte()
        targetType match {
            case 0x00
              |  0x01 => new TypeParamTarget(data.readUnsignedByte())
            case 0x10 => new SuperTypeTarget(data.readShort())
            case 0x11
              |  0x12 => new TypeParamBoundTarget(data.readUnsignedByte(), data.readUnsignedByte())
            case 0x13
              |  0x14
              |  0x15 => new EmptyTarget()
            case 0x16 => new MethodFormalParamTarget(data.readUnsignedByte())
            case 0x17 => new ThrowsTarget(data.readShort())
            case 0x40
              |  0x41 =>
                val table: Array[Array[Int]] = (0 until data.readShort()).map((_: Int) => {
                    (0 until 3).map((x: Int) => data.readShort()).toArray
                }).toArray
                new LocalVarTarget(table)
            case 0x42 => new CatchTarget(data.readShort())
            case 0x43
              |  0x44
              |  0x45
              |  0x46 => new OffsetTarget(data.readShort())
            case 0x47
              |  0x48
              |  0x49
              |  0x4A
              |  0x4B => new TypeArgTarget(data.readShort(), data.readUnsignedByte())
            case _    => throw new RuntimeException("unknown target type(" + targetType + ")")
        }
    }
}

class TypeParamTarget(_typeParam: Int) extends TargetInfo(TargetType.TypeParamTarget) {
    def typeParam: Int = _typeParam
    override def toString(): String = super.toString() + "index: " + typeParam
}

class SuperTypeTarget(_superType: Int) extends TargetInfo(TargetType.SuperTypeTarget) {
    def superType: Int = _superType
    override def toString(): String = super.toString() + "index: " + superType
}

class TypeParamBoundTarget(_typeParam: Int, _bounds: Int) extends TargetInfo(TargetType.TypeParamBoundTarget) {
    def typeParam: Int = _typeParam
    def bounds: Int = _bounds
    override def toString(): String = super.toString() + "bounds: " + bounds + ", type_param: " + typeParam
}

class EmptyTarget extends TargetInfo(TargetType.EmptyTarget) { /** has no members **/}

class MethodFormalParamTarget(_formal: Int) extends TargetInfo(TargetType.MethodFormalParamTarget) {
    def formal: Int = _formal
    override def toString(): String = super.toString() + "index: " + formal
}

class ThrowsTarget(_throwsType: Int) extends TargetInfo(TargetType.ThrowsTarget) {
    def throwsType: Int = _throwsType
    override def toString(): String = super.toString() + "index: " + throwsType
}

class LocalVarTarget(_table: Array[Array[Int]]) extends TargetInfo(TargetType.LocalVarTarget) {
    // Array[[start1, len1, index1], [start2, len2, index2], ...]
    def table: Array[Array[Int]] = _table
}

class CatchTarget(_exTable: Int) extends TargetInfo(TargetType.CatchTarget) {
    def exTable: Int = _exTable
    override def toString(): String = super.toString() + "index: " +  exTable
}

class OffsetTarget(_offset: Int) extends TargetInfo(TargetType.OffsetTarget) {
    def offset: Int = _offset
    override def toString(): String = super.toString() + "offset: " + offset
}

class TypeArgTarget(_offset: Int, _typeArg: Int) extends TargetInfo(TargetType.TypeArgTarget) {
    def offset:  Int = _offset
    def typeArg: Int = _typeArg
    override def toString(): String = super.toString() + "index: " + typeArg + ", offset: " + offset
}