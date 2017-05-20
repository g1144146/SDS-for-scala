package sds.classfile.attribute

import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.annotation.Annotation

class RuntimeTypeAnnotations(data: Stream, _name: String) extends AttributeInfo {
    val annotations: Array[TypeAnnotation] = (0 until data.short).map((index: Int) => {
        val info: TargetInfo = TargetInfo(data)
        val path: Array[(Int, Int)] = (0 until data.unsignedByte).map((_: Int) => {
            // path_king, arg_index
            (data.byte, data.byte)
        }).toArray
        new TypeAnnotation(info, path, data)
    }).toArray

    def name: String = _name
}

class TypeAnnotation(_target: TargetInfo, _path: Array[(Int, Int)], data: Stream) extends Annotation(data) {
    def target: TargetInfo = _target
    def path: Array[(Int, Int)] = _path
}

sealed abstract class TargetInfo {
    override def toString(): String = getClass().getSimpleName()
}

object TargetInfo {
    def apply(data: Stream): TargetInfo = data.unsignedByte match {
        case 0x00
          |  0x01 => new TypeParamTarget(data.unsignedByte)
        case 0x10 => new SuperTypeTarget(data.short)
        case 0x11
          |  0x12 => new TypeParamBoundTarget(data.unsignedByte, data.unsignedByte)
        case 0x13
          |  0x14
          |  0x15 => new EmptyTarget()
        case 0x16 => new MethodFormalParamTarget(data.unsignedByte)
        case 0x17 => new ThrowsTarget(data.short)
        case 0x40
          |  0x41 =>
                     val table: Array[Array[Int]] = (0 until data.short).map((_: Int) => {
                         (0 until 3).map((x: Int) => data.short).toArray
                     }).toArray
                     new LocalVarTarget(table)
        case 0x42 => new CatchTarget(data.short)
        case 0x43
          |  0x44
          |  0x45
          |  0x46 => new OffsetTarget(data.short)
        case 0x47
          |  0x48
          |  0x49
          |  0x4A
          |  0x4B => new TypeArgTarget(data.short, data.unsignedByte)
        case _    => throw new RuntimeException("unknown target type.")
    }
}

class EmptyTarget extends TargetInfo {}

class TypeParamTarget(_typeParam: Int) extends TargetInfo {
    def typeParam: Int = _typeParam
    override def toString(): String = super.toString() + ": (type_param: " + typeParam + ")"
}

class SuperTypeTarget(_superType: Int) extends TargetInfo {
    def superType: Int = _superType
    override def toString(): String = super.toString() + ": (super_type: " + superType + ")"
}

class MethodFormalParamTarget(_formal: Int) extends TargetInfo {
    def formal: Int = _formal
    override def toString(): String = super.toString() + ": (formal_index: " + formal + ")"
}

class ThrowsTarget(_throwsType: Int) extends TargetInfo {
    def throwsType: Int = _throwsType
    override def toString(): String = super.toString() + ": (throws_type: " + throwsType + ")"
}

class CatchTarget(_exTable: Int) extends TargetInfo {
    def exTable: Int = _exTable
    override def toString(): String = super.toString() + ": (catch_type: " +  exTable + ")"
}

class OffsetTarget(_offset: Int) extends TargetInfo {
    def offset: Int = _offset
    override def toString(): String = super.toString() + ": (offset: " + offset + ")"
}

class LocalVarTarget(_table: Array[Array[Int]]) extends TargetInfo {
    // Array[[start1, len1, index1], [start2, len2, index2], ...]
    def table: Array[Array[Int]] = _table
    override def toString(): String = super.toString() + ": [start, len, index] = " +
        table.map((array: Array[Int]) => array.mkString("[", ",", "]")).mkString("{", ", ", "}")
}

class TypeParamBoundTarget(_typeParam: Int, _bounds: Int) extends TargetInfo {
    def typeParam: Int = _typeParam
    def bounds:    Int = _bounds
    override def toString(): String = super.toString() + ": (bounds: " + bounds + ", type_param: " + typeParam + ")"
}

class TypeArgTarget(_offset: Int, _typeArg: Int) extends TargetInfo {
    def offset:  Int = _offset
    def typeArg: Int = _typeArg
    override def toString(): String = super.toString() + ": (type_arg: " + typeArg + ", offset: " + offset + ")"
}