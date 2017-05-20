package sds.classfile.attribute

import collection.mutable.{
  LinkedHashMap => Linked,
  HashMap       => Map,
  ArrayBuffer   => Buffer
}
import sds.classfile.{ClassfileStream => Stream}
import sds.classfile.attribute.{VerificationTypeInfo => VTI}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.StackMapFrameParser.parseFrame

class StackMapTable(data: Stream, pool: Array[CInfo], opcodes: Array[Opcode]) extends AttributeInfo {
    val entries: Linked[(Int, Int), Map[String, Buffer[String]]] =
        parseFrame((0 until data.short).map((_: Int) => StackMapFrame(data)).toArray, pool, opcodes)
}

sealed abstract class StackMapFrame(_tag: Int) {
    def tag: Int = _tag
    override def toString(): String = getClass().getSimpleName()
}

object StackMapFrame {
    def apply(data: Stream): StackMapFrame = {
        val tag: Int = data.unsignedByte
        if((0 to 63).contains(tag))    return new SameFrame(tag)
        if((64 to 127).contains(tag))  return new SameLocals1StackItemFrame(tag, data)
        if(tag == 247)                 return new SameLocals1StackItemFrameExtended(tag, data)
        if((248 to 250).contains(tag)) return new ChopFrame(tag, data.short)
        if(tag == 251)                 return new SameFrameExtended(tag, data)
        if((252 to 254).contains(tag)) return new AppendFrame(tag, data)
        if(tag == 255)                 return new FullFrame(tag, data)
        throw new RuntimeException("unknown tag(" + tag + ")")
    }
}

class SameFrame(tag: Int) extends StackMapFrame(tag) {}

class SameLocals1StackItemFrame(tag: Int, data: Stream) extends SameFrame(tag) {
    val stack: VTI = VTI(data)
}

class SameLocals1StackItemFrameExtended(tag: Int, data: Stream) extends SameFrame(tag) {
    val offset: Int = data.short
    val stack:  VTI = VTI(data)
}

class ChopFrame(tag: Int, _offset: Int) extends SameFrame(tag) {
    def offset: Int = _offset
}

class SameFrameExtended(tag: Int, data: Stream) extends ChopFrame(tag, data.short) {}

class AppendFrame(tag: Int, data: Stream) extends ChopFrame(tag, data.short) {
    val locals: Array[VTI] = (0 until (tag - 251)).map((_: Int) => VTI(data)).toArray
}

class FullFrame(tag: Int, data: Stream) extends ChopFrame(tag, data.short) {
    val locals: Array[VTI] = (0 until data.short).map((_: Int) => VTI(data)).toArray
    val stacks: Array[VTI] = (0 until data.short).map((_: Int) => VTI(data)).toArray
}