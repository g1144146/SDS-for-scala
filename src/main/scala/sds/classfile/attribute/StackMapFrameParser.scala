package sds.classfile.attribute

import collection.mutable.{
  ArrayBuffer => Buffer,
  HashMap => Map,
  LinkedHashMap => Link
}
import sds.classfile.attribute.{StackMapFrame => Frame, VerificationTypeInfo => Verify}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.bytecode.Operand.get
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.DescriptorParser.parse

object StackMapFrameParser {
    private def getBefore(parsed: Link[(Int, Int), Map[String, Buffer[String]]], before: (Int, Int)): Buffer[String] = {
        if(parsed.contains(before) && (parsed(before) != null)) {
            if(parsed(before).contains("local") && parsed(before)("local") != null) {
                return parsed(before)("local").clone()
            }
        }
        Buffer()
    }

    def parseFrame(frames: Array[Frame], pool: Array[CInfo], opcodes: Array[Opcode]):
    Link[(Int, Int), Map[String, Buffer[String]]] = {
        var before: Int = 0
        var beforeTag: Int = 0
        val parsed: Link[(Int, Int), Map[String, Buffer[String]]] = Link()
        frames.foreach((frame: Frame) => {
            val map: Map[String, Buffer[String]] = Map()
            val local: Buffer[String] = getBefore(parsed, (beforeTag, before))
            val key: Int = frame match {
                case sl: SameLocals1StackItemFrame =>
                    map.put("stack", Buffer(parseVerify(sl.stack, pool, opcodes)))
                    map.put("local", local)
                    sl.tag - 64
                case sle: SameLocals1StackItemFrameExtended =>
                    map.put("stack", Buffer(parseVerify(sle.stack, pool, opcodes)))
                    map.put("local", local)
                    sle.offset
                case sfe: SameFrameExtended =>
                    map.put("stack", Buffer())
                    map.put("local", local)
                    sfe.offset
                case app: AppendFrame =>
                    app.locals.foreach((v: Verify) => local += parseVerify(v, pool, opcodes))
                    map.put("stack", Buffer())
                    map.put("local", local)
                    app.offset
                case full: FullFrame =>
                    val ffStack: Buffer[String] = full.stacks.map((v: Verify) => {
                        parseVerify(v, pool, opcodes)
                    }).toBuffer.asInstanceOf[Buffer[String]]
                    val ffLocal: Buffer[String] = full.locals.map((v: Verify) => {
                        parseVerify(v, pool, opcodes)
                    }).toBuffer.asInstanceOf[Buffer[String]]
                    map.put("stack", ffStack)
                    map.put("local", ffLocal)
                    full.offset
                case chop: ChopFrame =>
                    val delArg: Int = 251 - chop.tag
                    (0 until delArg).foreach((i :Int) => local.remove(local.size - 1))
                    map.put("stack", Buffer())
                    map.put("local", local)
                    chop.offset
                case s: SameFrame =>
                    map.put("stack", Buffer())
                    map.put("local", local)
                    s.tag
                case _ => throw new RuntimeException("invalid StackMapFrame type.")
            }
            parsed.put((frame.tag, key), map)
            before = key
            beforeTag = frame.tag
        })
        parsed
    }

    private def parseVerify(info: Verify, pool: Array[CInfo], opcodes: Array[Opcode]): String = info match {
        case ov: ObjectVar        =>
            val value: String = extract(ov.cpool, pool)
            if(value.startsWith("[")) parse(value) else value
        case uv: UninitializedVar => get(opcodes(uv.offset), pool)
        case _                    => info.toString()
    }
}