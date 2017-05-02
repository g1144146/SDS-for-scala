package sds.classfile.attribute

import collection.mutable.{
  ArrayBuffer   => Buffer,
  HashMap       => Map,
  LinkedHashMap => Linked
}
import sds.classfile.attribute.{
  StackMapFrame        => Frame,
  VerificationTypeInfo => Verify
}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}

import sds.classfile.bytecode.Operand.get
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.DescriptorParser.parse

object StackMapFrameParser {
	private def getBefore(parsed: Linked[Int, Map[String, Buffer[String]]], before: Int, _type: String):
	Buffer[String] = {
		if(parsed.contains(before) && (parsed(before) != null)) {
			if(parsed(before).get(_type) ne null) {
				return parsed(before).get(_type).get
			}
		}
		Buffer()
	}

	def parseFrame(frames: Array[Frame], pool: Array[CInfo], opcodes: Array[Opcode]):
	Linked[Int, Map[String, Buffer[String]]] = {
		var before: Int = 0
		val parsed: Linked[Int, Map[String, Buffer[String]]] = Linked()
		frames.foreach((frame: Frame) => {
			val map: Map[String, Buffer[String]] = Map()
			val local: Buffer[String] = getBefore(parsed, before, "local")
			val list: Buffer[String] = Buffer()
			var key: Int = 0
			frame match {
				case sl: SameLocals1StackItemFrame =>
					list += parseVerify(sl.getStack(), pool, opcodes)
					map.put("stack", list)
					map.put("local", local)
					key = sl.tag - 64
				case sle: SameLocals1StackItemFrameExtended =>
					list += parseVerify(sle.getStack(), pool, opcodes)
					map.put("stack", list)
					map.put("local", local)
					key = sle.getOffset()
				case sfe: SameFrameExtended =>
					map.put("stack", list)
					map.put("local", local)
					key = sfe.offset
				case app: AppendFrame =>
					app.getLocals().foreach(local += parseVerify(_, pool, opcodes))
					map.put("stack", list)
					map.put("local", local)
					key = app.offset
				case full: FullFrame =>
					val ffStack: Buffer[String] = full.getStacks().map((v: Verify) => {
						parseVerify(v, pool, opcodes)
					}).toBuffer.asInstanceOf[Buffer[String]]
					val ffLocal: Buffer[String] = full.getLocals().map((v: Verify) => {
						parseVerify(v, pool, opcodes)
					}).toBuffer.asInstanceOf[Buffer[String]]
					map.put("stack", ffStack)
					map.put("local", ffLocal)
					key = full.offset
				case chop: ChopFrame =>
					val deleteArg: Int = 251 - chop.tag
					val argCount:  Int = local.size
					(((argCount - 1) - deleteArg) - 1 to argCount - 1 by -1).foreach(local.remove(_))
//					for(int i = argCount-1 i > ((argCount-1) - deleteArg) i--) {
//						local.remove(i)
//					}
					map.put("stack", list)
					map.put("local", local)
					key = chop.offset
				case s: SameFrame =>
					map.put("stack", list)
					map.put("local", local)
					key = frame.tag
			}
			parsed.put(key, map)
			before = key
		})
		parsed
	}

	private def parseVerify(info: Verify, pool: Array[CInfo], opcodes: Array[Opcode]): String = info match {
		case ov: ObjectVar =>
			val value: String = extract(ov.cpool, pool)
			if(value.startsWith("[")) parse(value) else value
		case uv: UninitializedVar => get(opcodes(uv.offset), pool)
		case _                    => info.toString()
	}
}