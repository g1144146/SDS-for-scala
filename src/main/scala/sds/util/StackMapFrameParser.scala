package sds.util

import collection.mutable.{ArrayBuffer => Buffer, HashMap => Map, LinkedHashMap => Linked}
import sds.classfile.attribute.{StackMapFrame => Frame,
                                SameLocals1StackItemFrame => SLSIF,
								SameLocals1StackItemFrameExtended => SLSIFE,
								ChopFrame => Chop, SameFrameExtended => SFE,
								AppendFrame => Append, FullFrame => Full,
                                VerificationTypeInfo => Verify,
                                ObjectVar, UninitializedVar => UVar}
import sds.classfile.attribute.FrameType._
import sds.classfile.attribute.VerificationType._
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}

import sds.classfile.bytecode.Operand.get
import sds.util.DescriptorParser.parse
import sds.util.Utf8ValueExtractor.extract

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
		frames.map((frame: Frame) => {
			val map: Map[String, Buffer[String]] = Map()
			val local: Buffer[String] = getBefore(parsed, before, "local")
			val list: Buffer[String] = Buffer()
			var key: Int = 0
			frame.getType() match {
				case SameFrame =>
					map.put("stack", list)
					map.put("local", local)
					key = frame.getTag()
				case SameLocals1StackItemFrame =>
					val sl: SLSIF = frame.asInstanceOf[SLSIF]
					list += parseVerify(sl.getStack(), pool, opcodes)
					map.put("stack", list)
					map.put("local", local)
					key = sl.getTag() - 64
				case SameLocals1StackItemFrameExtended =>
					val sle: SLSIFE = frame.asInstanceOf[SLSIFE]
					list += parseVerify(sle.getStack(), pool, opcodes)
					map.put("stack", list);
					map.put("local", local);
					key = sle.getOffset();
				case ChopFrame =>
					val chop: Chop = frame.asInstanceOf[Chop]
					val deleteArg: Int = 251 - chop.getTag();
					val argCount:  Int = local.size;
					(((argCount - 1) - deleteArg) - 1 to argCount - 1 by -1).foreach(local.remove(_))
//					for(int i = argCount-1; i > ((argCount-1) - deleteArg); i--) {
//						local.remove(i);
//					}
					map.put("stack", list);
					map.put("local", local);
					key = chop.getOffset();
				case SameFrameExtended =>
					val sfe: SFE = frame.asInstanceOf[SFE]
					map.put("stack", list);
					map.put("local", local);
					key = sfe.getOffset();
				case AppendFrame =>
					val app: Append = frame.asInstanceOf[Append]
					app.getLocals().foreach(local += parseVerify(_, pool, opcodes))
					map.put("stack", list);
					map.put("local", local);
					key = app.getOffset();
				case FullFrame =>
					val full: Full = frame.asInstanceOf[Full]
					val ffStack: Buffer[String] = full.getStacks().map((v: Verify) => {
						parseVerify(v, pool, opcodes)
					}).toBuffer.asInstanceOf[Buffer[String]]
					val ffLocal: Buffer[String] = full.getLocals().map((v: Verify) => {
						parseVerify(v, pool, opcodes)
					}).toBuffer.asInstanceOf[Buffer[String]]
					map.put("stack", ffStack);
					map.put("local", ffLocal);
					key = full.getOffset();
			}
			parsed.put(key, map)
			before = key
		})
		parsed
	}

	private def parseVerify(info: Verify, pool: Array[CInfo], opcodes: Array[Opcode]): String = {
		info.getType() match {
			case TopVar               => "top";
			case IntVar               => "int";
			case FloatVar             => "float";
			case LongVar              => "long";
			case DoubleVar            => "double";
			case NullVar              => "null";
			case UninitializedThisVar => "";
			case ObjectVar =>
				val ov: ObjectVar = info.asInstanceOf[ObjectVar];
				val value: String = extract(ov.getCPool(), pool);
				if(value.startsWith("[")) parse(value) else value;
			case UninitializedVar =>
				val uv: UVar = info.asInstanceOf[UVar];
				get(opcodes(uv.getOffset()), pool);
			case _ => ""
		}
	}
}