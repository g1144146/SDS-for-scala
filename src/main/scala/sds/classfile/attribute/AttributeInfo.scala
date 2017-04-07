package sds.classfile.attribute

import sds.classfile.{Information, ClassfileStream => Stream}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}
import sds.classfile.attribute.AttributeType.{LocalVariableTable => LVT,
											  LocalVariableTypeTable => LVTT,
                                              RuntimeVisibleAnnotations => RVA,
											  RuntimeInvisibleAnnotations => RIA,
                                              RuntimeVisibleTypeAnnotations => RVTA,
											  RuntimeInvisibleTypeAnnotations => RITA,
                                              RuntimeVisibleParameterAnnotations => RVPA,
											  RuntimeInvisibleParameterAnnotations => RIPA}

abstract class AttributeInfo(private val _type: AttributeType.Value) extends Information {
	def getType(): AttributeType.Value = _type
	override def toString(): String = _type.toString()
}

object AttributeInfo {
	def apply(name: String, data: Stream, pool: Array[CInfo]): AttributeInfo = {
		val len: Int = data.readInt()
		name match {
			case "AnnotationDefault"                    => new AnnotationDefault(data, pool)
			case "BootstrapMethods"                     => new BootstrapMethods(data, pool)
			case "Code"                                 => new Code(data, pool)
			case "ConstantValue"                        => new ConstantValue(data, pool)
			case "Deprecated"                           => new Deprecated()
			case "EnclosingMethod"                      => new EnclosingMethod(data, pool)
			case "Exceptions"                           => new Exceptions(data, pool)
			case "InnerClasses"                         => new InnerClasses(data, pool)
			case "LineNumberTable"                      => new LineNumberTable(data, pool)
			case "LocalVariableTable"                   => new LocalVariable(data, pool, LVT)
			case "LocalVariableTypeTable"               => new LocalVariable(data, pool, LVTT)
			case "MethodParameters"                     => new MethodParameters(data, pool)
			case "RuntimeInvisibleAnnotations"          => new RuntimeAnnotations(data, pool, RIA)
			case "RuntimeInvisibleParameterAnnotations" => new RuntimeParameterAnnotations(data, pool, RIPA)
			case "RuntimeInvisibleTypeAnnotations"      => new RuntimeTypeAnnotations(data, pool, RITA)
			case "RuntimeVisibleAnnotations"            => new RuntimeAnnotations(data, pool, RVA)
			case "RuntimeVisibleParameterAnnotations"   => new RuntimeParameterAnnotations(data, pool, RVPA)
			case "RuntimeVisibleTypeAnnotations"        => new RuntimeTypeAnnotations(data, pool, RVTA)
			case "Signature"                            => new Signature(data, pool)
			case "SourceDebugExtension"                 => new SourceDebugExtension(data, len)
			case "SourceFile"                           => new SourceFile(data, pool)
			case "Synthetic"                            => new Synthetic()
			case _ => throw new IllegalArgumentException("unknown attribute name(" + name + ").")
		}
	}

	def apply(name: String, data: Stream, pool: Array[CInfo], opcodes: Array[Opcode]): AttributeInfo = {
		if(name.equals("StackMapTable")) {
			val len: Int = data.readInt()
			new StackMapTable(data, pool, opcodes)
		} else {
			apply(name, data, pool)
		}
	}
}