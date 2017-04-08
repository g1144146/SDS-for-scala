package sds.classfile.attribute

import sds.classfile.{Information, ClassfileStream => Stream}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.{ConstantInfo => CInfo}

abstract class AttributeInfo() extends Information {
	override def toString(): String = getClass().getSimpleName()
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
			case "LocalVariableTable"
			  |  "LocalVariableTypeTable"               => new LocalVariable(data, pool, name)
			case "MethodParameters"                     => new MethodParameters(data, pool)
			case "RuntimeInvisibleAnnotations"
			  |  "RuntimeVisibleAnnotations"            => new RuntimeAnnotations(data, pool, name)
			case "RuntimeInvisibleParameterAnnotations"
			  |  "RuntimeVisibleParameterAnnotations"   => new RuntimeParameterAnnotations(data, pool, name)
			case "RuntimeInvisibleTypeAnnotations"
			  |  "RuntimeVisibleTypeAnnotations"        => new RuntimeTypeAnnotations(data, pool, name)
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