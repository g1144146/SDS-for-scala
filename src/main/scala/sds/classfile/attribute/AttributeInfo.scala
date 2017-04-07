package sds.classfile.attribute

import sds.classfile.{Information, ClassfileStream}
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

	override def read(stream: ClassfileStream, pool: Array[CInfo]): Unit = {
		throw new UnsupportedOperationException("unimplemented.")
	}

	override def toString(): String = _type.toString()
}

object AttributeInfo {
	def apply(name: String, len: Int): AttributeInfo = name match {
		case "AnnotationDefault"                    => new AnnotationDefault()
		case "BootstrapMethods"                     => new BootstrapMethods()
		case "Code"                                 => new Code()
		case "ConstantValue"                        => new ConstantValue()
		case "Deprecated"                           => new Deprecated()
		case "EnclosingMethod"                      => new EnclosingMethod()
		case "Exceptions"                           => new Exceptions()
		case "InnerClasses"                         => new InnerClasses()
		case "LineNumberTable"                      => new LineNumberTable()
		case "LocalVariableTable"                   => new LocalVariable(LVT)
		case "LocalVariableTypeTable"               => new LocalVariable(LVTT)
		case "MethodParameters"                     => new MethodParameters()
		case "RuntimeInvisibleAnnotations"          => new RuntimeAnnotations(RIA)
		case "RuntimeInvisibleParameterAnnotations" => new RuntimeParameterAnnotations(RIPA)
		case "RuntimeInvisibleTypeAnnotations"      => new RuntimeTypeAnnotations(RITA)
		case "RuntimeVisibleAnnotations"            => new RuntimeAnnotations(RVA)
		case "RuntimeVisibleParameterAnnotations"   => new RuntimeParameterAnnotations(RVPA)
		case "RuntimeVisibleTypeAnnotations"        => new RuntimeTypeAnnotations(RVTA)
		case "Signature"                            => new Signature()
		case "SourceDebugExtension"                 => new SourceDebugExtension(len)
		case "SourceFile"                           => new SourceFile()
		case "Synthetic"                            => new Synthetic()
		case "StackMapTable"                        => new StackMapTable()
		case _ => throw new IllegalArgumentException("unknown attribute name(" + name + ").")
	}
}