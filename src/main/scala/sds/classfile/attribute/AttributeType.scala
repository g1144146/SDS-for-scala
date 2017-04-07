package sds.classfile.attribute

object AttributeType extends Enumeration {
	val
	AnnotationDefault,
	BootstrapMethods,
	Code,
	ConstantValue,
	Deprecated,
	Exceptions,
	EnclosingMethod,
	InnerClasses,
	LineNumberTable,
	LocalVariableTable,
	LocalVariableTypeTable,
	MethodParameters,
	RuntimeVisibleAnnotations,
	RuntimeInvisibleAnnotations,
	RuntimeVisibleParameterAnnotations,
	RuntimeInvisibleParameterAnnotations,
	RuntimeVisibleTypeAnnotations,
	RuntimeInvisibleTypeAnnotations,
	Signature,
	SourceFile,
	SourceDebugExtension,
	StackMapTable,
	Synthetic
	= Value
}