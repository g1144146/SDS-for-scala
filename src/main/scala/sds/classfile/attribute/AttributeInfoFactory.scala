package sds.classfile.attribute

class AttributeInfoFactory {
	def create(name: String, len: Int): AttributeInfo = {
		name match {
			case "AnnotationDefault" => new AnnotationDefault()
			case "BootstrapMethods" => new BootstrapMethods()
			case "Code" => new Code()
			case "ConstantValue" => new ConstantValue()
			case "Deprecated" => new Deprecated()
			case "EnclosingMethod" => new EnclosingMethod()
			case "Exceptions" => new Exceptions()
			case "InnerClasses" => new InnerClasses()
			case "LineNumberTable" => new LineNumberTable()
			case "LocalVariableTable" => new LocalVariableTable()
			case "LocalVariableTypeTable" => new LocalVariableTypeTable()
			case "MethodParameters" => new MethodParameters()
			case "RuntimeInvisibleAnnotations" => new RuntimeInvisibleAnnotations()
			case "RuntimeInvisibleParameterAnnotations" => new RuntimeInvisibleParameterAnnotations()
			case "RuntimeInvisibleTypeAnnotations" => new RuntimeInvisibleTypeAnnotations()
			case "RuntimeVisibleAnnotations" => new RuntimeVisibleAnnotations()
			case "RuntimeVisibleParameterAnnotations" => new RuntimeVisibleParameterAnnotations()
			case "RuntimeVisibleTypeAnnotations" => new RuntimeVisibleTypeAnnotations()
			case "Signature" => new Signature()
			case "SourceDebugExtension" => new SourceDebugExtension(length)
			case "SourceFile" => new SourceFile()
			case "Synthetic" => new Synthetic()
			case "StackMapTable" => new StackMapTable()
			case _ => throw new IllegalArgumentException()
	}
}