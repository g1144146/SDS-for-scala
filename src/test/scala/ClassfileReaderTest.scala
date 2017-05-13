package sds

import java.io.InputStream
import java.net.URL
import java.nio.file.Paths
import sds.classfile.MemberInfo
import sds.classfile.attribute.{
  BootstrapMethods,
  Code,
  Exceptions,
  InnerClasses,
  LineNumberTable,
  LocalVariable,
  SourceFile,
  RuntimeAnnotations     => RunAnn,
  RuntimeParameterAnnotations => RunParamAnn,
  RuntimeTypeAnnotations => RunTypeAnn,
  TypeAnnotation
}
import sds.classfile.attribute.AnnotationGenerator.generate
import sds.classfile.bytecode.{
  OpcodeInfo,
  InvokeDynamic
}
import sds.classfile.bytecode.{HasReferenceOpcode => Has}
import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.{InvokeDynamicInfo => Invoke}
import sds.classfile.constant_pool.ConstantType._
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.AccessFlag.get
import org.junit.Test
import org.scalatest.Assertions

class ClassfileReaderTest extends Assertions {
    private val cf_1: Classfile = setUp("Hello.class")
    private val cf_2: Classfile = setUpStream("AnnotatedTest.class")

    private def setUp(file: String): Classfile = {
        val url: URL = getLoader.getResource(file)
        val read: ClassfileReader = new ClassfileReader(Paths.get(url.toURI()).toString())
        read.read()
        read.classfile
    }

    private def setUpStream(file: String): Classfile = {
        val stream: InputStream = getLoader.getResourceAsStream(file)
        val read: ClassfileReader = new ClassfileReader(stream)
        read.read()
        read.classfile
    }

    private def getLoader: ClassLoader = getClass.getClassLoader

    @Test
    def headerTest(): Unit = {
        // header
        assert(Integer.toHexString(cf_1.magic) === "cafebabe")
        assert(cf_1.major === 52)
        assert(cf_1.minor === 0)
        assert(get(cf_1.access, "class") === "public class ")
        // constant pool
        val pool: Array[ConstantInfo] = cf_1.pool
        assert(extract(cf_1.thisClass, pool) === "Hello")
        assert(extract(cf_1.superClass, pool) === "Object")
        assert(pool(0).tag === METHOD)
        assert(pool(0).toString() === "Methodref\t#6.#17")
        assert(pool(2).toString() === "String\t#20")
        assert(pool(4).toString() === "Class\t#23")
        assert(pool(6).toString() === "Utf8\t<init>")
        assert(pool(16).toString() === "NameAndType\t#7:#8")

        val invoke: Invoke = cf_2.pool(1).asInstanceOf[Invoke]
        assert(invoke.toString() === "InvokeDynamic\t#0:#71")
        assert(extract(invoke, cf_2.pool) === "accept|(sds.AnnotatedTest)java.util.function.Consumer")
    }

    @Test
    def fieldTest(): Unit = {
        val f: MemberInfo = cf_2.fields(0)
        assert(f.getType() === "field")
        assert(f.toString() === "private String field")
        val ra: RunAnn = f.getAttributes()(0).asInstanceOf[RunAnn]
        assert(ra.getAnnotations() === Array("@sds.RuntimeAnnotation(value = \"field\")"))
        val ta: TypeAnnotation = f.getAttributes()(1).asInstanceOf[RunTypeAnn].getAnnotations()(0)
        assert(ta.target.toString() === "EmptyTarget")
        assert(generate(ta, cf_2.pool) === "@sds.RuntimeAnnotation(value = \"field\")")
        assert(ta.path === Array())
    }

    @Test
    def methodTest(): Unit = {
        val methods: Array[MemberInfo] = cf_1.methods
        // method header
        val const: MemberInfo = methods(0)
        assert(const.getAccess() === "public ")
        assert(const.getDesc() === "()void")
        assert(const.getName() === "<init>")
        assert(const.toString() === "public <init>()void")
        assert(const.getType() === "method")
        // code
        val code1: Code = const.getAttributes()(0).asInstanceOf[Code]
        assert(code1.getMaxStack() === 1)
        assert(code1.getMaxLocals() === 1)
        // opcode on code
        val ops1: Array[OpcodeInfo] = code1.getOpcodes()
        assert(ops1(0).toString() === "0 - aload_0")
        assert(ops1(1).toString() === "1 - invokespecial: #1(Object.<init>|()void)")
        assert(ops1(2).toString() === "4 - _return")
        // line_number_table on code
        val line1: LineNumberTable = code1.getAttributes()(0).asInstanceOf[LineNumberTable]
        assert(line1.getTable() === Array(Array(0, 0, 1)))
        assert(line1.getTableStr()(0) === "[range:0-0|line:1]")
        // main method
        val main: MemberInfo = methods(1)
        assert(main.getAccess() === "public static ")
        assert(main.getDesc() === "(String[])void")
        assert(main.getName() === "main")
        assert(main.toString() === "public static main(String[])void")
        val code2: Code = main.getAttributes()(0).asInstanceOf[Code]
        val ops2: Array[OpcodeInfo] = code2.getOpcodes()
        val ldc: Has = ops2(1).asInstanceOf[Has]
        assert(ldc.toString() === "3 - ldc: #3(\"Hello World!!\"(String))")
        // exceptions on code
        val ex: Exceptions = main.getAttributes()(1).asInstanceOf[Exceptions]
        assert(ex.getEx() === Array("Exception"))
        assert(ex.toString() === "Exceptions: [Exception]")


        val methods2: Array[MemberInfo] = cf_2.methods
        val local: LocalVariable
            = methods2(0).getAttributes()(0).asInstanceOf[Code].getAttributes()(1).asInstanceOf[LocalVariable]
        assert(local.getNameTable()(0)(0) === "this")
        assert(local.getNameTable()(0)(1) === "sds.AnnotatedTest")
        assert(local.getTable()(0)(0) === 0)
        assert(local.getTable()(0)(1) === 5)
        assert(local.getTable()(0)(2) === 0)

        val cons: MemberInfo = methods2(1)
        val consCode: Code = cons.getAttributes()(0).asInstanceOf[Code]
        val indy: InvokeDynamic = consCode.getOpcodes()(1).asInstanceOf[InvokeDynamic]
        assert(indy.toString() === "1 - invokedynamic: #2(accept|(sds.AnnotatedTest)java.util.function.Consumer)")
        val rta2: RunTypeAnn = consCode.getAttributes()(3).asInstanceOf[RunTypeAnn]
        assert(rta2.getAnnotations()(0).target.toString() === "LocalVarTarget: [start, len, index] = {[7,1,2]}")
        assert(cons.getAttributes()(2).toString() === "Deprecated")
        assert(cons.getAttributes()(3).toString() === "Signature: <T extends Object>(int)void")
        val rta3: Array[TypeAnnotation] = cons.getAttributes()(5).asInstanceOf[RunTypeAnn].getAnnotations()
        assert(rta3(0).target.toString() === "TypeParamTarget: (type_param: 0)")
        assert(rta3(1).target.toString() === "TypeParamBoundTarget: (bounds: 0, type_param: 0)")
        assert(rta3(2).target.toString() === "ThrowsTarget: (throws_type: 0)")
        assert(rta3(3).target.toString() === "MethodFormalParamTarget: (formal_index: 0)")
        val rpa: RunParamAnn = cons.getAttributes()(6).asInstanceOf[RunParamAnn]
        assert(rpa.getAnnotations()(0)(0) === "@sds.RuntimeAnnotation(value = \"method_arg\")")
    }

    @Test
    def attributeTest(): Unit = {
        val source: SourceFile = cf_1.attributes(0).asInstanceOf[SourceFile]
        assert(source.toString() === "SourceFile: Hello.java")

        val inner: InnerClasses = cf_2.attributes(2).asInstanceOf[InnerClasses]
        val innerArray: Array[Array[String]] = Array(
            Array("sds.AnnotatedTest$Inner", "sds.AnnotatedTest", "Inner", "public class "),
            Array("sds.AnnotatedTest$Type", "sds.AnnotatedTest", "Type", "public static final enum "),
            Array("java.lang.invoke.MethodHandles$Lookup", "java.lang.invoke.MethodHandles",
                  "Lookup", "public static final class ")
        )
        assert(inner.getClasses() === innerArray)
        val bsm: Array[(String, Array[String])] = cf_2.attributes(3).asInstanceOf[BootstrapMethods].getBSM()
        val bsmRef: String = "java.lang.invoke.LambdaMetafactory.metafactory|" +
              "(java.lang.invoke.MethodHandles$Lookup,String,java.lang.invoke.MethodType," +
              "java.lang.invoke.MethodType,java.lang.invoke.MethodHandle,java.lang.invoke.MethodType)" +
              "java.lang.invoke.CallSite"
        val obj: String = "(Object)void"
        val bsmArg1: Array[String] = Array(obj, "sds.AnnotatedTest.lambda$method$0|" + obj, obj)
        val bsmArg2: Array[String] = Array(obj, "sds.AnnotatedTest.lambda$method2$1|" + obj, obj)
        assert(bsm(0)._1 === bsmRef)
        assert(bsm(0)._2 === bsmArg1)
        assert(bsm(1)._1 === bsmRef)
        assert(bsm(1)._2 === bsmArg2)
    }
}