package sds

import java.io.{InputStream, ByteArrayInputStream}
import java.net.URL
import java.nio.file.Paths

import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.MemberInfo
import sds.classfile.attribute.AnnotationGenerator.generate
import sds.classfile.attribute.{
  AnnotationDefault,
  BootstrapMethods,
  Code, Exceptions,
  InnerClasses,
  LineNumberTable,
  LocalVariable,
  SourceFile,
  TypeAnnotation,
  RuntimeAnnotations => RunAnn,
  RuntimeParameterAnnotations => RunParamAnn,
  RuntimeTypeAnnotations => RunTypeAnn,
  StackMapTable => Stack
}
import sds.classfile.bytecode.{
  InvokeDynamic,
  LookupSwitch,
  MultiANewArray => Multi,
  OpcodeInfo,
  Operand,
  TableSwitch,
  HasReferenceOpcode => Has
}
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.classfile.constant_pool.{ConstantInfo, InvokeDynamicInfo => Invoke}
import sds.util.AccessFlag.get
import sds.util.ClassfilePrinter

import scala.collection.mutable.{ArrayBuffer => Buffer}

class ClassfileReaderTest extends Assertions {
    private val cf_1: Classfile = setUp("Hello.class")
    private val cf_2: Classfile = setUpStream("AnnotatedTest.class")
    private val cf_3: Classfile = setUp("RuntimeAnnotation.class")
    private val cf_4: Classfile = setUpStream("Resize.class")
    private val cf_5: Classfile = setUp("HogeMain.class")
    private val cf_6: Classfile = setUpStream("HelloWorld.class")

    private def setUp(file: String): Classfile = {
        val url: URL = getLoader.getResource(file)
        val read: ClassfileReader = new ClassfileReader(Paths.get(url.toURI()).toString())
        new ClassfilePrinter(read.classfile)._print()
        read.classfile
    }

    private def setUpStream(file: String): Classfile = {
        val stream: InputStream = getLoader.getResourceAsStream(file)
        val read: ClassfileReader = new ClassfileReader(stream)
        new ClassfilePrinter(read.classfile)._print()
        read.classfile
    }

    private def getLoader: ClassLoader = getClass.getClassLoader

    @Test
    def classfileIOExceptionTest(): Unit = {
        new ClassfileReader("xxx.class")
        new ClassfileReader(new ByteArrayInputStream(Array[Byte]()))
    }

    @Test
    def headerTest(): Unit = {
//        sds.Main.main(Array("Hello.class"))
        // header
        assert(Integer.toHexString(cf_1.magic) === "cafebabe")
        assert(cf_1.major === 52)
        assert(cf_1.minor === 0)
        assert(get(cf_1.access, "class") === "public class ")
        // constant pool
        val pool: Array[ConstantInfo] = cf_1.pool
        assert(extract(cf_1.thisClass, pool) === "Hello")
        assert(extract(cf_1.superClass, pool) === "Object")
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
        assert(f._type === "field")
        assert(f.toString() === "private String field")
        val ra: RunAnn = f.attributes(0).asInstanceOf[RunAnn]
        assert(ra.annotations === Array("@sds.RuntimeAnnotation(value = \"field\")"))
        val ta: TypeAnnotation = f.attributes(1).asInstanceOf[RunTypeAnn].annotations(0)
        assert(ta.target.toString() === "EmptyTarget")
        assert(generate(ta, cf_2.pool) === "@sds.RuntimeAnnotation(value = \"field\")")
        assert(ta.path === Array())
    }

    @Test
    def methodTest(): Unit = {
        val methods: Array[MemberInfo] = cf_1.methods
        // method header
        val const: MemberInfo = methods(0)
        assert(const.access === "public ")
        assert(const.desc === "()void")
        assert(const.name === "<init>")
        assert(const.toString() === "public <init>()void")
        assert(const._type === "method")
        // code
        val code1: Code = const.attributes(0).asInstanceOf[Code]
        assert(code1.maxStack === 1)
        assert(code1.maxLocals === 1)
        // opcode on code
        val ops1: Array[OpcodeInfo] = code1.opcodes
        assert(ops1(0).toString() === "0 - aload_0")
        assert(ops1(1).toString() === "1 - invokespecial: #1(Object.<init>|()void)")
        assert(ops1(2).toString() === "4 - _return")
        // line_number_table on code
        val line1: LineNumberTable = code1.attributes(0).asInstanceOf[LineNumberTable]
        assert(line1.table === Array(Array(0, 0, 1)))
        assert(line1.getTableStr()(0) === "[range:0-0|line:1]")
        // main method
        val main: MemberInfo = methods(1)
        assert(main.access === "public static ")
        assert(main.desc === "(String[])void")
        assert(main.name === "main")
        assert(main.toString() === "public static main(String[])void")
        val code2: Code = main.attributes(0).asInstanceOf[Code]
        val ops2: Array[OpcodeInfo] = code2.opcodes
        val ldc: Has = ops2(1).asInstanceOf[Has]
        assert(ldc.toString() === "3 - ldc: #3(\"Hello World!!\"(String))")
        // exceptions on code
        val ex: Exceptions = main.attributes(1).asInstanceOf[Exceptions]
        assert(ex.ex === Array("Exception"))
        assert(ex.toString() === "Exceptions: [Exception]")


        val methods2: Array[MemberInfo] = cf_2.methods
        val local: LocalVariable
            = methods2(0).attributes(0).asInstanceOf[Code].attributes(1).asInstanceOf[LocalVariable]
        assert(local.getNameTable()(0)(0) === "this")
        assert(local.getNameTable()(0)(1) === "sds.AnnotatedTest")
        assert(local.getTable()(0)(0) === 0)
        assert(local.getTable()(0)(1) === 5)
        assert(local.getTable()(0)(2) === 0)

        val cons: MemberInfo = methods2(1)
        val consCode: Code = cons.attributes(0).asInstanceOf[Code]
        val indy: InvokeDynamic = consCode.opcodes(1).asInstanceOf[InvokeDynamic]
        assert(indy.toString() === "1 - invokedynamic: #2(accept|(sds.AnnotatedTest)java.util.function.Consumer)")
        val rta2: RunTypeAnn = consCode.attributes(3).asInstanceOf[RunTypeAnn]
        assert(rta2.name === "RuntimeVisibleTypeAnnotations")
        assert(rta2.annotations(0).target.toString() === "LocalVarTarget: [start, len, index] = {[7,1,2]}")
        assert(cons.attributes(2).toString() === "Deprecated")
        assert(cons.attributes(3).toString() === "Signature: <T extends Object>(int)void")
        val rta3: Array[TypeAnnotation] = cons.attributes(5).asInstanceOf[RunTypeAnn].annotations
        assert(rta3(0).target.toString() === "TypeParamTarget: (type_param: 0)")
        assert(rta3(1).target.toString() === "TypeParamBoundTarget: (bounds: 0, type_param: 0)")
        assert(rta3(2).target.toString() === "ThrowsTarget: (throws_type: 0)")
        assert(rta3(3).target.toString() === "MethodFormalParamTarget: (formal_index: 0)")
        val rpa: RunParamAnn = cons.attributes(6).asInstanceOf[RunParamAnn]
        assert(rpa.annotations(0)(0) === "@sds.RuntimeAnnotation(value = \"method_arg\")")


        val methods3: Array[MemberInfo] = cf_3.methods
        val ad1: AnnotationDefault = methods3(0).attributes(0).asInstanceOf[AnnotationDefault]
        val ad2: AnnotationDefault = methods3(1).attributes(0).asInstanceOf[AnnotationDefault]
        val ad3: AnnotationDefault = methods3(2).attributes(0).asInstanceOf[AnnotationDefault]
        val ad4: AnnotationDefault = methods3(3).attributes(0).asInstanceOf[AnnotationDefault]
        val ad5: AnnotationDefault = methods3(4).attributes(0).asInstanceOf[AnnotationDefault]
        val ad6: AnnotationDefault = methods3(5).attributes(0).asInstanceOf[AnnotationDefault]
        val ad7: AnnotationDefault = methods3(6).attributes(0).asInstanceOf[AnnotationDefault]
        val annotation: String = "@java.lang.annotation.Target(value = {java.lang.annotation.ElementType.TYPE})"
        assert(ad1.toString() === "AnnotationDefault: \"placement\"")
        assert(ad2.toString() === "AnnotationDefault: '97'")
        assert(ad3.toString() === "AnnotationDefault: for_test.RuntimeAnnotation$Type.Default")
        assert(ad4.toString() === "AnnotationDefault: " + annotation)
        assert(ad5.toString() === "AnnotationDefault: 100")
        assert(ad6.toString() === "AnnotationDefault: {1,2,3}")
        assert(ad7.toString() === "AnnotationDefault: int.class")


        val methods5: Array[MemberInfo] = cf_5.methods
        val code5: Code = methods5(2).attributes(0).asInstanceOf[Code]
        val switch: TableSwitch = code5.opcodes(4).asInstanceOf[TableSwitch]
        assert(Operand.get(switch, cf_5.pool) === "[36, 41, 46, 52]")

        val code5_1: Code = methods5(3).attributes(0).asInstanceOf[Code]
        val look: LookupSwitch = code5_1.opcodes(6).asInstanceOf[LookupSwitch]
        assert(Operand.get(look, cf_5.pool) === "[2223141:28, default:39]")


        val methods6: Array[MemberInfo] = cf_6.methods
        val code6: Code = methods6(8).attributes(0).asInstanceOf[Code]
        val multi: Multi = code6.opcodes(22).asInstanceOf[Multi]
        assert(Operand.get(multi, cf_6.pool) === "2,long[][]")
    }

    @Test
    def stackMapFrameTest(): Unit = {
        val code1: Code = cf_4.methods(0).attributes(0).asInstanceOf[Code]
        val stack1: Stack = code1.attributes(1).asInstanceOf[Stack]
        val entry_1 = stack1.entries.iterator
        // FullFrame
        val full = entry_1.next()
        assert(full._1 === (255, 28))
        assert(full._2("stack") === Buffer[String]())
        assert(full._2("local") === Buffer("Resize", "String[]", "int"))
        // ChopFrame
        val chop = entry_1.next()
        assert(chop._2("local") === Buffer("Resize", "String[]"))
        // SameFrame
        val same = entry_1.next()
        assert(same._2("local") === Buffer("Resize", "String[]"))
        // AppendFrame
        val append = entry_1.next()
        assert(append._2("local") === Buffer("Resize", "String[]", "int"))

        val code4: Code = cf_4.methods(3).attributes(0).asInstanceOf[Code]
        val stack4: Stack = code4.attributes(1).asInstanceOf[Stack]
        val entry_4 = stack4.entries.iterator
        entry_4.next()
        val sameLocals = entry_4.next()
        assert(sameLocals._2("stack") === Buffer("Runnable"))

        val code5: Code = cf_4.methods(4).attributes(0).asInstanceOf[Code]
        val stack5: Stack = code5.attributes(1).asInstanceOf[Stack]
        val entry_5 = stack5.entries.iterator
        val sameLocalsExtended = entry_5.next()
        assert(sameLocalsExtended._2("stack") === Buffer("java.io.IOException"))
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
        assert(inner.classes === innerArray)
        val bsm: Array[(String, Array[String])] = cf_2.attributes(3).asInstanceOf[BootstrapMethods].bsm
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