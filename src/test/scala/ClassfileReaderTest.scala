package sds

import sds.classfile.MemberInfo
import sds.classfile.attribute.AttributeInfo
import sds.classfile.attribute.Code
import sds.classfile.attribute.LineNumberTable
import sds.classfile.attribute.Exceptions
import sds.classfile.attribute.SourceFile
import sds.classfile.bytecode.OpcodeInfo
import sds.classfile.bytecode.{HasReferenceOpcode => Has}
import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.ConstantType._
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.AccessFlag.get
import org.junit.Test
import org.junit.Before
import org.scalatest.Assertions

class ClassfileReaderTest extends Assertions {
    var cf_1: Classfile = null

    @Before
    def setUp(): Unit = {
        val path: String = genPath("build", "resources", "test", "resources", "Hello.class")
        val read: ClassfileReader = new ClassfileReader(path)
        read.read()
        this.cf_1 = read.classfile
//        new sds.util.ClassfilePrinter(cf_1)._print()
    }

    def genPath(paths: String*): String = {
        val join: java.util.StringJoiner = new java.util.StringJoiner(java.io.File.separator)
        paths.foreach(join.add)
        join.toString
    }

    @Test
    def headerTest(): Unit = {
        assert(Integer.toHexString(cf_1.magic) === "cafebabe")
        assert(cf_1.major === 52)
        assert(cf_1.minor === 0)
        assert(get(cf_1.access, "class") === "public class ")

        val pool: Array[ConstantInfo] = cf_1.pool
        assert(extract(cf_1.thisClass, pool) === "Hello")
        assert(extract(cf_1.superClass, pool) === "Object")
        assert(pool(0).tag === METHOD)
        assert(pool(0).toString() === "Method_ref\t#6.#17")
        assert(pool(2).toString() === "String\t#20")
        assert(pool(4).toString() === "Class\t#23")
        assert(pool(6).toString() === "Utf8\t<init>")
        assert(pool(16).toString() === "NameAndType\t#7:#8")
    }

    @Test
    def methodTest(): Unit = {
        val methods: Array[MemberInfo] = cf_1.methods
        val const: MemberInfo = methods(0)
        assert(const.getAccess() === "public ")
        assert(const.getDesc() === "()void")
        assert(const.getName() === "<init>")
        assert(const.toString() === "public <init>()void")
        assert(const.getType() === "method")
        val code1: Code = const.getAttributes()(0).asInstanceOf[Code]
        assert(code1.getMaxStack() === 1)
        assert(code1.getMaxLocals() === 1)
        val ops1: Array[OpcodeInfo] = code1.getOpcodes()
        assert(ops1(0).toString() === "0 - aload_0")
        assert(ops1(1).toString() === "1 - invokespecial: #1(Object.<init>|()void)")
        assert(ops1(2).toString() === "4 - _return")
        val line1: LineNumberTable = code1.getAttributes()(0).asInstanceOf[LineNumberTable]
        assert(line1.getTable() === Array(Array(0, 0, 1)))
        assert(line1.getTableStr()(0) === "[range:0-0|line:1]")

        val main: MemberInfo = methods(1)
        assert(main.getAccess() === "public static ")
        assert(main.getDesc() === "(String[])void")
        assert(main.getName() === "main")
        assert(main.toString() === "public static main(String[])void")
        val code2: Code = main.getAttributes()(0).asInstanceOf[Code]
        val ops2: Array[OpcodeInfo] = code2.getOpcodes()
        val ldc: Has = ops2(1).asInstanceOf[Has]
        assert(ldc.toString() === "3 - ldc: #3(\"20\"(String))")
        val ex: Exceptions = main.getAttributes()(1).asInstanceOf[Exceptions]
        assert(ex.getEx() === Array("Exception"))
        assert(ex.toString() === "Exceptions: [Exception]")
    }

    @Test
    def attributeTest(): Unit = {
        val source: SourceFile = cf_1.attributes(0).asInstanceOf[SourceFile]
        assert(source.toString() === "SourceFile: Hello.java")
    }
}