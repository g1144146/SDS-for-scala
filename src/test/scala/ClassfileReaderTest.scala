package sds

import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.ConstantType._
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
        val pool: Array[ConstantInfo] = cf_1.pool
        assert(pool(0).tag === METHOD)
        assert(pool(0).toString() === "Method_ref\t#6.#17")
        assert(pool(2).toString() === "String\t#20")
        assert(pool(4).toString() === "Class\t#23")
        assert(pool(6).toString() === "Utf8\t<init>")
        assert(pool(16).toString() === "NameAndType\t#7:#8")
    }
}