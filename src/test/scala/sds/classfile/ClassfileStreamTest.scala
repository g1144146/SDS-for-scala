package sds.classfile

import java.io.InputStream

import org.junit.Test
import org.scalatest.Assertions

class ClassfileStreamTest extends Assertions {
    @Test
    def readTest(): Unit = {
        val stream: InputStream = getClass.getClassLoader.getResourceAsStream("HelloWorld.class")
        val data: ClassfileStream = ClassfileStream(stream)
        // double: 8, float: 4, long: 8,
        data.double
        assert(data.pointer === 8)
        data.long
        assert(data.pointer === 16)
        data.float
        assert(data.pointer === 20)
        data.skip(10)
        assert(data.pointer === 30)
    }
}