package sds.constant_pool

import org.junit.Before
import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.constant_pool._
import sds.classfile.constant_pool.Utf8ValueExtractor.extract

class Utf8ExtractorTest extends Assertions {
    val info: Array[ConstantInfo] = Array(
        new Utf8Info("utf8"), new IntInfo(0),              new FloatInfo(0.0f),
        new LongInfo(0L),     new DoubleInfo(0.0),         new StringInfo(1),
        new HandleInfo(1, 1), new InvokeDynamicInfo(1, 1), new ConstantInfoAdapter()
    )

    @Test
    def extractTest(): Unit = {
        assert(extract(info(0), info) === "utf8")
        assert(extract(info(1), info) === "0")
        assert(extract(info(2), info) === "0.0")
        assert(extract(info(3), info) === "0")
        assert(extract(info(4), info) === "0.0")
        assert(extract(info(5), info) === "utf8")
        assert(extract(info(6), info) === "utf8")
        assert(extract(info(7), info) === "utf8")
        intercept[IllegalArgumentException] {
            extract(info(8), info) === ""
        }
    }
}
