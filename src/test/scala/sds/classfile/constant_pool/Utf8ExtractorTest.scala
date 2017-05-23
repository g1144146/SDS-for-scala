package sds.classfile.constant_pool

import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.constant_pool.Utf8ValueExtractor.extract

class Utf8ExtractorTest extends Assertions {
    val info: Array[ConstantInfo] = Array(
        new Utf8Info("utf8"), new NumberInfo(ConstantInfo.INTEGER, 0), new NumberInfo(ConstantInfo.FLOAT, 0.0f),
        new NumberInfo(ConstantInfo.LONG, 0L), new NumberInfo(ConstantInfo.DOUBLE, 0.0), new StringInfo(1),
        new HandleInfo(1, 1), new InvokeDynamicInfo(1, 1), new ConstantInfoAdapter(),
        new Utf8Info("java/lang/Object"), new ClassInfo(10), new MemberInfo(ConstantInfo.FIELD, 11, 1),
        new Utf8Info("([[Ljava/util/List;Ljava/lang/System;)V"), new NameAndTypeInfo(1, 13), new TypeInfo(13)
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
        assert(extract(info(10), info) === "Object")
        assert(extract(info(11), info) === "Object.utf8")
        assert(extract(info(13), info) === "utf8|(java.util.List[][],System)void")
        assert(extract(info(14), info) === "(java.util.List[][],System)void")
    }
}
