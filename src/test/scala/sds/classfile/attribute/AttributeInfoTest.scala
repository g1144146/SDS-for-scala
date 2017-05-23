package sds.classfile.attribute

import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.constant_pool._

class AttributeInfoTest extends Assertions {
    val info: Array[ConstantInfo] = Array(
        new Utf8Info("utf8"), new NumberInfo(ConstantInfo.INTEGER, 0), new NumberInfo(ConstantInfo.FLOAT, 0.0f),
        new NumberInfo(ConstantInfo.LONG, 0L), new NumberInfo(ConstantInfo.DOUBLE, 0.0), new StringInfo(1),
        new HandleInfo(1, 1), new InvokeDynamicInfo(1, 1), new ConstantInfoAdapter(),
        new Utf8Info("java/lang/Object"), new ClassInfo(10), new MemberInfo(ConstantInfo.FIELD, 11, 1),
        new Utf8Info("([[Ljava/util/List;Ljava/lang/System;)V"), new NameAndTypeInfo(1, 13), new TypeInfo(13)
    )

    @Test
    def constantValueTest(): Unit = {
        val const: ConstantValue = new ConstantValue("Hoge")
        assert(const.toString() === "ConstantValue: Hoge")
    }

    @Test
    def enclosingMethodTest(): Unit = {
        val enc: EnclosingMethod = new EnclosingMethod(11, 13, info)
        val enc2: EnclosingMethod = new EnclosingMethod(11, 0, info)
        assert(enc.toString() === "EnclosingMethod: Object ([[Ljava/util/List;Ljava/lang/System;)V")
        assert(enc2.toString() === "EnclosingMethod: Object ")
    }

    @Test
    def signatureTest(): Unit = {
        val sig: Signature = new Signature("hoge")
        assert(sig.toString() === "Signature: hoge")
    }

    @Test
    def sourceFileTest(): Unit = {
        val source: SourceFile = new SourceFile("Hoge.java")
        assert(source.toString() === "SourceFile: Hoge.java")
    }

    @Test
    def deprecatedTest(): Unit = {
        val dep: Deprecated = new Deprecated();
        assert(dep.toString() === "Deprecated")
    }

    @Test
    def syntheticTest(): Unit = {
        val syn: Synthetic = new Synthetic();
        assert(syn.toString() === "Synthetic")
    }
}