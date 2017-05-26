package sds.classfile.bytecode

import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.constant_pool._

class OpcodeInfoTest extends Assertions {
    val info: Array[ConstantInfo] = Array(
        new Utf8Info("utf8"), new NumberInfo(ConstantInfo.INTEGER, 0), new NumberInfo(ConstantInfo.FLOAT, 0.0f),
        new NumberInfo(ConstantInfo.LONG, 0L), new NumberInfo(ConstantInfo.DOUBLE, 0.0), new StringInfo(1),
        new HandleInfo(1, 1), new InvokeDynamicInfo(1, 1), new ConstantInfoAdapter(),
        new Utf8Info("java/lang/Object"), new ClassInfo(10), new MemberInfo(ConstantInfo.FIELD, 11, 1),
        new Utf8Info("([[Ljava/util/List;Ljava/lang/System;)V"), new NameAndTypeInfo(1, 13), new TypeInfo(13)
    )

    @Test
    def branchTest(): Unit = {
        val branch: BranchOpcode = new BranchOpcode(10, "_goto", 0)
        assert(branch.toString() === "0 - _goto: 10")
    }

    @Test
    def iincTest(): Unit = {
        val iinc: Iinc = new Iinc(0, 0, 0)
        assert(iinc.toString() === "0 - iinc: 0, 0")
    }

    @Test
    def indexTest(): Unit = {
        val index: IndexOpcode = new IndexOpcode(1, "iload", 0)
        assert(index.toString() === "0 - iload: 1")
    }

    @Test
    def arrayTest(): Unit = {
        val array: Array[NewArray] = (4 to 11).map(new NewArray(_, 0)).toArray
        assert(array(0).toString() === "0 - newarray: boolean")
        assert(array(1).toString() === "0 - newarray: char")
        assert(array(2).toString() === "0 - newarray: float")
        assert(array(3).toString() === "0 - newarray: double")
        assert(array(4).toString() === "0 - newarray: byte")
        assert(array(5).toString() === "0 - newarray: short")
        assert(array(6).toString() === "0 - newarray: int")
        assert(array(7).toString() === "0 - newarray: long")
        intercept[RuntimeException] {
            new NewArray(12, 0).toString === ""
        }
    }

    @Test
    def pushTest(): Unit = {
        val push: PushOpcode = new PushOpcode(0, "bipush", 0)
        assert(push.toString() === "0 - bipush: 0")
    }

    @Test
    def hasTest(): Unit = {
        val has1: HasReferenceOpcode = new HasReferenceOpcode(1, info, "_new", 0)
        assert(has1.toString() === "0 - _new: #1(utf8)")

        val int:    HasReferenceOpcode = new HasReferenceOpcode(2,  info, "ldc", 0)
        val float:  HasReferenceOpcode = new HasReferenceOpcode(3,  info, "ldc", 0)
        val long:   HasReferenceOpcode = new HasReferenceOpcode(4,  info, "ldc", 0)
        val double: HasReferenceOpcode = new HasReferenceOpcode(5,  info, "ldc", 0)
        val string: HasReferenceOpcode = new HasReferenceOpcode(6,  info, "ldc", 0)
        val _class: HasReferenceOpcode = new HasReferenceOpcode(11, info, "ldc", 0)
        val utf8:   HasReferenceOpcode = new HasReferenceOpcode(1,  info, "ldc", 0)
        assert(int.toString()    === "0 - ldc: #2(0(int))")
        assert(float.toString()  === "0 - ldc: #3(0.0(float))")
        assert(long.toString()   === "0 - ldc: #4(0(long))")
        assert(double.toString() === "0 - ldc: #5(0.0(double))")
        assert(string.toString() === "0 - ldc: #6(\"utf8\"(String))")
        assert(_class.toString() === "0 - ldc: #11(Object(Object))")
        assert(utf8.toString()   === "0 - ldc: #1(utf8)")
    }
}