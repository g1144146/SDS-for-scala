package sds.constant_pool

import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.constant_pool._

class ConstantInfoTest extends Assertions {
    @Test
    def classTest(): Unit = {
        assert(new ClassInfo(1).toString() === "Class\t#1")
    }

    @Test
    def adapterTest(): Unit = {
        assert(new ConstantInfoAdapter().toString() === "null")
    }

    @Test
    def handleTest(): Unit = {
        val h: HandleInfo = new HandleInfo(1, 0)
        assert(h.kind === 1)
        assert(h.toString() === "MethodHandle\tREF_getField:#0")
        assert(new HandleInfo(2, 0).kindValue === "REF_getStatic")
        assert(new HandleInfo(3, 0).kindValue === "REF_putField")
        assert(new HandleInfo(4, 0).kindValue === "REF_putStatic")
        assert(new HandleInfo(5, 0).kindValue === "REF_invokeVirtual")
        assert(new HandleInfo(6, 0).kindValue === "REF_invokeStatic")
        assert(new HandleInfo(7, 0).kindValue === "REF_invokeSpecial")
        assert(new HandleInfo(8, 0).kindValue === "REF_newInvokeSpecial")
        assert(new HandleInfo(9, 0).kindValue === "REF_invokeInterface")
        intercept[IllegalStateException] {
            new HandleInfo(10, 0).kindValue === ""
        }
    }

    @Test
    def invokeDynamicTest(): Unit = {
        assert(new InvokeDynamicInfo(0, 0).toString() === "InvokeDynamic\t#0:#0")
    }

    @Test
    def fieldTest(): Unit = {
        assert(new MemberInfo(ConstantType.FIELD, 0, 0).toString() === "Fieldref\t#0.#0")
    }

    @Test
    def methodTest(): Unit = {
        assert(new MemberInfo(ConstantType.METHOD, 0, 0).toString() === "Methodref\t#0.#0")
    }

    @Test
    def interfaceTest(): Unit = {
        assert(new MemberInfo(ConstantType.INTERFACE, 0, 0).toString() === "InterfaceMethodref\t#0.#0")
    }

    @Test
    def nameAndTypeTest(): Unit = {
        assert(new NameAndTypeInfo(0, 0).toString() === "NameAndType\t#0:#0")
    }

    @Test
    def intTest(): Unit = {
        val int: IntInfo = new IntInfo(0)
        assert(int.int === 0)
        assert(int.toString() === "Int\t0")
    }

    @Test
    def floatTest(): Unit = {
        val float: FloatInfo = new FloatInfo(0.0f)
        assert(float.float === 0.0)
        assert(float.toString() === "Float\t0.0")
    }

    @Test
    def longTest(): Unit = {
        val long: LongInfo = new LongInfo(0L)
        assert(long.long === 0)
        assert(long.toString() === "Long\t0")
    }

    @Test
    def doubleTest(): Unit = {
        val double: DoubleInfo = new DoubleInfo(0.0)
        assert(double.double === 0.0)
        assert(double.toString() === "Double\t0.0")
    }

    @Test
    def stringTest(): Unit = {
        assert(new StringInfo(0).toString() === "String\t#0")
    }

    @Test
    def typeTest(): Unit = {
        assert(new TypeInfo(0).toString() === "MethodType\t#0")
    }

    @Test
    def utf8Test(): Unit = {
        assert(new Utf8Info("utf8").toString() === "Utf8\tutf8")
    }
}