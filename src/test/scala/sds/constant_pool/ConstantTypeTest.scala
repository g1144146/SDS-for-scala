package sds.constant_pool

import org.junit.Test
import org.scalatest.Assertions
import sds.classfile.constant_pool.{ConstantType => C}

class ConstantTypeTest extends Assertions {
    @Test
    def getTest(): Unit = {
        assert(C.get(C.CLASS) === "Class")
        assert(C.get(C.DOUBLE) === "Double")
        assert(C.get(C.FIELD) === "Fieldref")
        assert(C.get(C.FLOAT) === "Float")
        assert(C.get(C.HANDLE) === "MethodHandle")
        assert(C.get(C.INTEGER) === "Int")
        assert(C.get(C.INTERFACE) === "InterfaceMethodref")
        assert(C.get(C.INVOKE_DYNAMIC) === "InvokeDynamic")
        assert(C.get(C.LONG) === "Long")
        assert(C.get(C.METHOD) === "Methodref")
        assert(C.get(C.NAME_AND_TYPE) === "NameAndType")
        assert(C.get(C.STRING) === "String")
        assert(C.get(C.TYPE) === "MethodType")
        assert(C.get(C.UTF8) === "Utf8")
        assert(C.get(10000) === "Unknown")
    }
}
