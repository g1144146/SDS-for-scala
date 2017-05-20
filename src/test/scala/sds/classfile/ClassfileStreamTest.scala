package sds.classfile

import org.junit.Test
import org.scalatest.Assertions

class ClassfileStreamTest extends Assertions {
    @Test
    def exceptionTest(): Unit = {
        val data: ClassfileStream = new ClassfileStream()
        intercept[UnsupportedOperationException] { data.char   }
        intercept[UnsupportedOperationException] { data.byte   }
        intercept[UnsupportedOperationException] { data.int    }
        intercept[UnsupportedOperationException] { data.float  }
        intercept[UnsupportedOperationException] { data.long   }
        intercept[UnsupportedOperationException] { data.double }
        intercept[UnsupportedOperationException] { data.short  }
        intercept[UnsupportedOperationException] { data.pointer }
        intercept[UnsupportedOperationException] { data.close() }
        intercept[UnsupportedOperationException] { data.skip(0) }
        intercept[UnsupportedOperationException] { data.unsignedByte }
        intercept[UnsupportedOperationException] { data.fully(new Array(0)) }
    }
}