package sds

import org.junit.Test
import org.scalatest.Assertions

class SDSTest extends Assertions {
    @Test
    def jarFileIOExceptionTest(): Unit = {
        new SDS(Array("xxx.jar"))
    }

//    @Test
//    def classfileRunTest(): Unit = {
//        new SDS(Array("Hello.class")).run()
//    }
}