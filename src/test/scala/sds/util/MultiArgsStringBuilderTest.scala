package sds.util

import org.junit.Test
import org.scalatest.Assertions
import sds.util.{MultiArgsStringBuilder => Builder}

class MultiArgsStringBuilderTest extends Assertions {
	@Test
	def toStringTest() {
		val b: Builder = new Builder()
		assert(b.toString.equals(""))
		b.append("test")
		assert(b.toString.equals("test"))
		b.append(1)
		assert(b.toString.equals("test1"))
		b.append(1.0)
		assert(b.toString.equals("test11.0"))
		b.append(", ", 1.2, 0.10)
		assert(b.toString.equals("test11.0, 1.20.1"))
	}

	@Test
	def toStringTest2() {
		val b: Builder = new Builder("init")
		assert(b.toString.equals("init"))
	}
}