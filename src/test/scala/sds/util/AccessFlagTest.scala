package sds.util

import org.junit.Test
import org.scalatest.Assertions
import sds.util.{AccessFlag => Flag}

class AccessFlagTest extends Assertions {
	val PUBLIC:       Int = 0x0001
	val PRIVATE:      Int = 0x0002
	val PROTECTED:    Int = 0x0004
	val STATIC:       Int = 0x0008
	val FINAL:        Int = 0x0010
	val SUPER:        Int = 0x0020
	val SYNCHRONIZED: Int = 0x0020
	val VOLATILE:     Int = 0x0040
	val BRIDGE:       Int = 0x0040
	val TRANSIENT:    Int = 0x0080
	val VARARGS:      Int = 0x0080
	val NATIVE:       Int = 0x0100
	val INTERFACE:    Int = 0x0200
	val ABSTRACT:     Int = 0x0400
	val STRICT:       Int = 0x0800
	val SYNTHETIC:    Int = 0x1000
	val ANNOTATION:   Int = 0x2000
	val ENUM:         Int = 0x4000
	val MANDATED:     Int = 0x8000

	@Test
	def getTestException {
		intercept[IllegalArgumentException] {
			Flag.get(PUBLIC, "_class").equals("public class ")
		}
		intercept[IllegalArgumentException] {
			Flag.get(PUBLIC, "f_ield").equals("public ")
		}
		intercept[IllegalArgumentException] {
			Flag.get(PUBLIC, "method.").equals("public ")
		}
		intercept[IllegalArgumentException] {
			Flag.get(FINAL, "locals").equals("final ")
		}
	}

	@Test
	def getTestForClassAndNested {
		assert(Flag.get(PUBLIC, "class").equals("public class "))
		assert(Flag.get(FINAL, "class").equals("final class "))
		assert(Flag.get(ABSTRACT, "class").equals("abstract class "))
		assert(Flag.get(SYNTHETIC, "class").equals("synthetic class "))
		assert(Flag.get(ENUM, "class").equals("enum "))
		assert(Flag.get(ANNOTATION, "class").equals("@interface "))
		assert(Flag.get(INTERFACE, "class").equals("interface "))
		assert(Flag.get(STATIC, "nested").equals("static class "))

		val flag3: Int = PUBLIC | FINAL | ABSTRACT | SYNTHETIC | ENUM
		val flag4: Int = PUBLIC | FINAL | ABSTRACT | SYNTHETIC | ENUM | ANNOTATION
		val flag5: Int = PUBLIC | FINAL | ABSTRACT | SYNTHETIC | ENUM | INTERFACE
		assert(Flag.get(flag3, "class").equals("public final abstract synthetic enum "))
		assert(Flag.get(flag4, "class").equals("public final abstract synthetic @interface enum "))
		assert(Flag.get(flag5, "class").equals("public final abstract synthetic enum interface "))
		assert(Flag.get(0, "class").equals("class "))
		assert(Flag.get(0, "nested").equals("class "))
	}

	@Test
	def getTestForField {
		assert(Flag.get(PUBLIC, "field").equals("public "))
		assert(Flag.get(PRIVATE, "field").equals("private "))
		assert(Flag.get(PROTECTED, "field").equals("protected "))
		assert(Flag.get(STATIC, "field").equals("static "))
		assert(Flag.get(FINAL, "field").equals("final "))
		assert(Flag.get(VOLATILE, "field").equals("volatile "))
		assert(Flag.get(TRANSIENT, "field").equals("transient "))
		assert(Flag.get(SYNTHETIC, "field").equals("synthetic "))
		assert(Flag.get(ENUM, "field").equals("enum "))
		val FIELD:  Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL | VOLATILE | TRANSIENT | SYNTHETIC | ENUM
		val result: String = "public private protected static final volatile transient synthetic enum "
		assert(Flag.get(FIELD, "field").equals(result))
		assert(Flag.get(0, "field").equals(""))
	}

	@Test
	def getTestForMethod {
		assert(Flag.get(PUBLIC, "method").equals("public "))
		assert(Flag.get(PRIVATE, "method").equals("private "))
		assert(Flag.get(PROTECTED, "method").equals("protected "))
		assert(Flag.get(STATIC, "method").equals("static "))
		assert(Flag.get(FINAL, "method").equals("final "))
		assert(Flag.get(SYNCHRONIZED, "method").equals("synchronized "))
		assert(Flag.get(NATIVE, "method").equals("native "))
		assert(Flag.get(ABSTRACT, "method").equals("abstract "))
		assert(Flag.get(STRICT, "method").equals("strictfp "))
		assert(Flag.get(SYNTHETIC, "method").equals("synthetic "))
		val METHOD: Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL | SYNCHRONIZED | BRIDGE | VARARGS | 
	                      NATIVE | ABSTRACT | STRICT | SYNTHETIC
		val re: String = "public private protected static final synchronized native abstract strictfp synthetic "
		assert(Flag.get(METHOD, "method").equals(re))
		assert(Flag.get(0, "method").equals(""))
	}

	@Test
	def getTestForLocal {
		assert(Flag.get(FINAL, "local").equals("final "))
		assert(Flag.get(0, "local").equals(""))
	}
}