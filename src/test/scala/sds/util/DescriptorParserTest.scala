package sds.util

import org.junit.Test
import org.scalatest.Assertions
import sds.util.{DescriptorParser => DP}

class DescriptorParserTest extends Assertions {
	@Test
	def parseTest() {
		assert(DP.parse("B") === "byte")
		assert(DP.parse("C") === "char")
		assert(DP.parse("D") === "double")
		assert(DP.parse("F") === "float")
		assert(DP.parse("I") === "int")
		assert(DP.parse("J") === "long")
		assert(DP.parse("S") === "short")
		assert(DP.parse("Z") === "boolean")
		assert(DP.parse("V") === "void")
		assert(DP.parse("Ljava/lang/String;") === "String")
		assert(DP.parse("Lorg/scalatest/Assertions;") === "org.scalatest.Assertions")
		assert(DP.parse("(IIIR)B") === "(int,int,int)byte")
		assert(DP.parse("(IILjava/io/File;IR)Ljava/net/URL") === "(int,int,java.io.File,int)java.net.URL")
		assert(DP.parse("Ljava/util/List<TK;>;") === "java.util.List<K>")
		assert(DP.parse("Ljava/lang/Class<*>") === "Class< ? >")

		val case1: String = DP.parse("<K:Ljava/lang/Object;>Ljava/lang/Object;Ljava/lang/Runnable;", true)
		val case2: String = DP.parse("""<U:Ljava/lang/Object;T::Ljava/lang/Runnable;A:Ljava/lang/String;
		                             R::Ljava/lang/Runnable;:Ljava/lang/Appendable;S:Ljava/lang/Thread;>()V""", true)
		val case3: String = DP.parse("<+Ljava/lang/Object;>Ljava/util/List;")
		assert(case1 === "<K extends Object>Object,Runnable")
		assert(case2 === "<U extends Object,T extends Runnable," + 
							"A extends String,R extends Runnable & Appendable,S extends Thread>()void")
		assert(case3 === "<? extends Object>java.util.List")
	}

	@Test
	def removeLangPrefixTest() {
		assert(DP.removeLangPrefix("java.lang.String") === "String")
		assert(DP.removeLangPrefix("java.lang.Math.sqrt") === "Math.sqrt")

		val case1: String = "java.lang.annotation.Annotation"
		val case2: String = "java.lang.instrument.Instrumentation"
		val case3: String = "java.lang.invoke.MethodHandle"
		val case4: String = "java.lang.management.MemoryUsage"
		val case5: String = "java.lang.ref.Reference"
		val case6: String = "java.lang.reflect.Method"
		assert(DP.removeLangPrefix(case1) === case1)
		assert(DP.removeLangPrefix(case2) === case2)
		assert(DP.removeLangPrefix(case3) === case3)
		assert(DP.removeLangPrefix(case4) === case4)
		assert(DP.removeLangPrefix(case5) === case5)
		assert(DP.removeLangPrefix(case6) === case6)
	}
}