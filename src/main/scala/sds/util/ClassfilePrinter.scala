package sds.util

import sds.Classfile
import sds.classfile.{MemberInfo => Member}
import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.ConstantInfo
import sds.util.AccessFlag.get
import sds.util.Utf8ValueExtractor.extract

class ClassfilePrinter(private val cf: Classfile) {
	private val pool: Array[ConstantInfo] = cf.pool
	private val sep: String = System.getProperty("line.separator")

	def _print(): Unit = {
		println("<<< Magic Number >>>")
		println("  " + Integer.toHexString(cf.magic));
		println("<<< Major Version >>>")
		println("  " + cf.major)
		println("<<< Minor Version >>>")
		println("  " + cf.minor)
		printPool()
		printClass()
		printField()
		printMethod()
	}

	def printPool(): Unit = {
		println("<<< Constant Pool >>>")
		(0 until pool.length).foreach((i) => println("  [" + (i + 1) + "]: " + pool(i).toString()))
	}

	def printClass(): Unit = {
		println("<<< Class >>>")
		val thisClass: String = extract(cf.thisClass, pool)
		val superClass: String = if(check(cf.superClass)) " extends " + extract(cf.superClass, pool) else " "
		val interface: String = if(cf.interfaces.length > 0) {
			" implements " + cf.interfaces.map(extract(_, pool)).reduce((x, y) => x + ", " + y)
		} else {
			""
		}
		println("  " + get(cf.access, "class") + thisClass + superClass + interface)
	}

	def printField(): Unit = {
		println("    <<< Field >>>")
		val fields: Array[Member] = cf.fields
		(0 until fields.length).foreach((i) => {
				println("      [" + (i + 1) + "]: " + fields(i).toString())
				printAttribute("          ", fields(i).getAttributes(), "field")
		})
	}

	def printMethod(): Unit = {
		println("    <<< Method >>>")
		val methods: Array[Member] = cf.methods
		(0 until methods.length).foreach((i) => {
				println("      [" + (i + 1) + "]: " + methods(i).toString())
				printAttribute("          ", methods(i).getAttributes(), "method")
		})
	}

	def printAttribute(indent: String, attr: Array[AttributeInfo], _type: String): Unit = {
		if(attr.length == 0) return
		println(indent + "<<< Attribute in " + _type + " >>>")
		(0 until attr.length).foreach((i) => println(indent + "  [" + (i + 1) + "]: " + attr(i).toString()))
	}

	def check(index: Int): Boolean = (0 until pool.length).contains(index)
}