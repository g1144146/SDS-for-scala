package sds.util

import collection.mutable.{
  HashMap       => Hash,
  LinkedHashMap => Linked,
  ArrayBuffer   => Buffer
}
import sds.Classfile
import sds.classfile.{MemberInfo => Member}
import sds.classfile.attribute.{
  AttributeInfo,
  BootstrapMethods,
  Code,
  InnerClasses,
  LineNumberTable,
  LocalVariable,
  StackMapTable
}
import sds.classfile.bytecode.{OpcodeInfo => Opcode}
import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.AccessFlag.get

class ClassfilePrinter(private val cf: Classfile) {
	private val pool: Array[ConstantInfo] = cf.pool

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
		printAttribute("    ", cf.attributes, "class")
	}

	private def printPool(): Unit = {
		println("<<< Constant Pool >>>")
		(0 until pool.length).foreach((i) => println("  [" + (i + 1) + "]: " + pool(i).toString()))
	}

	private def printClass(): Unit = {
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

	private def printField(): Unit = {
		println("    <<< Field >>>")
		val fields: Array[Member] = cf.fields
		(0 until fields.length).foreach((i) => {
				println("      [" + (i + 1) + "]: " + fields(i).toString())
				printAttribute("          ", fields(i).getAttributes(), "field")
		})
	}

	private def printMethod(): Unit = {
		println("    <<< Method >>>")
		val methods: Array[Member] = cf.methods
		(0 until methods.length).foreach((i) => {
			println("      [" + (i + 1) + "]: " + methods(i).toString())
			printAttribute("          ", methods(i).getAttributes(), "method")
		})
	}

	private def printAttribute(indent: String, attr: Array[AttributeInfo], _type: String): Unit = {
		if(attr.length == 0) return
		println(indent + "<<< Attribute in " + _type + " >>>")
		(0 until attr.length).foreach((i) => {
			attr(i) match {
				case boot: BootstrapMethods =>
					println(indent + "  [" + (i + 1) + " in " + _type + "]: BootstrapMethods")
					val bsm: Array[(String, Array[String])] = boot.getBSM()
					(0 until bsm.length).foreach((i: Int) => {
						val t: (String, Array[String]) = bsm(i)
						println(indent + "    (" + i + "): ")
						println(indent + "        bsm_ref : " + t._1)
						println(indent + "        bsm_args: " + t._2.mkString(", "))
					})
				case code: Code =>
					println(indent + "  max_stack: " + code.getMaxStack() + ", max_locals: " + code.getMaxLocals())
					val opcodes: Array[Opcode] = code.getOpcodes()
					(0 until opcodes.length).foreach((i: Int) => println(indent + "    " + opcodes(i)))
					val table: Array[(Array[Int], String)] = code.getExTable()
					if(table.length > 0) {
						println(indent + "  Exception Table:")
						(0 until table.length).foreach((i: Int) => {
							val t: Array[Int] = table(i)._1
							val target:  String = table(i)._2
							println(indent + "    [" + i + "]: " + t(0) + "-" + t(1) + ", " + t(2) + ": " + target)
						})
					}
					printAttribute(indent + "  ", code.getAttributes(), "Code")
				case ic: InnerClasses =>
					println(indent + "  [" + (i + 1) + " in " + _type + "]: InnerClasses")
					val inner: Array[Array[String]] = ic.getClasses()
					(0 until inner.length).foreach((i: Int) => {
						val first:  String = indent + "    (" + (i + 1) + "): "
						val second: String = inner(i)(3) + inner(i)(0) + " " + inner(i)(2)
						val third:  String = if(inner(i)(1).length > 0)" {in " + inner(i)(1) + "}" else ""
						println(first + second + third)
					})
				case line: LineNumberTable =>
					println(indent + "  [" + (i + 1) + " in " + _type + "]: LineNumberTable")
					val lines: Array[String] = line.getTableStr()
					(0 until lines.length).foreach((i: Int) => println(indent + "    [" + i + "]: " + lines(i)))
				case local: LocalVariable =>
					println(indent + "  [" + (i + 1) + " in " + _type + "]: LocalVariable")
					val name:  Array[Array[String]] = local.getNameTable()
					val table: Array[Array[Int]]    = local.getTable()
					(0 until name.length).foreach((i: Int) => {
						val first: String  = indent + "    [" + i + "]: "
						val second: String = name(i)(1) + " " + name(i)(0)
						val third: String  = " {" + table(i)(0) + "-" + table(i)(1) + "}"
						println(first + second + third)
					})
				case stack: StackMapTable =>
					println(indent + "  [" + (i + 1) + " in " + _type + "]: StackMapTable")
					val entries: Linked[Int, Hash[String, Buffer[String]]] = stack.getEntries()
					entries.foreach((entry: (Int, Hash[String, Buffer[String]])) => {
						println(indent + "    " + getFrame(entry._1) + " - " + entry._1)
						entry._2.foreach((e: (String, Buffer[String])) => {
							println(indent + "      " + e._1 + " - " + e._2.mkString("[", ", ", "]"))
						})
					})
				case _ =>
					val before: String = indent + "  [" + (i + 1) + " in " + _type + "]: "
					println(before + attr(i).toString())
			}
		})
	}

	private def getFrame(tag: Int): String = {
		if((0 to 63).contains(tag))    return "SameFrame"
		if((64 to 127).contains(tag))  return "SameLocals1StackItemFrame"
		if(tag == 247)                 return "SameLocals1StackItemFrameExtended"
		if((248 to 250).contains(tag)) return "ChopFrame"
		if(tag == 251)                 return "SameFrameExtended"
		if((252 to 254).contains(tag)) return "AppendFrame"
		if(tag == 255)                 return "FullFrame"
		throw new RuntimeException("unknown tag(" + tag + ")")
	}

	private def check(index: Int): Boolean = (0 until pool.length).contains(index)
}