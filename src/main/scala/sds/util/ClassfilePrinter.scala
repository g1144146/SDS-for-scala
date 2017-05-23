package sds.util

import collection.mutable.{
  ArrayBuffer   => Buffer,
  HashMap       => Hash
}
import sds.Classfile
import sds.classfile.{MemberInfo => Member}
import sds.classfile.attribute.{
  AttributeInfo, BootstrapMethods, Code, InnerClasses,
  LineNumberTable, LocalVariable, RuntimeAnnotations,
  RuntimeParameterAnnotations, RuntimeTypeAnnotations,
  TypeAnnotation, StackMapTable
}
import sds.classfile.attribute.AnnotationGenerator.generate
import sds.classfile.bytecode.{
  OpcodeInfo => Opcode,
  SwitchOpcode,
  Operand
}
import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.Utf8ValueExtractor.extract
import sds.util.AccessFlag.get

class ClassfilePrinter(cf: Classfile) {
    private val pool: Array[ConstantInfo] = cf.pool

    def _print(): Unit = {
        println("<<< Magic Number >>>")
        println(s"  ${Integer.toHexString(cf.magic)}")
        println("<<< Version >>>")
        println(s"  ${cf.major}.${cf.minor}")

        printPool()
        printClass()
        printField()
        printMethod()
        printAttribute("    ", cf.attributes, "class")
    }

    private def printPool(): Unit = {
        println("<<< Constant Pool >>>")
        pool.indices.foreach((i) => println(s"  [${i + 1}]: ${pool(i).toString()}"))
    }

    private def printClass(): Unit = {
        println("<<< Class >>>")
        val thisClass: String  = extract(cf.thisClass, pool)
        val superClass: String = if(check(cf.superClass)) s" extends ${extract(cf.superClass, pool)}" else " "
        val interface: String  = if(cf.interfaces.length > 0) {
            s" implements ${cf.interfaces.map(extract(_, pool)).mkString(", ")}"
        } else {
            ""
        }
        println(s"  ${get(cf.access, "class")}$thisClass$superClass$interface")
    }

    private def printField(): Unit = {
        println("    <<< Field >>>")
        val fields: Array[Member] = cf.fields
        fields.indices.foreach((i) => {
            println(s"      [${i + 1}]: ${fields(i).toString()}")
            printAttribute("          ", fields(i).attributes, "field")
        })
    }

    private def printMethod(): Unit = {
        println("    <<< Method >>>")
        val methods: Array[Member] = cf.methods
        methods.indices.foreach((i) => {
            println(s"      [${i + 1}]: ${methods(i).toString()}")
            printAttribute("          ", methods(i).attributes, "method")
        })
    }

    private def printAttribute(indent: String, attr: Array[AttributeInfo], _type: String): Unit = {
        if(attr.length == 0) return
        println(s"$indent<<< Attribute in ${_type} >>>")
        attr.indices.foreach((i) => {
            attr(i) match {
                case boot: BootstrapMethods =>
                    println(s"$indent  [${i + 1} in ${_type}]: BootstrapMethods")
                    val bsm: Array[(String, Array[String])] = boot.bsm
                    bsm.indices.foreach((i: Int) => {
                        val t: (String, Array[String]) = bsm(i)
                        println(s"$indent    ($i): ")
                        println(s"$indent        bsm_ref : ${t._1}")
                        println(s"$indent        bsm_args: ${t._2.mkString(", ")}")
                    })
                case code: Code =>
                    println(s"$indent  [${i + 1} in ${_type}]: Code")
                    println(s"$indent  max_stack: ${code.maxStack}, max_locals: ${code.maxLocals}")
                    val opcodes: Array[Opcode] = code.opcodes
                    opcodes.indices.foreach((i: Int) => println(s"$indent    ${opcodes(i)}"))
                    val table: Array[(Array[Int], String)] = code.exTable
                    if(table.length > 0) {
                        println(s"$indent  Exception Table:")
                        table.indices.foreach((i: Int) => {
                            val t: Array[Int]   = table(i)._1
                            val target:  String = table(i)._2
                            println(s"$indent    [$i]: ${t(0)}-${t(1)}, ${t(2)}: $target")
                        })
                    }
                    printAttribute(s"$indent    ", code.attributes, "Code")
                case ic: InnerClasses =>
                    println(s"$indent  [${i + 1} in ${_type}]: InnerClasses")
                    val inner: Array[Array[String]] = ic.classes
                    inner.indices.foreach((i: Int) => {
                        val first:  String = s"$indent    (${i + 1}): "
                        val second: String = s"${inner(i)(3)}${inner(i)(0)} ${inner(i)(2)}"
                        val third:  String = if(inner(i)(1).length > 0) s" {in ${inner(i)(1)}}" else ""
                        println(s"$first$second$third")
                    })
                case line: LineNumberTable =>
                    println(s"$indent  [${i + 1} in ${_type}]: LineNumberTable")
                    val lines: Array[String] = line.getTableStr()
                    lines.indices.foreach((i: Int) => println(s"$indent    [$i]: ${lines(i)}"))
                case local: LocalVariable =>
                    println(s"$indent  [${i + 1} in ${_type}]: LocalVariable")
                    val name:  Array[Array[String]] = local.getNameTable()
                    val table: Array[Array[Int]]    = local.getTable()
                    name.indices.foreach((i: Int) => {
                        val first: String  = s"$indent    [$i]: "
                        val second: String = s"${name(i)(1)} ${name(i)(0)}"
                        val third: String  = s" {${table(i)(0)}-${table(i)(1)}"
                        println(s"$first$second$third")
                    })
                case ra: RuntimeAnnotations =>
                    println(s"$indent  [${i + 1} in ${_type}]: RuntimeAnnotation")
                    ra.annotations.foreach((a: String) => println(s"$indent    $a"))
                case rpa: RuntimeParameterAnnotations =>
                    println(s"$indent  [${i + 1} in ${_type}]: RuntimeParameterAnnotation")
                    rpa.annotations.foreach((a: Array[String]) => println(s"$indent    ${a.mkString(", ")}"))
                case rta: RuntimeTypeAnnotations =>
                    println(s"$indent  [${i + 1} in ${_type}]: RuntimeTypeAnnotation")
                    rta.annotations.foreach((_type: TypeAnnotation) => {
                        print(s"$indent    ${generate(_type, pool)} (${_type.target})")
                        println(s", (${_type.path.map(_.toString()).mkString("_")})")
                    })
                case stack: StackMapTable =>
                    println(s"$indent  [${i + 1} in ${_type}]: StackMapTable")
                    stack.entries.foreach((entry: ((Int, Int), Hash[String, Buffer[String]])) => {
                        val key: (Int, Int) = entry._1
                        println(s"$indent    ${getFrame(key._1)} - tag:${key._1}, offset:${key._2}")
                        println(s"$indent      stack  - ${entry._2("stack").mkString("[", ", ", "]")}")
                        println(s"$indent      locals - ${entry._2("local").mkString("[", ", ", "]")}")
                    })
                case _ => println(s"$indent  [${i + 1} in ${_type}]: ${attr(i).toString()}")
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
        throw new RuntimeException(s"unknown tag($tag)")
    }

    private def check(index: Int): Boolean = pool.indices.contains(index)
}