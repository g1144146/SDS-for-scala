package sds;

import java.io.{IOException, File};
import java.util.jar.JarFile;
import scala.collection.mutable.ArrayBuffer;
import sds.util.{ClassfilePrinter => Printer}

class SDS(args: Array[String]) {
    var jar: JarFile = null;
    val classfiles: ArrayBuffer[String] = ArrayBuffer();
    args.foreach(parseArgs)

    private def parseArgs(arg: String): Unit = {
        if(arg.endsWith(".class")) {
            classfiles += arg
        } else if(arg.endsWith(".jar")) {
            try {
                this.jar = new JarFile(new File(arg))
            } catch {
                case e: IOException => e.printStackTrace()
            }
        } else {
            println(arg + " is not classfile or jar.")
        }
    }

    private def analyzeClassfile(): Unit = {
        classfiles.foreach((file: String) => {
            val reader: ClassfileReader = new ClassfileReader(file)
            reader.read()
            val classfile: Classfile = reader.classfile
            val p: Printer = new Printer(classfile)
            p._print
        })
    }

    def run(): Unit = {
        if(classfiles.nonEmpty) analyzeClassfile()
        if(jar != null) println("")
    }
}