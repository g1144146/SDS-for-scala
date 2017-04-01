package sds;

import java.io.{IOException, File};
import java.util.jar.JarFile;
import scala.collection.mutable.ArrayBuffer;

class SDS(args: Array[String]) {
	var jar: JarFile = null;
	val classfiles: ArrayBuffer[String] = ArrayBuffer();
	args.foreach((arg: String) => parseArgs(arg))

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
		val out: Boolean = true
		classfiles.foreach((file: String) => {
			val reader: ClassfileReader = new ClassfileReader(file)
			reader.read()
			val classfile: Classfile = reader.classfile
			(0 until classfile.pool.length).foreach((i: Int) => println("[" + (i+1) + "]: " + classfile.pool(i)))
		})
	}

	def run(): Unit = {
		if(classfiles.length > 0) analyzeClassfile()
		if(jar != null) println("")
	}
}