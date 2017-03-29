package sds;

import java.util.jar.JarFile;
import java.io.{IOException, File};
import scala.collection.mutable.ArraySeq;

class SDS(args: Array[String]) {
	args.foreach((_: String) => parseArgs(_))

	var jar: JarFile = null;
	var classfiles: ArraySeq[String] = null;

	private def parseArgs(arg: String): Unit = {
		if(arg.endsWith(".class")) {
			classfiles +: arg
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
		})
	}

	def run(): Unit = {
		if(classfiles.length > 0) analyzeClassfile()
		if(jar != null) println("")
	}
}