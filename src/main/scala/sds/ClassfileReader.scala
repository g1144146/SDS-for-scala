package sds

import java.io.{IOException, InputStream, RandomAccessFile};
import sds.classfile.{ConstantPool => Pool, ClassfileStream};
import sds.classfile.constant_pool.{ConstantInfo, ConstantInfoFactory => CFactory}

class ClassfileReader {
	var stream: ClassfileStream = null
	val classfile: Classfile = new Classfile()

	def this(stream: InputStream) {
		this()
		try {
			this.stream = new ClassfileStream(stream)
		} catch {
			case e: IOException => e.printStackTrace()
		}
	}

	def this(fileName: String) {
		this()
		try {
			val raf: RandomAccessFile = new RandomAccessFile(fileName, "r")
			this.stream = new ClassfileStream(raf)
		} catch {
			case e: IOException => e.printStackTrace()
		}
	}

	def read(): Unit = {
		classfile.magic = stream.readInt()
		classfile.minor = stream.readShort()
		classfile.major = stream.readShort()
		classfile.pool = readConstantPool(0, new CFactory, new Pool(stream.readShort() - 1))
		classfile.access = stream.readShort()
		classfile.thisClass = stream.readShort()
		classfile.superClass = stream.readShort()
	}

	private def readConstantPool(i: Int, factory: CFactory, pool: Pool): Pool = {
		if(i >= pool.length) {
			return pool
		}
		val info: ConstantInfo = factory.create(stream.readByte())
		info.read(stream, pool)
		pool :+ info
//		if(info.getTag() == 0) {
//			
//		}
		readConstantPool(i + 1, factory, pool)
	}
}