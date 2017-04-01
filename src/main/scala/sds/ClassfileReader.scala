package sds

import java.io.{IOException, InputStream, RandomAccessFile};
import sds.classfile.ClassfileStream;
import sds.classfile.constant_pool.{ConstantInfo => CInfo, ConstantInfoAdapter, ConstantInfoFactory => CFactory}
import sds.classfile.constant_pool.ConstantType.{LONG, DOUBLE}

class ClassfileReader {
	private var stream: ClassfileStream = null
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
		classfile.pool = readConstantPool(0, new CFactory(), new Array[CInfo](stream.readShort() - 1))
		classfile.access = stream.readShort()
		classfile.thisClass = stream.readShort()
		classfile.superClass = stream.readShort()
	}

	private def readConstantPool(i: Int, factory: CFactory, pool: Array[CInfo]): Array[CInfo] = {
		if(i >= pool.length) {
			return pool
		}
		val info: CInfo = factory.create(stream.readByte())
		info.read(stream)
		pool(i) = info
		if(info.getTag() == LONG || info.getTag() == DOUBLE) {
			pool(i + 1) = new ConstantInfoAdapter()
			readConstantPool(i + 2, factory, pool)
		} else {
			readConstantPool(i + 1, factory, pool)
		}
	}
}