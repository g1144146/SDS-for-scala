package sds

import java.io.{IOException, InputStream, RandomAccessFile};
import sds.classfile.{ClassfileStream, MemberInfo};
import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.{ConstantInfo => CInfo, Utf8Info => Utf8, ConstantInfoAdapter => Adapter}
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
			this.stream = new ClassfileStream(new RandomAccessFile(fileName, "r"))
		} catch {
			case e: IOException => e.printStackTrace()
		}
	}

	def read(): Unit = {
		classfile.magic = stream.readInt()
		classfile.minor = stream.readShort()
		classfile.major = stream.readShort()
		classfile.pool = readConstantPool(0, new Array[CInfo](stream.readShort() - 1))
		classfile.access = stream.readShort()
		classfile.thisClass = stream.readShort()
		classfile.superClass = stream.readShort()
		classfile.interfaces = range(stream.readShort()).map((_: Int) => stream.readShort()).toArray

		val genAttr: ((Int) => (AttributeInfo)) = (_: Int) => {
			val name: Int = stream.readShort()
			val utf8: Utf8 = classfile.pool(name - 1).asInstanceOf[Utf8]
			val info: AttributeInfo = AttributeInfo(utf8.getValue(), stream.readInt())
			info.read(stream, classfile.pool)
			info
		}
		val genMember: ((Int) => (MemberInfo)) = (_: Int) => {
			val member: MemberInfo = new MemberInfo()
			member.read(stream, classfile.pool)
			member.attributes = range(stream.readShort()).map(genAttr).toArray
			member
		}
		classfile.fields     = range(stream.readShort()).map(genMember).toArray
		classfile.methods    = range(stream.readShort()).map(genMember).toArray
		classfile.attributes = range(stream.readShort()).map(genAttr).toArray
	}

	private def range(end: Int): Range = (0 until end)

	private def readConstantPool(i: Int, pool: Array[CInfo]): Array[CInfo] = {
		if(i >= pool.length) {
			return pool
		}
		val info: CInfo = CInfo(stream.readByte())
		info.read(stream)
		pool(i) = info
		if(info.getTag() == LONG || info.getTag() == DOUBLE) {
			pool(i + 1) = new Adapter()
			readConstantPool(i + 2, pool)
		} else {
			readConstantPool(i + 1, pool)
		}
	}
}