package sds

import java.io.{IOException, InputStream, RandomAccessFile};
import sds.classfile.{ClassfileStream, MemberInfo};
import sds.classfile.attribute.{AttributeInfo, AttributeInfoFactory => AFactory}
import sds.classfile.constant_pool.{ConstantInfo => CInfo, Utf8Info => Utf8,
                                    ConstantInfoFactory => CFactory, ConstantInfoAdapter => Adapter}
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
		classfile.pool = readConstantPool(0, new CFactory(), new Array[CInfo](stream.readShort() - 1))
		classfile.access = stream.readShort()
		classfile.thisClass = stream.readShort()
		classfile.superClass = stream.readShort()
		classfile.interfaces = (0 until stream.readShort()).map((_: Int) => stream.readShort()).toArray
		classfile.fields     = (0 until stream.readShort()).map(genMember(_)).toArray
		classfile.methods    = (0 until stream.readShort()).map(genMember(_)).toArray
		classfile.attributes = (0 until stream.readShort()).map(genAttribute(_)).toArray
	}

	private def readConstantPool(i: Int, factory: CFactory, pool: Array[CInfo]): Array[CInfo] = {
		if(i >= pool.length) {
			return pool
		}
		val info: CInfo = factory.create(stream.readByte())
		info.read(stream)
		pool(i) = info
		if(info.getTag() == LONG || info.getTag() == DOUBLE) {
			pool(i + 1) = new Adapter()
			readConstantPool(i + 2, factory, pool)
		} else {
			readConstantPool(i + 1, factory, pool)
		}
	}

	private def genMember(n: Int): MemberInfo = {
		val member: MemberInfo = new MemberInfo()
		member.read(stream, classfile.pool)
		val attrCount: Int = stream.readShort()
		member.attributes = (0 until attrCount).map(genAttribute(_)).toArray
		member
	}

	private def genAttribute(n: Int): AttributeInfo = {
		val name: Int = stream.readShort()
		val utf8: Utf8 = classfile.pool(name - 1).asInstanceOf[Utf8]
		val factory: AFactory = new AFactory()
		val info: AttributeInfo = factory.create(utf8.getValue, stream.readInt())
		info.read(stream, classfile.pool)
		info
	}
}