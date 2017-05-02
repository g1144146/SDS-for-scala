package sds

import java.io.{
  IOException,
  InputStream,
  RandomAccessFile
}
import sds.classfile.{ClassfileStream, MemberInfo};
import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.{
  ConstantInfo        => CInfo,
  Utf8Info            => Utf8,
  ConstantInfoAdapter => Adapter
}
import sds.classfile.constant_pool.ConstantType.{LONG, DOUBLE}

class ClassfileReader {
	private var data: ClassfileStream = null
	val classfile: Classfile = new Classfile()

	def this(data: InputStream) {
		this()
		try {
			this.data = new ClassfileStream(data)
		} catch {
			case e: IOException => e.printStackTrace()
		}
	}

	def this(fileName: String) {
		this()
		try {
			this.data = new ClassfileStream(new RandomAccessFile(fileName, "r"))
		} catch {
			case e: IOException => e.printStackTrace()
		}
	}

	def read(): Unit = {
		classfile.magic = data.readInt()
		classfile.minor = data.readShort()
		classfile.major = data.readShort()
		classfile.pool = readConstantPool(0, new Array[CInfo](data.readShort() - 1))
		classfile.access = data.readShort()
		classfile.thisClass = data.readShort()
		classfile.superClass = data.readShort()
		classfile.interfaces = (0 until data.readShort()).map((_: Int) => data.readShort()).toArray

		lazy val genAttr: ((Int) => (AttributeInfo)) = (_: Int) => {
			val name: Int = data.readShort()
			val utf8: Utf8 = classfile.pool(name - 1).asInstanceOf[Utf8]
			AttributeInfo(utf8.value, data, classfile.pool)
		}
		lazy val genMember: ((Int) => (MemberInfo)) = (_: Int) => new MemberInfo(data, classfile.pool)
		classfile.fields     = (0 until data.readShort()).map(genMember).toArray
		classfile.methods    = (0 until data.readShort()).map(genMember).toArray
		classfile.attributes = (0 until data.readShort()).map(genAttr).toArray
	}

	private def readConstantPool(i: Int, pool: Array[CInfo]): Array[CInfo] = {
		if(i >= pool.length) {
			return pool
		}
		pool(i) = CInfo(data)
		if(pool(i).tag == LONG || pool(i).tag == DOUBLE) {
			pool(i + 1) = new Adapter()
			readConstantPool(i + 2, pool)
		} else {
			readConstantPool(i + 1, pool)
		}
	}
}