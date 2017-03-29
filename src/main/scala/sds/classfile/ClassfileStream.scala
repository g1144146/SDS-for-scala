package sds.classfile

import java.lang.{Byte => By, Character, Double => Do, Float => Fl, Integer, Long => Lo, Short}
import java.io.{ByteArrayInputStream, DataInputStream, IOException, InputStream, RandomAccessFile}

class ClassfileStream(stream: InputStream) extends AutoCloseable {
	var stream: DataInputStream = null;
	var raf: RandomAccessFile = null;
	var pointer: Long = 0;

	init(stream)

	@throws(classOf[IOException])
	def this(raf: RandomAccessFile) {
		this(new ByteArrayInputStream(new Array[Byte](0)))
		this.raf = raf;
	}

	@throws(classOf[IOException])
	private def init(stream: InputStream): Unit = {
		if(stream.isInstanceOf[ByteArrayInputStream]) return
		this.stream = new DataInputStream(stream);
	}

	@throws(classOf[IOException])
	def readByte(): Int = {
		pointer += By.BYTES
		if(raf != null) raf.readByte() else stream.readByte()
	}

	@throws(classOf[IOException])
	def readUnsignedByte(): Int = {
		pointer += By.BYTES
		if(raf != null) raf.readUnsignedByte() else stream.readUnsignedByte()
	}

	@throws(classOf[IOException])
	def readChar(): Char = {
		pointer += Character.BYTES
		if(raf != null) raf.readChar() else stream.readChar()
	}

	@throws(classOf[IOException])
	def readDouble(): Double = {
		pointer += Do.BYTES
		if(raf != null) raf.readDouble() else stream.readDouble()
	}

	@throws(classOf[IOException])
	def readFloat(): Float = {
		pointer += Fl.BYTES
		if(raf != null) raf.readFloat() else stream.readFloat()
	}

	@throws(classOf[IOException])
	def readInt(): Int = {
		pointer += Integer.BYTES
		if(raf != null) raf.readInt() else stream.readInt()
	}

	@throws(classOf[IOException])
	def readLong(): Long = {
		pointer += Lo.BYTES
		if(raf != null) raf.readLong() else stream.readLong()
	}

	@throws(classOf[IOException])
	def readShort(): Int = {
		pointer += Short.BYTES
		if(raf != null) raf.readShort() else stream.readShort()
	}

	@throws(classOf[IOException])
	def readUnsignedShort(): Int = {
		pointer += Short.BYTES
		if(raf != null) raf.readUnsignedShort() else stream.readUnsignedShort()
	}

	@throws(classOf[IOException])
	def readFully(b: Array[Byte]): Array[Byte] = {
		pointer += b.length
		if(raf != null) raf.readFully(b) else stream.readFully(b)
		return b
	}

	@throws(classOf[IOException])
	def skipBytes(n: Int): Unit = {
		pointer += n
		if(raf != null) raf.skipBytes(n) else stream.skipBytes(n)
	}

	@throws(classOf[IOException])
	def getFilePointer(): Long = if(raf != null) raf.getFilePointer() else pointer

	@throws(classOf[IOException])
	override def close(): Unit = if(raf != null) raf.close() else stream.close()
}