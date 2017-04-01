package sds.classfile

import java.lang.{Byte => By, Character, Double => Do, Float => Fl, Integer, Long => Lo, Short}
import java.io.{DataInputStream, InputStream, RandomAccessFile}

class ClassfileStream extends AutoCloseable {
	var stream: DataInputStream = null;
	var raf: RandomAccessFile = null;
	var pointer: Long = 0;

	def this(raf: RandomAccessFile) {
		this()
		this.raf = raf;
	}

	def this(stream: InputStream) {
		this()
		this.stream = new DataInputStream(stream);
	}

	def readByte(): Int = {
		pointer += By.BYTES
		if(raf != null) raf.readByte() else stream.readByte()
	}

	def readUnsignedByte(): Int = {
		pointer += By.BYTES
		if(raf != null) raf.readUnsignedByte() else stream.readUnsignedByte()
	}

	def readChar(): Char = {
		pointer += Character.BYTES
		if(raf != null) raf.readChar() else stream.readChar()
	}

	def readDouble(): Double = {
		pointer += Do.BYTES
		if(raf != null) raf.readDouble() else stream.readDouble()
	}

	def readFloat(): Float = {
		pointer += Fl.BYTES
		if(raf != null) raf.readFloat() else stream.readFloat()
	}

	def readInt(): Int = {
		pointer += Integer.BYTES
		if(raf != null) raf.readInt() else stream.readInt()
	}

	def readLong(): Long = {
		pointer += Lo.BYTES
		if(raf != null) raf.readLong() else stream.readLong()
	}

	def readShort(): Int = {
		pointer += Short.BYTES
		if(raf != null) raf.readShort() else stream.readShort()
	}

	def readUnsignedShort(): Int = {
		pointer += Short.BYTES
		if(raf != null) raf.readUnsignedShort() else stream.readUnsignedShort()
	}

	def readFully(b: Array[Byte]): Array[Byte] = {
		pointer += b.length
		if(raf != null) raf.readFully(b) else stream.readFully(b)
		b
	}

	def skipBytes(n: Int): Unit = {
		pointer += n
		if(raf != null) raf.skipBytes(n) else stream.skipBytes(n)
	}

	def getFilePointer(): Long = if(raf != null) raf.getFilePointer() else pointer

	override def close(): Unit = if(raf != null) raf.close() else stream.close()
}