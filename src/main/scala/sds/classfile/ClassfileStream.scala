package sds.classfile

import java.lang.{
  Byte   => By,
  Double => Do,
  Float  => Fl,
  Long   => Lo,
  Character,
  Integer,
  Short
}
import java.io.{
  DataInputStream,
  InputStream,
  RandomAccessFile
}

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

	def skipBytes(n: Int):   Unit   = if(isRaf(n))               raf.skipBytes(n) else stream.skipBytes(n)
	def readByte():          Int    = if(isRaf(By.BYTES))        raf.readByte()   else stream.readByte()
	def readChar():          Char   = if(isRaf(Character.BYTES)) raf.readChar()   else stream.readChar()
	def readDouble():        Double = if(isRaf(Do.BYTES))        raf.readDouble() else stream.readDouble()
	def readFloat():         Float  = if(isRaf(Fl.BYTES))        raf.readFloat()  else stream.readFloat()
	def readInt():           Int    = if(isRaf(Integer.BYTES))   raf.readInt()    else stream.readInt()
	def readLong():          Long   = if(isRaf(Lo.BYTES))        raf.readLong()   else stream.readLong()
	def readShort():         Int    = if(isRaf(Short.BYTES))     raf.readShort()  else stream.readShort()
	def readUnsignedByte():  Int    = if(isRaf(By.BYTES))    raf.readUnsignedByte()  else stream.readUnsignedByte()
	def readUnsignedShort(): Int    = if(isRaf(Short.BYTES)) raf.readUnsignedShort() else stream.readUnsignedShort()
	
	def readFully(b: Array[Byte]): Array[Byte] = {
		if(isRaf(b.length)) raf.readFully(b) else stream.readFully(b)
		b
	}

	def isRaf(bytes: Int): Boolean = {
		pointer += bytes
		raf != null
	}

	def getFilePointer(): Long = if(raf != null) raf.getFilePointer() else pointer
	override def close(): Unit = if(raf != null) raf.close() else stream.close()
}