package sds.classfile

import java.lang.{
  Byte   => By,
  Double => Do,
  Float  => Fl,
  Long   => Lo,
  Short
}
import java.io.{
  DataInputStream,
  InputStream,
  RandomAccessFile
}

sealed trait ClassfileStream extends AutoCloseable {
    def skip(n: Int): Unit
    def byte:         Int
    def double:       Double
    def float:        Float
    def int:          Int
    def long:         Long
    def short:        Int
    def unsignedByte: Int
    def pointer:      Int
    def fully(b: Array[Byte]): Array[Byte]
}

object ClassfileStream {
    def apply(filePath: String):    ClassfileStream = new ImplWithRandomAccessFile(filePath)
    def apply(stream: InputStream): ClassfileStream = new ImplWithDataInputStream(new DataInputStream(stream))
}

/**
  * This class is for specified classfile stream.
  * @param filePath classfile path
  */
private class ImplWithRandomAccessFile(filePath: String) extends ClassfileStream {
    private val raf: RandomAccessFile = new RandomAccessFile(filePath, "r")
    override def skip(n: Int): Unit   = raf.skipBytes(n)
    override def byte:         Int    = raf.readByte
    override def double:       Double = raf.readDouble
    override def float:        Float  = raf.readFloat
    override def int:          Int    = raf.readInt
    override def long:         Long   = raf.readLong
    override def short:        Int    = raf.readShort
    override def unsignedByte: Int    = raf.readUnsignedByte
    override def fully(b: Array[Byte]): Array[Byte] = {
        raf.readFully(b)
        b
    }
    override def pointer: Int  = raf.getFilePointer.toInt
    override def close(): Unit = raf.close()
}

/**
  * This class is for a classfile in specified jar file.
  * Because most jar file have some classfiles,
  * it does not hold classfile stream as 'val' for reducing used memory.
  * @param _stream classfile stream in jar file.
  */
private class ImplWithDataInputStream(_stream: DataInputStream) extends ClassfileStream {
    private var _pointer: Int = 0
    private  def stream: DataInputStream = _stream
    override def byte:         Int    = read(By.BYTES,      stream.readByte)
    override def double:       Double = read(Do.BYTES,      stream.readDouble)
    override def float:        Float  = read(Fl.BYTES,      stream.readFloat)
    override def int:          Int    = read(Integer.BYTES, stream.readInt)
    override def long:         Long   = read(Lo.BYTES,      stream.readLong)
    override def short:        Int    = read(Short.BYTES,   stream.readShort)
    override def unsignedByte: Int    = read(By.BYTES,      stream.readUnsignedByte)
    override def skip(n: Int): Unit = {
        _pointer += n
        stream.skipBytes(n)
    }
    override def fully(b: Array[Byte]): Array[Byte] = {
        stream.readFully(b)
        read(b.length, b)
    }
    override def pointer: Int  = _pointer
    override def close(): Unit = stream.close()
    private def read[T](bytes: Int, readValue: T): T = {
        _pointer += bytes
        readValue
    }
}