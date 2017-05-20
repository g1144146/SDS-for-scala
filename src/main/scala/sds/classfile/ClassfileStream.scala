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

class ClassfileStream extends AutoCloseable {
    def skip(n: Int):  Unit   = throw new UnsupportedOperationException("unimplemented method.")
    def char:          Char   = throw new UnsupportedOperationException("unimplemented method.")
    def byte:          Int    = throw new UnsupportedOperationException("unimplemented method.")
    def double:        Double = throw new UnsupportedOperationException("unimplemented method.")
    def float:         Float  = throw new UnsupportedOperationException("unimplemented method.")
    def int:           Int    = throw new UnsupportedOperationException("unimplemented method.")
    def long:          Long   = throw new UnsupportedOperationException("unimplemented method.")
    def short:         Int    = throw new UnsupportedOperationException("unimplemented method.")
    def unsignedByte:  Int    = throw new UnsupportedOperationException("unimplemented method.")
    def pointer:       Int    = throw new UnsupportedOperationException("unimplemented method.")
    def fully(b: Array[Byte]): Array[Byte] = throw new UnsupportedOperationException("unimplemented method.")
    override def close(): Unit = throw new UnsupportedOperationException("unimplemented method.")

    private def create(file: String):        ClassfileStream = new ImplWithRandomAccessFile(file)
    private def create(stream: InputStream): ClassfileStream = new ImplWithDataInputStream(stream)

    class ImplWithRandomAccessFile(file: String) extends ClassfileStream {
        private val raf: RandomAccessFile = new RandomAccessFile(file, "r")
        override def skip(n: Int):  Unit   = raf.skipBytes(n)
        override def byte:          Int    = raf.readByte
        override def char:          Char   = raf.readChar
        override def double:        Double = raf.readDouble
        override def float:         Float  = raf.readFloat
        override def int:           Int    = raf.readInt
        override def long:          Long   = raf.readLong
        override def short:         Int    = raf.readShort
        override def unsignedByte:  Int    = raf.readUnsignedByte
        override def fully(b: Array[Byte]): Array[Byte] = {
            raf.readFully(b)
            b
        }
        override def pointer: Int = raf.getFilePointer.toInt
        override def close(): Unit = raf.close()
    }

    class ImplWithDataInputStream(_stream: InputStream) extends ClassfileStream {
        private val stream: DataInputStream = new DataInputStream(_stream)
        private var _pointer: Int = 0
        override def byte:          Int    = read(By.BYTES,        stream.readByte)
        override def char:          Char   = read(Character.BYTES, stream.readChar)
        override def double:        Double = read(Do.BYTES,        stream.readDouble)
        override def float:         Float  = read(Fl.BYTES,        stream.readFloat)
        override def int:           Int    = read(Integer.BYTES,   stream.readInt)
        override def long:          Long   = read(Lo.BYTES,        stream.readLong)
        override def short:         Int    = read(Short.BYTES,     stream.readShort)
        override def unsignedByte:  Int    = read(By.BYTES,        stream.readUnsignedByte)
        override def skip(n: Int): Unit = {
            _pointer += n
            stream.skipBytes(n)
        }
        override def fully(b: Array[Byte]): Array[Byte] = {
            _pointer += b.length
            stream.readFully(b)
            b
        }
        override def pointer: Int = _pointer
        override def close(): Unit = stream.close()
        def read[T](bytes: Int, readValue: T): T = {
            _pointer += bytes
            readValue
        }
    }
}

object ClassfileStream {
    def apply(file: String):        ClassfileStream = new ClassfileStream().create(file)
    def apply(stream: InputStream): ClassfileStream = new ClassfileStream().create(stream)
}