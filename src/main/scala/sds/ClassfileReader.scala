package sds

import java.io.{IOException, InputStream}

import sds.classfile.{MemberInfo, ClassfileStream => Stream}
import sds.classfile.attribute.{AttributeInfo => Attribute}
import sds.classfile.constant_pool.{ConstantType, NumberInfo, ConstantInfo => CInfo, ConstantInfoAdapter => Adapter, Utf8Info => Utf8}

import scala.annotation.tailrec

class ClassfileReader {
    val classfile: Classfile = new Classfile()

    def this(data: InputStream) {
        this()
        try {
            read(Stream(data))
        } catch {
            case e: IOException => e.printStackTrace()
        }
    }

    def this(fileName: String) {
        this()
        try {
            read(Stream(fileName))
        } catch {
            case e: IOException => e.printStackTrace()
        }
    }

    private def read(data: Stream): Unit = {
        classfile.magic = data.int
        classfile.minor = data.short
        classfile.major = data.short
        classfile.pool = readConstantPool(0, new Array[CInfo](data.short - 1), data)
        classfile.access = data.short
        classfile.thisClass = data.short
        classfile.superClass = data.short
        classfile.interfaces = (0 until data.short).map((_: Int) => data.short).toArray

        lazy val genAttr: ((Stream, Array[CInfo]) => (Attribute)) = (_data: Stream, _pool: Array[CInfo]) => {
            val name: Int = _data.short
            val utf8: Utf8 = _pool(name - 1).asInstanceOf[Utf8]
            Attribute(utf8.value, _data, _pool)
        }
        lazy val genMember: ((Int) => (MemberInfo)) = (_: Int) => new MemberInfo(data, classfile.pool, genAttr)
        classfile.fields     = (0 until data.short).map(genMember).toArray
        classfile.methods    = (0 until data.short).map(genMember).toArray
        classfile.attributes = (0 until data.short).map((_: Int) => genAttr(data, classfile.pool)).toArray
        data.close()
    }

    @tailrec
    private def readConstantPool(i: Int, pool: Array[CInfo], data: Stream): Array[CInfo] = {
        if(i >= pool.length) {
            return pool
        }
        val tag: Int = data.byte
        pool(i) = CInfo(tag, data)
        tag match {
            case ConstantType.LONG|ConstantType.DOUBLE =>
                pool(i + 1) = new Adapter()
                readConstantPool(i + 2, pool, data)
            case _ => readConstantPool(i + 1, pool, data)
        }
    }
}