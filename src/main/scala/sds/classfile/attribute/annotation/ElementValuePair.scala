package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream

class ElementValuePair(data: ClassfileStream) {
    private val name: Int = data.readShort()
    private val value: ElementValue = new ElementValue(data)

    def getName(): Int = name
    def getValue(): ElementValue = value
}

class ElementValue(data: ClassfileStream) {
    private val tag: Char = data.readByte().asInstanceOf[Char]
    private var constVal:  Int = -1
    private var classInfo: Int = -1
    private var annotation: Annotation = null
    private var enumConst: EnumConstValue = null
    private var array: ArrayValue = null
    init()

    private def init(): Unit = tag match {
        case 'B'|'C'|'D'|'F'|'I'|'J'|'S'|'Z' => this.constVal = data.readShort()
        case 'c' => this.classInfo  = data.readShort()
        case 'e' => this.enumConst  = new EnumConstValue(data.readShort(), data.readShort())
        case '@' => this.annotation = new Annotation(data)
        case '[' => this.array      = new ArrayValue(data)
        case _   => throw new RuntimeException("unknow tag(" + tag + ").")
    }

    def getTag(): Char = tag
    def getConstVal():   Int = constVal
    def getClassInfo():  Int = classInfo
    def getAnnotation(): Annotation     = annotation
    def getEnumConst():  EnumConstValue = enumConst
    def getArray():      ArrayValue     = array
}