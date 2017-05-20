package sds.classfile.attribute.annotation

import sds.classfile.ClassfileStream

class ElementValuePair(data: ClassfileStream) {
    val name: Int = data.short
    val value: ElementValue = new ElementValue(data)
}

class ElementValue(data: ClassfileStream) {
    val tag: Char = data.byte.asInstanceOf[Char]
    private var constVal:  Int = -1
    private var classInfo: Int = -1
    private var annotation: Annotation = null
    private var enumConst: EnumConstValue = null
    private var array: ArrayValue = null
    init()

    private def init(): Unit = tag match {
        case 'B'|'C'|'D'|'F'|'I'|'J'|'S'|'Z'|'s' => this.constVal = data.short
        case 'c' => this.classInfo  = data.short
        case 'e' => this.enumConst  = new EnumConstValue(data.short, data.short)
        case '@' => this.annotation = new Annotation(data)
        case '[' => this.array      = new ArrayValue(data)
        case _   => throw new RuntimeException("unknown tag(" + tag + ").")
    }

    def getConstVal():   Int = constVal
    def getClassInfo():  Int = classInfo
    def getAnnotation(): Annotation     = annotation
    def getEnumConst():  EnumConstValue = enumConst
    def getArray():      ArrayValue     = array
}