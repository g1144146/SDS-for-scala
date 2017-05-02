package sds.util

object AccessFlag {
    private val PUBLIC:       Int = 0x0001
    private val PRIVATE:      Int = 0x0002
    private val PROTECTED:    Int = 0x0004
    private val STATIC:       Int = 0x0008
    private val FINAL:        Int = 0x0010
    private val SUPER:        Int = 0x0020
    private val SYNCHRONIZED: Int = 0x0020
    private val VOLATILE:     Int = 0x0040
    private val BRIDGE:       Int = 0x0040
    private val TRANSIENT:    Int = 0x0080
    private val VARARGS:      Int = 0x0080
    private val NATIVE:       Int = 0x0100
    private val INTERFACE:    Int = 0x0200
    private val ABSTRACT:     Int = 0x0400
    private val STRICT:       Int = 0x0800
    private val SYNTHETIC:    Int = 0x1000
    private val ANNOTATION:   Int = 0x2000
    private val ENUM:         Int = 0x4000
    private val MANDATED:     Int = 0x8000
    private val CLASS:  Int = PUBLIC | FINAL | SUPER | INTERFACE | ABSTRACT | SYNTHETIC | ANNOTATION | ENUM
    private val FIELD:  Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL | VOLATILE | TRANSIENT |
                              SYNTHETIC | ENUM
    private val METHOD: Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL | SYNCHRONIZED | BRIDGE | VARARGS | 
                              NATIVE | ABSTRACT | STRICT | SYNTHETIC
    private val NESTED: Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL |
                              INTERFACE | ABSTRACT | SYNTHETIC | ANNOTATION | ENUM
    private val LOCAL:  Int = FINAL

    def get(flag: Int, _type: String): String = {
        if(_type.eq("class")  && checkOr(flag, CLASS))  return getClassFlag(flag)
        if(_type.eq("field")  && checkOr(flag, FIELD))  return getFieldFlag(flag)
        if(_type.eq("method") && checkOr(flag, METHOD)) return getMethodFlag(flag)
        if(_type.eq("nested") && checkOr(flag, NESTED)) return getClassFlag(flag)
        if(_type.eq("local")  && checkOr(flag, LOCAL))  return getLocalFlag(flag)
        throw new IllegalArgumentException()
    }

    private def getClassFlag(flag: Int): String = {
        val flagStr: StringBuilder = new StringBuilder()
        if(checkAnd(flag, PUBLIC))     flagStr.append("public ")
        if(checkAnd(flag, STATIC))     flagStr.append("static ")
        if(checkAnd(flag, FINAL))      flagStr.append("final ")
        if(checkAnd(flag, ABSTRACT))   flagStr.append("abstract ")
        if(checkAnd(flag, SYNTHETIC))  flagStr.append("synthetic ")
        if(checkAnd(flag, ANNOTATION)) flagStr.append("@interface ")
        if(checkAnd(flag, ENUM))       flagStr.append("enum ")
        if(checkAnd(flag, INTERFACE) && ((flag & ANNOTATION) == 0)) {
            flagStr.append("interface ")
        }
        if((flag & (INTERFACE | ENUM | ANNOTATION)) == 0) {
            flagStr.append("class ")
        }
        flagStr.toString()
    }

    private def getFieldFlag(flag: Int): String = {
        val flagStr: StringBuilder = new StringBuilder()
        if(checkAnd(flag, PUBLIC))     flagStr.append("public ")
        if(checkAnd(flag, PRIVATE))    flagStr.append("private ")
        if(checkAnd(flag, PROTECTED))  flagStr.append("protected ")
        if(checkAnd(flag, STATIC))     flagStr.append("static ")
        if(checkAnd(flag, FINAL))      flagStr.append("final ")
        if(checkAnd(flag, VOLATILE))   flagStr.append("volatile ")
        if(checkAnd(flag, TRANSIENT))  flagStr.append("transient ")
        if(checkAnd(flag, SYNTHETIC))  flagStr.append("synthetic ")
        if(checkAnd(flag, ENUM))       flagStr.append("enum ")
        flagStr.toString()
    }

    private def getMethodFlag(flag: Int): String = {
        val flagStr: StringBuilder = new StringBuilder()
        if(checkAnd(flag, PUBLIC))         flagStr.append("public ")
        if(checkAnd(flag, PRIVATE))        flagStr.append("private ")
        if(checkAnd(flag, PROTECTED))      flagStr.append("protected ")
        if(checkAnd(flag, STATIC))         flagStr.append("static ")
        if(checkAnd(flag, FINAL))          flagStr.append("final ")
        if(checkAnd(flag, SYNCHRONIZED))   flagStr.append("synchronized ")
        if(checkAnd(flag, NATIVE))         flagStr.append("native ")
        if(checkAnd(flag, ABSTRACT))       flagStr.append("abstract ")
        if(checkAnd(flag, STRICT))         flagStr.append("strictfp ")
        if(checkAnd(flag, SYNTHETIC))      flagStr.append("synthetic ")
        flagStr.toString()
    }

    private def getLocalFlag(flag: Int): String = if(checkAnd(flag, FINAL)) "final " else ""
    private def checkAnd(target: Int, flag: Int): Boolean = (target & flag) == flag
    private def checkOr(target:  Int, flag: Int): Boolean = (target | flag) == flag
}