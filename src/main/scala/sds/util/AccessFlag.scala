package sds.util

object AccessFlag {
    private val PUBLIC:       (Int, String) = (0x0001, "public "      )
    private val PRIVATE:      (Int, String) = (0x0002, "private "     )
    private val PROTECTED:    (Int, String) = (0x0004, "protected "   )
    private val STATIC:       (Int, String) = (0x0008, "static "      )
    private val FINAL:        (Int, String) = (0x0010, "final "       )
    private val SYNCHRONIZED: (Int, String) = (0x0020, "synchronized ")
    private val VOLATILE:     (Int, String) = (0x0040, "volatile "    )
    private val TRANSIENT:    (Int, String) = (0x0080, "transient "   )
    private val NATIVE:       (Int, String) = (0x0100, "native "      )
    private val INTERFACE:    (Int, String) = (0x0200, "interface "   )
    private val ABSTRACT:     (Int, String) = (0x0400, "abstract "    )
    private val STRICT:       (Int, String) = (0x0800, "strictfp "    )
    private val SYNTHETIC:    (Int, String) = (0x1000, "synthetic "   )
    private val ANNOTATION:   (Int, String) = (0x2000, "@interface "  )
    private val ENUM:         (Int, String) = (0x4000, "enum "        )
    private val SUPER:        Int = 0x0020
    private val BRIDGE:       Int = 0x0040
    private val VARARGS:      Int = 0x0080
    private val CLASS:  Int = PUBLIC._1   | FINAL._1     | SUPER         | INTERFACE._1 |
                              ABSTRACT._1 | SYNTHETIC._1 | ANNOTATION._1 | ENUM._1
    private val FIELD:  Int = PUBLIC._1 | PRIVATE._1 | PROTECTED._1 | STATIC._1 | FINAL._1 | VOLATILE._1 | TRANSIENT._1 |
                              SYNTHETIC._1 | ENUM._1
    private val METHOD: Int = PUBLIC._1 | PRIVATE._1 | PROTECTED._1 | STATIC._1   |  FINAL._1 | SYNCHRONIZED._1 |
                              BRIDGE    | VARARGS    | NATIVE._1    | ABSTRACT._1 | STRICT._1 | SYNTHETIC._1
    private val NESTED: Int = PUBLIC._1    | PRIVATE._1  | PROTECTED._1 | STATIC._1     | FINAL._1 |
                              INTERFACE._1 | ABSTRACT._1 | SYNTHETIC._1 | ANNOTATION._1 | ENUM._1
    private val LOCAL:  Int = FINAL._1

    def get(flag: Int, _type: String): String = {
        if(_type.eq("class")  && checkOr(flag, CLASS))  return getClassFlag(flag)
        if(_type.eq("field")  && checkOr(flag, FIELD))  return getFieldFlag(flag)
        if(_type.eq("method") && checkOr(flag, METHOD)) return getMethodFlag(flag)
        if(_type.eq("nested") && checkOr(flag, NESTED)) return getClassFlag(flag)
        if(_type.eq("local")  && checkOr(flag, LOCAL))  return getLocalFlag(flag)
        throw new IllegalArgumentException()
    }

    private def getClassFlag(flag: Int): String  = {
        val _type: String = if(checkAnd(flag, INTERFACE._1) && ((flag & ANNOTATION._1) == 0)) "interface "
                            else if((flag & (INTERFACE._1 | ENUM._1 | ANNOTATION._1)) == 0)   "class "    else ""
        build(flag, PUBLIC, STATIC, FINAL, ABSTRACT, SYNTHETIC, ANNOTATION, ENUM) + _type
    }
    private def getFieldFlag(flag: Int): String  =
        build(flag, PUBLIC, PRIVATE, PROTECTED, STATIC, FINAL, VOLATILE, TRANSIENT, SYNTHETIC, ENUM)
    private def getMethodFlag(flag: Int): String =
        build(flag, PUBLIC, PRIVATE, PROTECTED, STATIC, FINAL, SYNCHRONIZED, NATIVE, ABSTRACT, STRICT, SYNTHETIC)
    private def getLocalFlag(flag: Int): String = if(checkAnd(flag, FINAL._1)) "final " else ""

    private def build(target: Int, flags: (Int, String)*): String =
        flags.filter((t: (Int, String)) => checkAnd(target, t._1)).map(_._2).mkString
    private def checkAnd(target: Int, flag: Int): Boolean = (target & flag) == flag
    private def checkOr(target:  Int, flag: Int): Boolean = (target | flag) == flag
}