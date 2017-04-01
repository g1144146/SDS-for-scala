package sds.util

object AccessFlag {
	val PUBLIC:       Int = 0x0001
	val PRIVATE:      Int = 0x0002
	val PROTECTED:    Int = 0x0004
	val STATIC:       Int = 0x0008
	val FINAL:        Int = 0x0010
	val SUPER:        Int = 0x0020
	val SYNCHRONIZED: Int = 0x0020
	val VOLATILE:     Int = 0x0040
	val BRIDGE:       Int = 0x0040
	val TRANSIENT:    Int = 0x0080
	val VARARGS:      Int = 0x0080
	val NATIVE:       Int = 0x0100
	val INTERFACE:    Int = 0x0200
	val ABSTRACT:     Int = 0x0400
	val STRICT:       Int = 0x0800
	val SYNTHETIC:    Int = 0x1000
	val ANNOTATION:   Int = 0x2000
	val ENUM:         Int = 0x4000
	val MANDATED:     Int = 0x8000
	val CLASS:  Int = PUBLIC | FINAL | SUPER | INTERFACE | ABSTRACT | SYNTHETIC | ANNOTATION | ENUM
	val FIELD:  Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL | VOLATILE | TRANSIENT | SYNTHETIC | ENUM
	val METHOD: Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL | SYNCHRONIZED | BRIDGE | VARARGS | 
	                  NATIVE | ABSTRACT | STRICT | SYNTHETIC
	val NESTED: Int = PUBLIC | PRIVATE | PROTECTED | STATIC | FINAL |
	                  INTERFACE | ABSTRACT | SYNTHETIC | ANNOTATION | ENUM
	val LOCAL:  Int = FINAL | SYNTHETIC | MANDATED

	def get(flag: Int, _type: String): String = {
		if(_type.eq("class")  && checkOr(flag, CLASS))  getClassFlag(flag)
		if(_type.eq("field")  && checkOr(flag, FIELD))  getFieldFlag(flag)
		if(_type.eq("method") && checkOr(flag, METHOD)) getMethodFlag(flag)
		if(_type.eq("nested") && checkOr(flag, NESTED)) getClassFlag(flag)
		if(_type.eq("local")  && checkOr(flag, LOCAL))  getLocalFlag(flag)
		throw new IllegalArgumentException()
	}

	private def getClassFlag(flag: Int): String = {
		val flagStr: StringBuilder = new StringBuilder()
		if(checkAnd(flag, PUBLIC))     flagStr.append("public ")
		if(checkAnd(flag, STATIC))     flagStr.append("static ")
		if(checkAnd(flag, FINAL))      flagStr.append("final ")
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
		if(checkAnd(flag, BRIDGE))         flagStr.append("bridge ")
		if(checkAnd(flag, NATIVE))         flagStr.append("native ")
		if(checkAnd(flag, ABSTRACT))       flagStr.append("abstract ")
		if(checkAnd(flag, STRICT))         flagStr.append("strict ")
		if(checkAnd(flag, SYNTHETIC))      flagStr.append("synthetic ")
		flagStr.toString()
	}

	private def getLocalFlag(flag: Int): String = {
		val flagStr: StringBuilder = new StringBuilder()
		if(checkAnd(flag, FINAL))     flagStr.append("final ")
		if(checkAnd(flag, SYNTHETIC)) flagStr.append("synthetic ")
		if(checkAnd(flag, MANDATED))  flagStr.append("mandated ")
		flagStr.toString()
	}

	private def checkAnd(target: Int, flag: Int): Boolean = (target & flag) == flag
	private def checkOr(target:  Int, flag: Int): Boolean = (target | flag) == flag
}