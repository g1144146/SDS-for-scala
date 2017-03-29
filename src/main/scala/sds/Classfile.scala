package sds

import sds.classfile.ConstantPool

class Classfile {
	var magic:  Int = -1
	var major:  Int = -1
	var minor:  Int = -1
	var access: Int = -1
	var thisClass:  Int = -1
	var superClass: Int = -1
	var interfaces: Array[Int] = null
	var pool: ConstantPool = null
}
