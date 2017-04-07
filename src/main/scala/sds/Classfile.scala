package sds

import sds.classfile.MemberInfo
import sds.classfile.attribute.AttributeInfo
import sds.classfile.constant_pool.ConstantInfo

class Classfile {
	var magic:  Int = -1
	var major:  Int = -1
	var minor:  Int = -1
	var access: Int = -1
	var thisClass:  Int = -1
	var superClass: Int = -1
	var interfaces: Array[Int] = new Array(0)
	var pool:       Array[ConstantInfo]  = new Array(0)
	var fields:     Array[MemberInfo]    = new Array(0)
	var methods:    Array[MemberInfo]    = new Array(0)
	var attributes: Array[AttributeInfo] = new Array(0)
}