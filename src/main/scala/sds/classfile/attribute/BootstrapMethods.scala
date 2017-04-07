package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.attribute.AttributeType.BootstrapMethods
import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}

class BootstrapMethods(data: ClassfileStream, pool: Array[ConstantInfo]) extends AttributeInfo(BootstrapMethods) {
	/**
	 * (bsm_reference, bootstrap_args)
	 */
	private val bsm: Array[(String, Array[String])] = (0 until data.readShort()).map((_: Int) => {
		val bsmRef: String = extract(data.readShort(), pool)
		val args: Array[String] = (0 until data.readShort()).map((i: Int) => extract(data.readShort(), pool)).toArray
		(bsmRef, args)
	}).toArray

	def getBSM(): Array[(String, Array[String])] = bsm

	override def toString(): String = {
		val b: Builder = new Builder(super.toString())
		b.append(": ")
		bsm.foreach((t: (String, Array[String])) => {
			b.append("(bsmRef: ", t._1, " | args: ", t._2.mkString(","), ")_")
		})
		b.toString()
	}
}