package sds.classfile.attribute

import sds.classfile.ClassfileStream
import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}

class BootstrapMethods extends AttributeInfo(AttributeType.BootstrapMethods) {
	/**
	 * (bsm_reference, bootstrap_args)
	 */
	private var bsm: Array[(String, Array[String])] = null

	def getBSM(): Array[(String, Array[String])] = bsm

	override def read(data: ClassfileStream, pool: Array[ConstantInfo]): Unit = {
		this.bsm = (0 until data.readShort()).map((_: Int) => {
			val bsmRef: String = extract(data.readShort(), pool)
			val args: Array[String] = (0 until data.readShort()).map((i: Int) => {
				extract(data.readShort(), pool)
			}).toArray
			(bsmRef, args)
		}).toArray
	}

	override def toString(): String = {
		val b: Builder = new Builder(super.toString())
		b.append(": ")
		bsm.foreach((t: (String, Array[String])) => {
			b.append("(bsmRef: ", t._1, " | args: ", t._2.mkString(","), ")_")
		})
		b.toString()
	}
}