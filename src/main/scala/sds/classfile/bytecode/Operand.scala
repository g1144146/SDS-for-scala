package sds.classfile.bytecode

import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.Utf8ValueExtractor.extract

object Operand {
	def get(op: OpcodeInfo, pool: Array[ConstantInfo]): String = op match {
		case branch: BranchOpcode       => branch.branch.toString
		case iinc:   Iinc               => iinc.index.toString +  "," + iinc.const.toString
		case index:  IndexOpcode        => index.index.toString
		case inter:  InvokeInterface    => inter.getCount() + "," + extract(inter.index, pool)
		case multi:  MultiANewArray     => multi.getDimensions() + "," + extract(multi.index, pool)
		case array:  NewArray           => array.atype
		case push:   PushOpcode         => push.value.toString
		case wide:   Wide               => wide.getConst() + ", " + extract(wide.index, pool)
		case ref:    HasReferenceOpcode => extract(ref.index, pool)
		case table:  TableSwitch        =>
			table.getOffset().mkString("[", ",", "]") + ", " + table.getDefault()
		case look:   LookupSwitch       => 
			val _match: Array[Int] = look.getMatch()
			val offset: Array[Int] = look.getOffset()
			"[" + (0 until _match.length).map((i: Int) => _match(i) + ":" + offset(i))
			      .reduce((x, y) => x + "," + y) + "], " + look.getDefault
		case _ => ""
	}
}