package sds.classfile.bytecode

import sds.classfile.constant_pool.ConstantInfo
import sds.util.Utf8ValueExtractor.extract

object Operand {
	def get(op: OpcodeInfo, pool: Array[ConstantInfo]): String = op match {
		case branch: BranchOpcode       => branch.getBranch().toString
		case iinc:   Iinc               => iinc.getIndex().toString +  "," + iinc.getConst().toString
		case index:  IndexOpcode        => index.getIndex().toString
		case inter:  InvokeInterface    => extract(inter.getCount(), pool)
		case multi:  MultiANewArray     => extract(multi.getDimensions(), pool)
		case array:  NewArray           => array.getAType()
		case push:   PushOpcode         => push.getValue().toString
		case wide:   Wide               => wide.getConst() + ", " + extract(wide.getIndex(), pool)
		case ref:    HasReferenceOpcode => extract(ref.getIndex(), pool)
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