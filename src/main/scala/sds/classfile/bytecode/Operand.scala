package sds.classfile.bytecode

import sds.classfile.constant_pool.ConstantInfo
import sds.util.{MultiArgsStringBuilder => Builder}
import sds.util.Utf8ValueExtractor.extract

object Operand {
	def get(op: OpcodeInfo, pool: Array[ConstantInfo]): String = {
		val b: Builder = new Builder()
		op match {
			case branch: BranchOpcode       => b.append(branch.getBranch())
			case iinc:   Iinc               => b.append(iinc.getIndex(), ",", iinc.getConst())
			case index:  IndexOpcode        => b.append(index.getIndex())
			case inter:  InvokeInterface    => b.append(extract(inter.getCount(), pool))
			case multi:  MultiANewArray     => b.append(extract(multi.getDimensions(), pool))
			case array:  NewArray           => b.append(array.getAType())
			case push:   PushOpcode         => b.append(push.getValue())
			case wide:   Wide               => b.append(wide.getConst(), ", ", extract(wide.getIndex(), pool))
			case ref:    HasReferenceOpcode => b.append(extract(ref.getIndex(), pool))
			case table:  TableSwitch        =>
				b.append(table.getOffset().mkString("[", ",", "]"), ", ", table.getDefault())
			case look:   LookupSwitch       => 
				val _match: Array[Int] = look.getMatch()
				val offset: Array[Int] = look.getOffset()
				b.append("[", (0 until _match.length).map((i: Int) => {
					_match(i) + ":" + offset(i)
				}).reduce((x, y) => x + "," + y), "], " + look.getDefault)
			case _ =>
		}
		b.toString()
	}
}