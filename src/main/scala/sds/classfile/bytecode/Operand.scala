package sds.classfile.bytecode

import sds.classfile.constant_pool.ConstantInfo
import sds.classfile.constant_pool.Utf8ValueExtractor.extract

object Operand {
    def get(op: OpcodeInfo, pool: Array[ConstantInfo]): String = op match {
        case branch: BranchOpcode       => branch.branch.toString
        case iinc:   Iinc               => s"${iinc.index.toString},${iinc.const.toString}"
        case index:  IndexOpcode        => index.index.toString
        case inter:  InvokeInterface    => s"${inter.count},${extract(inter.index, pool)}"
        case multi:  MultiANewArray     => s"${multi.dimensions},${extract(multi.index, pool)}"
        case array:  NewArray           => array.atype
        case push:   PushOpcode         => push.value.toString
        case wide:   Wide               => s"${wide.const}, ${extract(wide.index, pool)}"
        case ref:    HasReferenceOpcode => extract(ref.index, pool)
        case table:  TableSwitch        =>
            s"[${table.getOffset().mkString(", ")}, ${table.default}]"
        case look:   LookupSwitch       => 
            val _ma: Array[Int] = look.getMatch()
            val off: Array[Int] = look.getOffset()
            s"[${off.indices.map((i: Int) => s"${_ma(i)}:${off(i)}").mkString(", ")}, default:${look.default}]"
        case _ => ""
    }
}