package sds.classfile.constant_pool

class HandleInfo(_refKind: Int, _refIndex: Int) extends ConstantInfo(ConstantType.HANDLE) {
    def kind:  Int = _refKind
    def index: Int = _refIndex
    def kindValue: String = _refKind match {
        case 1 => "REF_getField"
        case 2 => "REF_getStatic"
        case 3 => "REF_putField"
        case 4 => "REF_putStatic"
        case 5 => "REF_invokeVirtual"
        case 6 => "REF_invokeStatic"
        case 7 => "REF_invokeSpecial"
        case 8 => "REF_newInvokeSpecial"
        case 9 => "REF_invokeInterface"
        case _ => throw new IllegalStateException("reference kind index is invalid value(" + _refKind + ")")
    }
    override def toString(): String = super.toString + "\t" + kindValue + ":#" + index
}