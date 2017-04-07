package sds.classfile.bytecode

import sds.classfile.ClassfileStream

class BranchWide(data: ClassfileStream, _type: MnemonicTable.Value, pc: Int) extends BranchOpcode(data, _type, pc) {}