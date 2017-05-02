package sds.classfile.bytecode

import sds.classfile.ClassfileStream
import sds.classfile.Information
import sds.classfile.bytecode.{MnemonicTable => Table}
import sds.classfile.constant_pool.ConstantInfo

class OpcodeInfo(__type: String, _pc: Int) extends Information {
    def _type: String = __type
    def pc: Int = _pc
    override def toString(): String = _pc + " - " + __type
}

object OpcodeInfo {
    def apply(pc: Int, data: ClassfileStream, pool: Array[ConstantInfo]): OpcodeInfo = {
        val opcode: Int = data.readByte() & 0xff
        val opType: String = Table.OPCODES(opcode)
        if((0x00 to 0x0f).contains(opcode)  || (0x1a to 0x35).contains(opcode)  ||
           (0x3b to 0x83).contains(opcode)  || (0x85 to 0x98).contains(opcode)  ||
           (0xac to 0xb1).contains(opcode)  || opcode == 0xbe || opcode == 0xbf ||
           opcode == 0xc2 || opcode == 0xc3 || opcode == 0xca ) {
            return new OpcodeInfo(opType, pc);
        }
        opcode match {
            case 0x10 => new PushOpcode(data.readByte(),  opType, pc) /** bipush **/
            case 0x11 => new PushOpcode(data.readShort(), opType, pc) /** sipush **/
            case 0x12 => new HasReferenceOpcode(data.readUnsignedByte(), pool, opType, pc) /** ldc **/
            case 0x13  /** ldc_w **/
              |  0x14  /** ldc2_w **/
              |  0xb2  /** getstatic **/
              |  0xb3  /** putstatic **/
              |  0xb4  /** getfield **/
              |  0xb5  /** putfield **/
              |  0xb6  /** invokevirtual **/
              |  0xb7  /** invokespecial **/
              |  0xb8  /** invokestatic **/
              |  0xbb  /** new **/
              |  0xc0  /** checkcast **/
              |  0xbd  /** anewarray **/
              |  0xc1  /** instanceof **/
                      => new HasReferenceOpcode(data.readShort(), pool, opType, pc);
            case 0x15  /** iload **/
              |  0x16  /** lload **/
              |  0x17  /** fload **/
              |  0x18  /** dload **/
              |  0x19  /** aload **/
              |  0x36  /** istore **/
              |  0x37  /** lstore **/
              |  0x38  /** fstore **/
              |  0x39  /** dstore **/
              |  0x3a  /** astore **/
              |  0xa9  /** ret **/
                      => new IndexOpcode(data.readUnsignedByte(), opType, pc);
            case 0x84 => new Iinc(data.readUnsignedByte(), data.readByte(), pc);
            case 0x99  /** ifeq **/
              |  0x9a  /** ifne **/
              |  0x9b  /** iflt **/
              |  0x9c  /** ifge **/
              |  0x9d  /** ifgt **/
              |  0x9e  /** ifle **/
              |  0x9f  /** if_icmpeq **/
              |  0xa0  /** if_icmpne **/
              |  0xa1  /** if_icmplt **/
              |  0xa2  /** if_icmpge **/
              |  0xa3  /** if_icmpgt **/
              |  0xa4  /** if_icmple **/
              |  0xa5  /** if_acmpeq **/
              |  0xa6  /** if_acmpne **/
              |  0xa7  /** goto **/
              |  0xa8  /** jsr **/    
              |  0xc6  /** ifnull **/
              |  0xc7  /** ifnonnull **/
                      => new BranchOpcode(data.readShort(), opType, pc);
            case 0xaa => new TableSwitch(data, pc);
            case 0xab => new LookupSwitch(data, pc);
            case 0xb9 => new InvokeInterface(data, pool, pc);
            case 0xba => new InvokeDynamic(data, pool, pc);
            case 0xbc => new NewArray(data.readUnsignedByte(), pc);
            case 0xc4 => new Wide(data, pool, pc);
            case 0xc5 => new MultiANewArray(data, pool, pc);
            case 0xc8  /** goto_w **/
              |  0xc9  /** jsr_w **/
                      => new BranchOpcode(data.readInt(), opType, pc);
            case 0xfe => new OpcodeInfo(Table.OPCODES(0xcb), pc); 
            case 0xff => new OpcodeInfo(Table.OPCODES(0xcc), pc);
            case _    => throw new IllegalArgumentException("undefined opcode(" + opcode + ")");
        }
    }
}