/*
 * Copyright (c) 1997, 2011, Oracle and/or its affiliates. All rights reserved.
 * Copyright (c) 2015, 2016, Loongson Technology. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 *
 */

#ifndef CPU_MIPS_VM_NATIVEINST_MIPS_HPP
#define CPU_MIPS_VM_NATIVEINST_MIPS_HPP

#include "asm/assembler.hpp"
#include "memory/allocation.hpp"
#include "runtime/icache.hpp"
#include "runtime/os.hpp"
#include "utilities/top.hpp"

// We have interfaces for the following instructions:
// - NativeInstruction
// - - NativeCall
// - - NativeMovConstReg
// - - NativeMovConstRegPatching
// - - NativeMovRegMem
// - - NativeMovRegMemPatching
// - - NativeJump
// - - NativeIllegalOpCode
// - - NativeGeneralJump
// - - NativeReturn
// - - NativeReturnX (return with argument)
// - - NativePushConst
// - - NativeTstRegMem

// The base class for different kinds of native instruction abstractions.
// Provides the primitive operations to manipulate code relative to this.

class NativeInstruction VALUE_OBJ_CLASS_SPEC {
  friend class Relocation;

 public:
  enum mips_specific_constants {
    nop_instruction_code        =    0,	//sll zero, zero, zero
    nop_instruction_size        =    4
  };

  bool is_nop()                        { return long_at(0) == nop_instruction_code; }
  bool is_dtrace_trap();
  inline bool is_call();
  inline bool is_illegal();
  inline bool is_return();
  bool is_jump();
  inline bool is_cond_jump();
  bool is_safepoint_poll();
  
	//mips has no instruction to generate a illegal instrucion exception
	//we define ours: break 11 
	static int illegal_instruction();
  
	bool is_int_branch();
	bool is_float_branch();


 protected:
  address addr_at(int offset) const    { return address(this) + offset; }
  address instruction_address() const       { return addr_at(0); }
  address next_instruction_address() const  { return addr_at(BytesPerInstWord); }
  address prev_instruction_address() const	{ return addr_at(-BytesPerInstWord); }
  
  s_char sbyte_at(int offset) const    { return *(s_char*) addr_at(offset); }
  u_char ubyte_at(int offset) const    { return *(u_char*) addr_at(offset); }
  jint int_at(int offset) const         { return *(jint*) addr_at(offset); }
  intptr_t ptr_at(int offset) const    { return *(intptr_t*) addr_at(offset); }
  oop  oop_at (int offset) const       { return *(oop*) addr_at(offset); }
  int  long_at(int offset) const       { return *(jint*)addr_at(offset); }


  void set_char_at(int offset, char c)        { *addr_at(offset) = (u_char)c; wrote(offset); }
  void set_int_at(int offset, jint  i)        { *(jint*)addr_at(offset) = i;  wrote(offset); }
  void set_ptr_at (int offset, intptr_t  ptr) { *(intptr_t*) addr_at(offset) = ptr;  wrote(offset); }
  void set_oop_at (int offset, oop  o)        { *(oop*) addr_at(offset) = o;  wrote(offset); }
  void set_long_at(int offset, long  i);
  //void set_jlong_at(int offset, jlong i);
  //void set_addr_at(int offset, address x);

  int  insn_word() const { return long_at(0); }
  static bool is_op (int insn, Assembler::ops op) { return Assembler::opcode(insn) == (int)op; }
  bool is_op (Assembler::ops op)     const { return is_op(insn_word(), op); }
  bool is_rs (int insn, Register rs) const { return Assembler::rs(insn) == (int)rs->encoding(); }
  bool is_rs (Register rs)           const { return is_rs(insn_word(), rs); }
  bool is_rt (int insn, Register rt) const { return Assembler::rt(insn) == (int)rt->encoding(); }
  bool is_rt (Register rt) 	     const { return is_rt(insn_word(), rt); }

  static bool is_special_op (int insn, Assembler::special_ops op) { 
    return is_op(insn, Assembler::special_op) && Assembler::special(insn)==(int)op; 
  }
  bool is_special_op (Assembler::special_ops op) const { return is_special_op(insn_word(), op); }

  // This doesn't really do anything on Intel, but it is the place where
  // cache invalidation belongs, generically:
  void wrote(int offset);

 public:

  // unit test stuff
  static void test() {}                 // override for testing

  inline friend NativeInstruction* nativeInstruction_at(address address);
};

inline NativeInstruction* nativeInstruction_at(address address) {
  NativeInstruction* inst = (NativeInstruction*)address;
#ifdef ASSERT
  //inst->verify();
#endif
  return inst;
}

inline NativeCall* nativeCall_at(address address);
// The NativeCall is an abstraction for accessing/manipulating native call imm32/imm64
// instructions (used to manipulate inline caches, primitive & dll calls, etc.).
// MIPS has no call instruction with imm32/imm64. Usually, a call was done like this:
// 32 bits:
// 			lui 		rt, imm16
// 			addiu		rt, rt, imm16
// 			jalr 		rt
//			nop
// 			
// 64 bits:
//    		    lui   rd, imm(63...48);
//		    ori   rd, rd, imm(47...32);
//		    dsll  rd, rd, 16;
//		    ori   rd, rd, imm(31...16);
//		    dsll  rd, rd, 16;
//		    ori   rd, rd, imm(15...0);
//		    jalr  rd
//      	    nop
//		    

// we just consider the above for instruction as one call instruction
class NativeCall: public NativeInstruction {
  public:
    enum mips_specific_constants {
      instruction_offset          =    0,
#ifndef _LP64
      instruction_size            =   4 * BytesPerInstWord,
      return_address_offset       =   4 * BytesPerInstWord,
#else
      instruction_size            =   6 * BytesPerInstWord,
      return_address_offset       =   6 * BytesPerInstWord,
#endif
      displacement_offset         =    0
    };

    address instruction_address() const       { return addr_at(instruction_offset); }
    address next_instruction_address() const  { return addr_at(return_address_offset); }
    address return_address() const            { return addr_at(return_address_offset); }
    address destination() const;
    void  set_destination(address dest);
    void  set_destination_mt_safe(address dest) { set_destination(dest);}

    void  verify_alignment() {  }
    void  verify();
    void  print();

    // Creation
    inline friend NativeCall* nativeCall_at(address address);
    inline friend NativeCall* nativeCall_before(address return_address);

    static bool is_call_at(address instr) {
      return nativeInstruction_at(instr)->is_call();
    }

    static bool is_call_before(address return_address) {
      return is_call_at(return_address - NativeCall::return_address_offset);
    }

    static bool is_call_to(address instr, address target) {
      return nativeInstruction_at(instr)->is_call() &&
	nativeCall_at(instr)->destination() == target;
    }

    // MT-safe patching of a call instruction.
    static void insert(address code_pos, address entry);

    static void replace_mt_safe(address instr_addr, address code_buffer);
};

inline NativeCall* nativeCall_at(address address) {
  NativeCall* call = (NativeCall*)(address - NativeCall::instruction_offset);
#ifdef ASSERT
  call->verify();
#endif
  return call;
}

inline NativeCall* nativeCall_before(address return_address) {
  NativeCall* call = (NativeCall*)(return_address - NativeCall::return_address_offset);
#ifdef ASSERT
  call->verify();
#endif
  return call;
}

// An interface for accessing/manipulating native mov reg, imm32 instructions.
// (used to manipulate inlined 32bit data dll calls, etc.)
//we use two instructions to implement this:
//		lui rd, imm16
//		addiu rd, immm16
//see MacroAssembler::move(Register, int)
class NativeMovConstReg: public NativeInstruction {
 public:
  enum mips_specific_constants {
    instruction_offset  	=    0,
#ifndef _LP64
    instruction_size  	      	=    2 * BytesPerInstWord,
    next_instruction_offset 	=    2 * BytesPerInstWord,
#else
    instruction_size  	      	=    4 * BytesPerInstWord,
    next_instruction_offset 	=    4 * BytesPerInstWord,
#endif
  };
  
  int     insn_word() const                 { return long_at(instruction_offset); }
  address instruction_address() const       { return addr_at(0); }
  address next_instruction_address() const  { return addr_at(next_instruction_offset); }
  intptr_t data() const;
  void    set_data(intptr_t x);
	

  void  verify();
  void  print();

  // unit test stuff
  static void test() {}

  // Creation
  inline friend NativeMovConstReg* nativeMovConstReg_at(address address);
  inline friend NativeMovConstReg* nativeMovConstReg_before(address address);
};

inline NativeMovConstReg* nativeMovConstReg_at(address address) {
  NativeMovConstReg* test = (NativeMovConstReg*)(address - NativeMovConstReg::instruction_offset);
#ifdef ASSERT
  test->verify();
#endif
  return test;
}

inline NativeMovConstReg* nativeMovConstReg_before(address address) {
  NativeMovConstReg* test = (NativeMovConstReg*)(address - NativeMovConstReg::instruction_size - NativeMovConstReg::instruction_offset);
#ifdef ASSERT
  test->verify();
#endif
  return test;
}

class NativeMovConstRegPatching: public NativeMovConstReg {
 private:
    friend NativeMovConstRegPatching* nativeMovConstRegPatching_at(address address) {
    NativeMovConstRegPatching* test = (NativeMovConstRegPatching*)(address - instruction_offset);
    #ifdef ASSERT
      test->verify();
    #endif
    return test;
  }
};

// An interface for accessing/manipulating native moves of the form:
// 			lui   AT, split_high(offset)
// 			addiu AT, split_low(offset)
// 			add   reg, reg, AT
// 			lb/lbu/sb/lh/lhu/sh/lw/sw/lwc1/swc1 dest, reg, 0
// 			[lw/sw/lwc1/swc1                    dest, reg, 4]
// 		or 
// 			lb/lbu/sb/lh/lhu/sh/lw/sw/lwc1/swc1 dest, reg, offset
// 			[lw/sw/lwc1/swc1                    dest, reg, offset+4]
//
// Warning: These routines must be able to handle any instruction sequences
// that are generated as a result of the load/store byte,word,long
// macros.  

class NativeMovRegMem: public NativeInstruction {
 public:
  enum mips_specific_constants {
    instruction_offset  = 0,
    hiword_offset 	= 4,
    ldst_offset   	= 12,
    immediate_size	= 4,
    ldst_size     	= 16
  };

  //offset is less than 16 bits.
  bool is_immediate() const { return !is_op(long_at(instruction_offset), Assembler::lui_op); }
  bool is_64ldst() const {
    if (is_immediate()) {
      return (Assembler::opcode(long_at(hiword_offset)) == Assembler::opcode(long_at(instruction_offset))) &&
	     (Assembler::imm_off(long_at(hiword_offset)) == Assembler::imm_off(long_at(instruction_offset)) + wordSize);
    } else {
      return (Assembler::opcode(long_at(ldst_offset+hiword_offset)) == Assembler::opcode(long_at(ldst_offset))) &&
	     (Assembler::imm_off(long_at(ldst_offset+hiword_offset)) == Assembler::imm_off(long_at(ldst_offset)) + wordSize);
    }
  }

  address instruction_address() const       { return addr_at(instruction_offset); }
  address next_instruction_address() const  { 
    return addr_at( (is_immediate()? immediate_size : ldst_size) + (is_64ldst()? 4 : 0));  
  }
/*  // helper
  int instruction_start() const;

  address instruction_address() const;

  address next_instruction_address() const;
*/

  int   offset() const;

  void  set_offset(int x);

  void  add_offset_in_bytes(int add_offset)     { set_offset ( ( offset() + add_offset ) ); }

  void verify();
  void print ();

  // unit test stuff
  static void test() {}

 private:
  inline friend NativeMovRegMem* nativeMovRegMem_at (address address);
};

inline NativeMovRegMem* nativeMovRegMem_at (address address) {
  NativeMovRegMem* test = (NativeMovRegMem*)(address - NativeMovRegMem::instruction_offset);
#ifdef ASSERT
  test->verify();
#endif
  return test;
}

class NativeMovRegMemPatching: public NativeMovRegMem {
 private:
  friend NativeMovRegMemPatching* nativeMovRegMemPatching_at (address address) {
    NativeMovRegMemPatching* test = (NativeMovRegMemPatching*)(address - instruction_offset);
    #ifdef ASSERT
      test->verify();
    #endif
    return test;
  }
};


// Handles all kinds of jump on Loongson. Long/far, conditional/unconditional
// 32 bits:
//    far jump:
//				lui   reg, split_high(addr)
//				addiu reg, split_low(addr)
//				jr    reg
//				nop
//    or 
//				beq 	ZERO, ZERO, offset
//				nop
//

//64 bits:
//    far jump:
//			    lui   rd, imm(63...48);
//			    ori   rd, rd, imm(47...32);
//			    dsll  rd, rd, 16;
//			    ori   rd, rd, imm(31...16);
//			    dsll  rd, rd, 16;
//			    ori   rd, rd, imm(15...0);
//			    jalr  rd
//			    nop
//		    
class NativeGeneralJump: public NativeInstruction {
public:
  enum mips_specific_constants {
    instruction_offset 	=    0,
    beq_opcode         	=    0x10000000,//000100|00000|00000|offset
    b_mask       	=    0xffff0000,
    short_size    	=    8,
#ifndef _LP64
    instruction_size   =    4 * BytesPerInstWord
#else
    instruction_size   =    6 * BytesPerInstWord
#endif
  };

  bool is_short() const { return (long_at(instruction_offset) & b_mask) == beq_opcode; }
#ifdef _LP64
  bool is_b_far();
#endif
  address instruction_address() const { return addr_at(instruction_offset); }
  address jump_destination();

  void  set_jump_destination(address dest);

	// Creation
  inline friend NativeGeneralJump* nativeGeneralJump_at(address address);

	// Insertion of native general jump instruction
  static void insert_unconditional(address code_pos, address entry);
  static void replace_mt_safe(address instr_addr, address code_buffer);
  static void check_verified_entry_alignment(address entry, address verified_entry){}
  static void patch_verified_entry(address entry, address verified_entry, address dest);

  void verify();
};

inline NativeGeneralJump* nativeGeneralJump_at(address address) {
  NativeGeneralJump* jump = (NativeGeneralJump*)(address);
  debug_only(jump->verify();)
  return jump;
}

/*class NativePopReg : public NativeInstruction {
  public:
  enum Intel_specific_constants {
  instruction_code            = 0x58,
  instruction_size            =    1,
  instruction_offset          =    0,
  data_offset                 =    1,
  next_instruction_offset     =    1
  };

// Insert a pop instruction
static void insert(address code_pos, Register reg);
};*/


class NativeIllegalInstruction: public NativeInstruction {
public:
  enum Intel_specific_constants {
    instruction_size          =    4,
    instruction_offset        =    0,
    next_instruction_offset   =    4
  };

  // Insert illegal opcode as specific address
  static void insert(address code_pos);
};

// return instruction that does not pop values of the stack
// jr RA
// delay slot
class NativeReturn: public NativeInstruction {
public:
  enum mips_specific_constants {
    instruction_size          =    8,
    instruction_offset        =    0,
    next_instruction_offset   =    8
  };
};




class NativeCondJump;
inline NativeCondJump* nativeCondJump_at(address address);
class NativeCondJump: public NativeInstruction {
public:
  enum mips_specific_constants {
    instruction_size 	      = 16,
    instruction_offset        = 12,
    next_instruction_offset   = 20
  };


  int insn_word() const  { return long_at(instruction_offset); }
  address instruction_address() const { return addr_at(0); }
  address next_instruction_address() const { return addr_at(next_instruction_offset); }
		
  // Creation
  inline friend NativeCondJump* nativeCondJump_at(address address);

  address jump_destination()  const {
    return ::nativeCondJump_at(addr_at(12))->jump_destination();
  }

  void set_jump_destination(address dest) {
    ::nativeCondJump_at(addr_at(12))->set_jump_destination(dest);
  }

};	

inline NativeCondJump* nativeCondJump_at(address address) {
  NativeCondJump* jump = (NativeCondJump*)(address);
  return jump;
}



inline bool NativeInstruction::is_illegal() { return insn_word() == illegal_instruction(); }

inline bool NativeInstruction::is_call()    { 
#ifndef _LP64
  return is_op(long_at(0), Assembler::lui_op) &&
         is_op(long_at(4), Assembler::addiu_op) &&
	 is_special_op(long_at(8), Assembler::jalr_op);
#else
  /* li64 or li48 */
  if (is_special_op(long_at(16), Assembler::dsll_op)) {
  /* li64 */
    return is_op(long_at(0), Assembler::lui_op) &&
           is_op(long_at(4), Assembler::ori_op) &&
           is_special_op(long_at(8), Assembler::dsll_op) &&
           is_op(long_at(12), Assembler::ori_op) &&
           is_special_op(long_at(16), Assembler::dsll_op) &&
           is_op(long_at(20), Assembler::ori_op) &&
	   is_special_op(long_at(24), Assembler::jalr_op);
  } else {
  /* li48 */
    return is_op(long_at(0), Assembler::lui_op) &&
           is_op(long_at(4), Assembler::ori_op) &&
           is_special_op(long_at(8), Assembler::dsll_op) &&
           is_op(long_at(12), Assembler::ori_op) &&
	   is_special_op(long_at(16), Assembler::jalr_op);
  }
#endif
}

inline bool NativeInstruction::is_return()  { return is_special_op(Assembler::jr_op) && is_rs(RA);}

inline bool NativeInstruction::is_cond_jump()    { return is_int_branch() || is_float_branch(); }
#endif // CPU_MIPS_VM_NATIVEINST_MIPS_HPP

