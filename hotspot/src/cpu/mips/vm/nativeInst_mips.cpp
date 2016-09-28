/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
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

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_mips.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/handles.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "utilities/ostream.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

#include <sys/mman.h>

void NativeInstruction::wrote(int offset) {
  ICache::invalidate_word(addr_at(offset));
}

void NativeInstruction::set_long_at(int offset, long i) {
  address addr = addr_at(offset);
  *(long*)addr = i;
  //ICache::invalidate_word(addr);
}

static int illegal_instruction_bits = 0;

int NativeInstruction::illegal_instruction() {
	if (illegal_instruction_bits == 0) {
		ResourceMark rm;
		char buf[40];
		CodeBuffer cbuf((address)&buf[0], 20);     
		MacroAssembler* a = new MacroAssembler(&cbuf);     
		address ia = a->pc();     
		a->brk(11);
		int bits = *(int*)ia;
		illegal_instruction_bits = bits;   
	}
	return illegal_instruction_bits;
}

bool NativeInstruction::is_int_branch() {
	switch(Assembler::opcode(insn_word())) {
		case Assembler::beq_op:
		case Assembler::beql_op:
		case Assembler::bgtz_op:
		case Assembler::bgtzl_op:
		case Assembler::blez_op:
		case Assembler::blezl_op:
		case Assembler::bne_op:
		case Assembler::bnel_op:
			return true;
		case Assembler::regimm_op:
			switch(Assembler::rt(insn_word())) {
				case Assembler::bgez_op:
				case Assembler::bgezal_op:
				case Assembler::bgezall_op:
				case Assembler::bgezl_op:
				case Assembler::bltz_op:
				case Assembler::bltzal_op:
				case Assembler::bltzall_op:
				case Assembler::bltzl_op:
					return true;
			}
	}

	return false;
}

bool NativeInstruction::is_float_branch() {
	if (!is_op(Assembler::cop1_op) || 
			!is_rs((Register)Assembler::bc_op)) return false;

	switch(Assembler::rt(insn_word())) {
		case Assembler::bcf_op:
		case Assembler::bcfl_op:
		case Assembler::bct_op:
		case Assembler::bctl_op:
			return true;
	}

	return false;
}


//-------------------------------------------------------------------

void NativeCall::verify() {
  // make sure code pattern is actually a call instruction
#ifndef _LP64
  if (	!is_op(Assembler::lui_op) || 
	!is_op(int_at(4), Assembler::addiu_op) || 
	!is_special_op(int_at(8), Assembler::jalr_op) ) {
      fatal("not a call");
  }
#else
  /* li64 or li48 */
  int li_64 = 0;
  int li_48 = 0;

  if (  is_op	(Assembler::lui_op) &&
	  is_op	(int_at(4), Assembler::ori_op) &&
	  is_special_op(int_at(8), Assembler::dsll_op) &&
	  is_op	(int_at(12), Assembler::ori_op) &&
	  is_special_op(int_at(16), Assembler::dsll_op) &&
	  is_op	(int_at(20), Assembler::ori_op) &&
	  is_special_op(int_at(24), Assembler::jalr_op) ) {
      li_64 = 1;
  }

  if (  is_op	(Assembler::lui_op) &&
	  is_op	(int_at(4), Assembler::ori_op) &&
	  is_special_op(int_at(8), Assembler::dsll_op) &&
	  is_op	(int_at(12), Assembler::ori_op) &&
	  is_special_op(int_at(16), Assembler::jalr_op) ) {
      li_48 = 1;
  }

  if (!li_64 && !li_48) {
tty->print_cr("NativeCall::verify addr=%lx", addr_at(0));
      fatal("not a call");
  }
#endif
}

address NativeCall::destination() const {
#ifndef _LP64
  return (address)Assembler::merge(int_at(4)&0xffff, long_at(0)&0xffff);
#else
  /* li64 or li48 */
  if (is_special_op(int_at(16), Assembler::dsll_op)) {
    return (address)Assembler::merge( (intptr_t)(int_at(20) & 0xffff), 
				    (intptr_t)(int_at(12) & 0xffff),
				    (intptr_t)(int_at(4) & 0xffff),
				    (intptr_t)(int_at(0) & 0xffff));
  } else if (is_special_op(int_at(16), Assembler::jalr_op)) {
    return (address)Assembler::merge( (intptr_t)(int_at(12) & 0xffff), 
				    (intptr_t)(int_at(4) & 0xffff),
				    (intptr_t)(int_at(0) & 0xffff),
				    (intptr_t)0);
  }
#endif
}

/* 2013/6/14 Jin: manual implementation of GSSQ
 *
 *  00000001200009c0 <atomic_store128>:
 *     1200009c0:   0085202d        daddu   a0, a0, a1
 *     1200009c4:   e8860027        gssq    a2, a3, 0(a0)
 *     1200009c8:   03e00008        jr      ra
 *     1200009cc:   00000000        nop
 */
typedef void (* atomic_store128_ptr)(long *addr, int offset, long low64, long hi64);

static int *buf;

static atomic_store128_ptr get_atomic_store128_func()
{
  static atomic_store128_ptr p = NULL;
  if (p != NULL)
    return p;

  buf = (int *)mmap(NULL, 1024, PROT_READ | PROT_WRITE | PROT_EXEC, MAP_PRIVATE | MAP_ANONYMOUS,
                       -1, 0);
  buf[0] = 0x0085202d;
  buf[1] = (0x3a << 26) | (4 << 21) | (6 << 16) | 0x27;   /* gssq $a2, $a3, 0($a0) */
  buf[2] = 0x03e00008;
  buf[3] = 0;

  p = (atomic_store128_ptr)buf;
  return p;
}

void  NativeCall::set_destination(address dest) {
#ifndef _LP64
      OrderAccess::fence();
      set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_high((intptr_t)dest) & 0xffff));
      set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)dest) & 0xffff));
      ICache::invalidate_range(addr_at(0), 8);
#else
      OrderAccess::fence();
  /* 2013/6/13 Jin: ensure 100% atomicity */
  guarantee(!os::is_MP() || (((long)addr_at(0) % 16) == 0), "destination must be aligned for GSSD");

  /* li64 or li48 */
  if (is_special_op(int_at(16), Assembler::dsll_op)) {
      int first_word = int_at(0);
      set_int_at(0, 0x1000ffff); /* .1: b .1 */
      set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 32) & 0xffff));
      set_int_at(12, (int_at(12) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 16) & 0xffff));
      set_int_at(20, (int_at(20) & 0xffff0000) | (Assembler::split_low((intptr_t)dest) & 0xffff));
      set_int_at(0, (first_word & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 48) & 0xffff));
      ICache::invalidate_range(addr_at(0), 24);
  } else if (is_special_op(int_at(16), Assembler::jalr_op)) {
      int insts[4];
      insts[0] = (int_at(0) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 32) & 0xffff);
      insts[1] = (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 16) & 0xffff);
      insts[2] = int_at(8);
      insts[3] = (int_at(12) & 0xffff0000) | (Assembler::split_low((intptr_t)dest) & 0xffff);

      atomic_store128_ptr func = get_atomic_store128_func();
      (*func)((long *)addr_at(0), 0, *(long *)&insts[0], *(long *)&insts[2]);
  } else {
      fatal("not a call");
  }
#endif
}

void NativeCall::print() {
  tty->print_cr(PTR_FORMAT ": call " PTR_FORMAT,
                instruction_address(), destination());
}

// Inserts a native call instruction at a given pc
void NativeCall::insert(address code_pos, address entry) {
  NativeCall *call = nativeCall_at(code_pos);
  CodeBuffer cb(call->addr_at(0), instruction_size);
  MacroAssembler masm(&cb);
#define __ masm.
#ifndef _LP64
  __ lui(T9, Assembler::split_high((int)entry));
  __ addiu(T9, T9, Assembler::split_low((int)entry));
#else
  __ li48(T9, (long)entry);
#endif
  __ jalr ();
  __ delayed()->nop();
#undef __

  ICache::invalidate_range(call->addr_at(0), instruction_size);
}

// MT-safe patching of a call instruction.
// First patches first word of instruction to two jmp's that jmps to them
// selfs (spinlock). Then patches the last byte, and then atomicly replaces
// the jmp's with the first 4 byte of the new instruction.
void NativeCall::replace_mt_safe(address instr_addr, address code_buffer) {
	Unimplemented();
}

//-------------------------------------------------------------------

void NativeMovConstReg::verify() {
#ifndef _LP64
  if ( !is_op(Assembler::lui_op) || 
	!is_op(int_at(4), Assembler::addiu_op) )
    fatal("not a mov reg, imm32")
#else
  /* li64 or li48 */
  int li_64 = 0;
  int li_48 = 0;

  if ( is_op(Assembler::lui_op) &&
	is_op(int_at(4), Assembler::ori_op) &&
	is_special_op(int_at(8), Assembler::dsll_op) &&
	is_op(int_at(12), Assembler::ori_op) &&
	is_special_op(int_at(16), Assembler::dsll_op) &&
	is_op(int_at(20), Assembler::ori_op) )
	{
      li_64 = 1;
  }

  if (  is_op(Assembler::lui_op) &&
	  is_op	(int_at(4), Assembler::ori_op) &&
	  is_special_op(int_at(8), Assembler::dsll_op) &&
	  is_op	(int_at(12), Assembler::ori_op) ) {
      li_48 = 1;
  }

  if (!li_64 && !li_48) {
    fatal("not a mov reg, imm64/imm48");
  }
#endif
}

void NativeMovConstReg::print() {
  tty->print_cr(PTR_FORMAT ": mov reg, " INTPTR_FORMAT,
              	instruction_address(), data());
}

intptr_t NativeMovConstReg::data() const { 
#ifndef _LP64
  return Assembler::merge(int_at(4)&0xffff, long_at(0)&0xffff); 
#else
  /* li64 or li48 */
  if (is_special_op(int_at(16), Assembler::dsll_op) && is_op(long_at(20), Assembler::ori_op)) {
    return Assembler::merge( (intptr_t)(int_at(20) & 0xffff), 
				    (intptr_t)(int_at(12) & 0xffff),
				    (intptr_t)(int_at(4) & 0xffff),
				    (intptr_t)(int_at(0) & 0xffff));
  } else {
    return Assembler::merge( (intptr_t)(int_at(12) & 0xffff), 
				    (intptr_t)(int_at(4) & 0xffff),
				    (intptr_t)(int_at(0) & 0xffff),
				    (intptr_t)0);
  }
#endif
}

void NativeMovConstReg::set_data(intptr_t x) {
/*
#ifndef CORE
  // also store the value into an oop_Relocation cell, if any
  CodeBlob* cb = CodeCache::find_blob(instruction_address());
  nmethod*  nm = cb ? cb->as_nmethod_or_null() : NULL;
  if (nm != NULL) {
    RelocIterator iter(nm, instruction_address(), instruction_address() + 1); 
    oop* oop_addr = NULL;
    while (iter.next()) {
      if (iter.type() == relocInfo::oop_type) {
	oop_Relocation *r = iter.oop_reloc();
	if (oop_addr == NULL && r->oop_index()!=0) {
	  oop_addr = r->oop_addr();
	  *oop_addr = (oop)x;
	} else {
	  assert(oop_addr == r->oop_addr(), "must be only one set-oop here");
	}   
      }   
    }   
  }
#endif
*/

#ifndef _LP64
  set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_high(x) & 0xffff));
  set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low(x) & 0xffff));
  ICache::invalidate_range(addr_at(0), 8); 
#else
  /* li64 or li48 */
  if (is_special_op(int_at(16), Assembler::dsll_op) && is_op(long_at(20), Assembler::ori_op)) {
    set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_low((intptr_t)x >> 48) & 0xffff));
    set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)x >> 32) & 0xffff));
    set_int_at(12, (int_at(12) & 0xffff0000) | (Assembler::split_low((intptr_t)x >> 16) & 0xffff));
    set_int_at(20, (int_at(20) & 0xffff0000) | (Assembler::split_low((intptr_t)x) & 0xffff));
  } else {
      //assert(is_simm16(dest >> 32), "Not a 48-bit address");
      set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_low((intptr_t)x >> 32) & 0xffff));
      set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)x >> 16) & 0xffff));
      set_int_at(12, (int_at(12) & 0xffff0000) | (Assembler::split_low((intptr_t)x) & 0xffff));
  }
  ICache::invalidate_range(addr_at(0), 24);
#endif
}

//-------------------------------------------------------------------

int NativeMovRegMem::offset() const{
  if (is_immediate()) 
    return (short)(int_at(instruction_offset)&0xffff);
  else 
    return Assembler::merge(int_at(hiword_offset)&0xffff, long_at(instruction_offset)&0xffff);
}

void NativeMovRegMem::set_offset(int x) {
  if (is_immediate()) {
    assert(Assembler::is_simm16(x), "just check");
    set_int_at(0, (int_at(0)&0xffff0000) | (x&0xffff) );
    if (is_64ldst()) {
      assert(Assembler::is_simm16(x+4), "just check");
			set_int_at(4, (int_at(4)&0xffff0000) | ((x+4)&0xffff) );
		}
  } else {
    set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_high(x) & 0xffff));
    set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low(x) & 0xffff));
  }
  ICache::invalidate_range(addr_at(0), 8);
}

void NativeMovRegMem::verify() {
  int offset = 0;

  if ( Assembler::opcode(int_at(0)) == Assembler::lui_op ) {
#ifndef _LP64
    if ( (Assembler::opcode(int_at(4)) != Assembler::addiu_op) ||
	(Assembler::opcode(int_at(8)) != Assembler::special_op) || 
	(Assembler::special(int_at(8)) != Assembler::add_op))
#else
      /* Jin: fit MIPS64 */
      if ( (Assembler::opcode(int_at(4)) != Assembler::addiu_op && 
	    Assembler::opcode(int_at(4)) != Assembler::daddiu_op ) ||
	  (Assembler::opcode(int_at(8)) != Assembler::special_op) || 
	  (Assembler::special(int_at(8)) != Assembler::add_op
	   && Assembler::special(int_at(8)) != Assembler::dadd_op))
#endif
	fatal ("not a mov [reg+offs], reg instruction");
    offset += 12;
  }

  switch(Assembler::opcode(int_at(offset))) {
	case Assembler::lb_op:
	case Assembler::lbu_op:
	case Assembler::lh_op:
	case Assembler::lhu_op:
	case Assembler::lw_op:
	LP64_ONLY(case Assembler::ld_op:)
	case Assembler::lwc1_op:
	LP64_ONLY(case Assembler::ldc1_op:)
	case Assembler::sb_op:
	case Assembler::sh_op:
	case Assembler::sw_op:
	LP64_ONLY(case Assembler::sd_op:)
	case Assembler::swc1_op:
	LP64_ONLY(case Assembler::sdc1_op:)
		break;
	default:
		fatal ("not a mov [reg+offs], reg instruction");
	}
}


void NativeMovRegMem::print() {
  tty->print_cr("0x%x: mov reg, [reg + %x]", instruction_address(), offset());
}



void NativeIllegalInstruction::insert(address code_pos) {
  CodeBuffer cb(code_pos, instruction_size);
  MacroAssembler masm(&cb);
#define __ masm.
  __ brk(11);
#undef __

  ICache::invalidate_range(code_pos, instruction_size);
}

void NativeGeneralJump::verify() {
  assert(((NativeInstruction *)this)->is_jump() ||
         ((NativeInstruction *)this)->is_cond_jump(), "not a general jump instruction");
}


void  NativeGeneralJump::set_jump_destination(address dest) {
//tty->print_cr("NativeGeneralJump::set_jump_destination dest=%lx", dest);
  OrderAccess::fence();

  if (is_short()) {
    assert(Assembler::is_simm16(dest-addr_at(4)), "change this code");
    set_int_at(0, (int_at(0) & 0xffff0000) | (dest - addr_at(4)) & 0xffff );
    ICache::invalidate_range(addr_at(0), 4);
#ifdef _LP64
  } else if (is_b_far()) {
    int offset = dest - addr_at(12);
    set_int_at(12, (int_at(12) & 0xffff0000) | (offset >> 16));
    set_int_at(16, (int_at(16) & 0xffff0000) | (offset & 0xffff));
#endif
  } else {
#ifndef _LP64
    set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_high((intptr_t)dest) & 0xffff));
    set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)dest) & 0xffff));
    ICache::invalidate_range(addr_at(0), 8);
#else
  /* li64 or li48 */
  if (is_special_op(int_at(16), Assembler::dsll_op)) {
    set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 48) & 0xffff));
    set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 32) & 0xffff));
    set_int_at(12, (int_at(12) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 16) & 0xffff));
    set_int_at(20, (int_at(20) & 0xffff0000) | (Assembler::split_low((intptr_t)dest) & 0xffff));
  } else {
    int jr_word = int_at(16);
    set_int_at(16, 0x1000fffb); /* .1: --; --; --; --; b .1; nop */

    set_int_at(0, (int_at(0) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 32) & 0xffff));
    set_int_at(4, (int_at(4) & 0xffff0000) | (Assembler::split_low((intptr_t)dest >> 16) & 0xffff));
    set_int_at(12, (int_at(12) & 0xffff0000) | (Assembler::split_low((intptr_t)dest) & 0xffff));
    set_int_at(16, jr_word);    /* .1: --; --; --; --; jr ; nop */
  }

  ICache::invalidate_range(addr_at(0), 24);
#endif
  }
}

// we now use b to do this. be careful when using this method
// by yjl 9/16/2005
void NativeGeneralJump::insert_unconditional(address code_pos, address entry) {
  CodeBuffer cb(code_pos, instruction_size);
  MacroAssembler masm(&cb);
#define __ masm. 
#ifdef _LP64
  if (Assembler::is_simm16((entry - code_pos - 4) / 4))
  {
    __ b(entry);
    __ delayed()->nop();
  }
  else
  {
    /* a simplified b_far */
    int offset = entry - code_pos;

    // FIXME: need to preserve RA?
    __ emit_long(0x4110001); //__ emit_long(Assembler::insn_ORRI(Assembler::regimm_op, 0, Assembler::bgezal_op, 1));
    __ lui(T9, (offset - 8) >> 16);	// delay slot
    __ ori(T9, T9, (offset - 8) & 0xffff);
    __ daddu(T9, T9, RA);
    __ jr(T9);
    __ nop();
  }
#else
  __ b(entry);
  __ delayed()->nop();
#endif
#undef __

  ICache::invalidate_range(code_pos, instruction_size);
}

#ifdef _LP64
bool NativeGeneralJump::is_b_far() {
/*
   0x000000556809f198: dadd at, ra, zero
   0x000000556809f19c: [4110001]bgezal zero, 0x000000556809f1a4

   0x000000556809f1a0: nop
   0x000000556809f1a4: lui t9, 0xfffffffd
   0x000000556809f1a8: ori t9, t9, 0x14dc 
   0x000000556809f1ac: daddu t9, t9, ra 
   0x000000556809f1b0: dadd ra, at, zero
   0x000000556809f1b4: jr t9
   0x000000556809f1b8: nop
  ;; ImplicitNullCheckStub slow case
   0x000000556809f1bc: lui t9, 0x55
 */
  return is_op(int_at(12), Assembler::lui_op);
}
#endif

address NativeGeneralJump::jump_destination() {
  if ( is_short() ) {
    return addr_at(4) + Assembler::imm_off(int_at(instruction_offset)) * 4;
  }
#ifndef _LP64
  return (address)Assembler::merge(int_at(4)&0xffff, long_at(instruction_offset)&0xffff);
#else
  /* 2012/4/19 Jin: Assembler::merge() is not correct in MIPS_64!

     Example:
       hi16 = 0xfffd,
       lo16 = f7a4,
       
       offset=0xfffdf7a4 (Right)
       Assembler::merge = 0xfffcf7a4 (Wrong)
    */
  if ( is_b_far() ) {
    int hi16 = int_at(12)&0xffff;
    int low16 = int_at(16)&0xffff;
    address target = addr_at(12) + (hi16 << 16) + low16;
    return target;
  }

  /* li64 or li48 */
  if (is_special_op(int_at(16), Assembler::dsll_op)) {
    return (address)Assembler::merge( (intptr_t)(int_at(20) & 0xffff), 
				      (intptr_t)(int_at(12) & 0xffff),
				      (intptr_t)(int_at(4) & 0xffff),
				      (intptr_t)(int_at(0) & 0xffff));
  } else {
    return (address)Assembler::merge( (intptr_t)(int_at(12) & 0xffff), 
				    (intptr_t)(int_at(4) & 0xffff),
				    (intptr_t)(int_at(0) & 0xffff),
				    ((int_at(0) & 0xffff) >= 0x8000) ? (intptr_t)0xffff : (intptr_t)0); /* sign-extended to 64-bit*/
  }
#endif
}

// MT-safe patching of a long jump instruction.
// First patches first word of instruction to two jmp's that jmps to them
// selfs (spinlock). Then patches the last byte, and then atomicly replaces
// the jmp's with the first 4 byte of the new instruction.
void NativeGeneralJump::replace_mt_safe(address instr_addr, address code_buffer) {
	NativeGeneralJump* h_jump =  nativeGeneralJump_at (instr_addr);
  assert(NativeGeneralJump::instruction_size == NativeCall::instruction_size, 
          "note::Runtime1::patch_code uses NativeCall::instruction_size");

  /* 2013/6/13 Jin: ensure 100% atomicity */
  guarantee(!os::is_MP() || (((long)instr_addr % BytesPerWord) == 0), "destination must be aligned for SD");

  int *p = (int *)instr_addr;
  int jr_word = p[4];

  p[4] = 0x1000fffb;   /* .1: --; --; --; --; b .1; nop */
  memcpy(instr_addr, code_buffer, NativeCall::instruction_size - 8);
  *(long *)(instr_addr + 16) = *(long *)(code_buffer + 16);
}

/* Must ensure atomicity */
void NativeGeneralJump::patch_verified_entry(address entry, address verified_entry, address dest) {
    /* 2013/11/5 Jin: ensure 100% atomicity.
     * The destination is fixed and can be cached in JavaThread.
     */
    guarantee(!os::is_MP() || (((long)verified_entry % BytesPerWord) == 0), "destination must be aligned for SD");

    int code_buffer[4];

    CodeBuffer cb((address)code_buffer, instruction_size);
    MacroAssembler masm(&cb);
#define __ masm.
    __ ld(T9, TREG, in_bytes(JavaThread::handle_wrong_method_stub_offset()));
    __ jr(T9);
    __ delayed()->nop();
    __ nop();

    atomic_store128_ptr func = get_atomic_store128_func();
    (*func)((long *)verified_entry, 0, *(long *)&code_buffer[0], *(long *)&code_buffer[2]);

    ICache::invalidate_range(verified_entry, instruction_size);
}

bool NativeInstruction::is_jump()
{ 
#ifndef _LP64
  return ((int_at(0) & NativeGeneralJump::b_mask) == NativeGeneralJump::beq_opcode) ||
          (is_op(int_at(0), Assembler::lui_op) &&
          is_op(int_at(4), Assembler::addiu_op) &&
          is_special_op(int_at(8), Assembler::jr_op)); 
#else
//    		    lui   rd, imm(63...48);
//		    ori   rd, rd, imm(47...32);
//		    dsll  rd, rd, 16;
//		    ori   rd, rd, imm(31...16);
//		    dsll  rd, rd, 16;
//		    ori   rd, rd, imm(15...0);
//		    jalr  rd
//      	    nop
//		    
  if ((int_at(0) & NativeGeneralJump::b_mask) == NativeGeneralJump::beq_opcode)
    return true;
  if (is_op(int_at(4), Assembler::lui_op)) /* simplified b_far */
    return true;
  if (is_op(int_at(12), Assembler::lui_op)) /* original b_far */
    return true;
  if (is_op(int_at(0), Assembler::lui_op) &&
          is_op(int_at(4), Assembler::ori_op) &&
          is_special_op(int_at(8), Assembler::dsll_op) &&
          is_op(int_at(12), Assembler::ori_op) &&
          is_special_op(int_at(16), Assembler::dsll_op) &&
          is_op(int_at(20), Assembler::ori_op))
    return true;
  if (is_op(int_at(0), Assembler::lui_op) &&
          is_op(int_at(4), Assembler::ori_op) &&
          is_special_op(int_at(8), Assembler::dsll_op) &&
          is_op(int_at(12), Assembler::ori_op)) 
    return true;
  return false;
#endif
}

bool NativeInstruction::is_dtrace_trap() {
  //return (*(int32_t*)this & 0xff) == 0xcc;
	Unimplemented();
	return false;
}

			// is mips we have to use two instruction to poll, however, we don't want to bother checking two instructions
			// instead, we use a lw $0, at() as the second instruction, and only check this.
			// change ZERO -> AT, only in godson-2e @jerome,11/25/2006
bool NativeInstruction::is_safepoint_poll() {
#ifdef _LP64
/*
   0x0000005565d28868: lui t2, 0x0         ; -24
   0x0000005565d2886c: ori t2, t2, 0x55    ; -20
   0x0000005565d28870: dsll t2, t2, 16     ; -16
   0x0000005565d28874: ori t2, t2, 0x6428  ; -12
   0x0000005565d28878: dsll t2, t2, 16     ; -8
   0x0000005565d2887c: ori t2, t2, 0x100   ; -4
   0x0000005565d28880: lw at, 0x0(t2)    <-- PC
 */
  #ifndef OPT_SAFEPOINT
  /* li64 or li48 */
  if (is_op(Assembler::lw_op) && is_rt(AT)) {
    return true;
  } else if (is_special_op(long_at(-16), Assembler::dsll_op)) {
    /* li64 */
    return (is_op(int_at(-24), Assembler::lui_op) && 
         is_op(int_at(-20), Assembler::ori_op) && 
         is_special_op(int_at(-16), Assembler::dsll_op) && 
         is_op(int_at(-12), Assembler::ori_op) && 
         is_special_op(int_at(-8), Assembler::dsll_op) && 
         is_op(int_at(-4), Assembler::ori_op) && 
         is_op(Assembler::lw_op) && 
         is_rt(AT));
  } else if (is_op(int_at(-16), Assembler::lui_op)) {
    /* li48 */
    return is_op(int_at(-16), Assembler::lui_op) && 
         is_op(int_at(-12), Assembler::ori_op) && 
         is_special_op(int_at(-8), Assembler::dsll_op) && 
         is_op(int_at(-4), Assembler::ori_op) && 
         is_op(Assembler::lw_op) && 
         is_rt(AT);
  } else {
    return false;
  }
  #else // OPT_SAFEPOINT
  return is_op(int_at(-4), Assembler::lui_op) && 
         is_op(Assembler::lw_op) && 
         is_rt(AT);
  #endif
#else
  return is_op(int_at(-4), Assembler::lui_op) && 
         is_op(Assembler::lw_op) && 
         is_rt(AT);
#endif
}
