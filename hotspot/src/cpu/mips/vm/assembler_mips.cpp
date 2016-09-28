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
#include "asm/assembler.hpp"
#include "asm/assembler.inline.hpp"
#include "gc_interface/collectedHeap.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "memory/cardTableModRefBS.hpp"
#include "memory/resourceArea.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/interfaceSupport.hpp"
#include "runtime/objectMonitor.hpp"
#include "runtime/os.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#ifndef SERIALGC
#include "gc_implementation/g1/g1CollectedHeap.inline.hpp"
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#include "gc_implementation/g1/heapRegion.hpp"
#endif
#ifdef PRODUCT
#define BLOCK_COMMENT(str) /* nothing */
#define STOP(error) stop(error)
#else
#define BLOCK_COMMENT(str) block_comment(str)
#define STOP(error) block_comment(error); stop(error)
#endif

#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")

intptr_t MacroAssembler::i[32] = {0};
float MacroAssembler::f[32] = {0.0};

void MacroAssembler::print(outputStream *s) {
	unsigned int k;
	for(k=0; k<sizeof(i)/sizeof(i[0]); k++) {
		s->print_cr("i%d = 0x%.16lx", k, i[k]);
	}
	s->cr();

	for(k=0; k<sizeof(f)/sizeof(f[0]); k++) {
		s->print_cr("f%d = %f", k, f[k]); 
	}
	s->cr();
}


int MacroAssembler::i_offset(unsigned int k) { return (intptr_t)&((MacroAssembler*)0)->i[k]; }
int MacroAssembler::f_offset(unsigned int k) { return (intptr_t)&((MacroAssembler*)0)->f[k]; }
	
void MacroAssembler::save_registers(MacroAssembler *masm) {
#define __ masm->
	for(int k=0; k<32; k++) {
		__ sw (as_Register(k), A0, i_offset(k));
	}
	
	for(int k=0; k<32; k++) {
		__ swc1 (as_FloatRegister(k), A0, f_offset(k));
	}
#undef __
}

void MacroAssembler::restore_registers(MacroAssembler *masm) {
#define __ masm->
	for(int k=0; k<32; k++) {
		__ lw (as_Register(k), A0, i_offset(k));
	}
		
	for(int k=0; k<32; k++) {
		__ lwc1 (as_FloatRegister(k), A0, f_offset(k));
	}
#undef __
}


// Implementation of AddressLiteral

AddressLiteral::AddressLiteral(address target, relocInfo::relocType rtype) {
  _is_lval = false;
  _target = target;
  _rspec = rspec_from_rtype(rtype, target);
}

// Implementation of Address

//FIXME aoqi
//#ifdef _LP64
#if 0

Address Address::make_array(ArrayAddress adr) {
  // Not implementable on 64bit machines
  // Should have been handled higher up the call chain.
  ShouldNotReachHere();
  return Address();
}

// exceedingly dangerous constructor
Address::Address(int disp, address loc, relocInfo::relocType rtype) {
  _base  = noreg;
  _index = noreg;
  _scale = no_scale;
  _disp  = disp;
  switch (rtype) {
    case relocInfo::external_word_type:
      _rspec = external_word_Relocation::spec(loc);
      break;
    case relocInfo::internal_word_type:
      _rspec = internal_word_Relocation::spec(loc);
      break;
    case relocInfo::runtime_call_type:
      // HMM
      _rspec = runtime_call_Relocation::spec();
      break;
    case relocInfo::poll_type:
    case relocInfo::poll_return_type:
      _rspec = Relocation::spec_simple(rtype);
      break;
    case relocInfo::none:
      break;
    default:
      ShouldNotReachHere();
  }
}
#else // LP64

Address Address::make_array(ArrayAddress adr) {
  AddressLiteral base = adr.base();
  Address index = adr.index();
  assert(index._disp == 0, "must not have disp"); // maybe it can?
  Address array(index._base, index._index, index._scale, (intptr_t) base.target());
  array._rspec = base._rspec;
  return array;
}

// exceedingly dangerous constructor
Address::Address(address loc, RelocationHolder spec) {
  _base  = noreg;
  _index = noreg;
  _scale = no_scale;
  _disp  = (intptr_t) loc;
  _rspec = spec;
}

#endif // _LP64


/*
// Convert the raw encoding form into the form expected by the constructor for
// Address.  An index of 4 (rsp) corresponds to having no index, so convert
// that to noreg for the Address constructor.
Address Address::make_raw(int base, int index, int scale, int disp) {
  bool valid_index = index != rsp->encoding();
  if (valid_index) {
    Address madr(as_Register(base), as_Register(index), (Address::ScaleFactor)scale, in_ByteSize(disp));
    return madr;
  } else {
    Address madr(as_Register(base), noreg, Address::no_scale, in_ByteSize(disp));
    return madr;
  }
}
*/

// Implementation of Assembler
const char *Assembler::ops_name[] = {
	"special",  "regimm",   "j",      "jal",    "beq",      "bne",      "blez",   "bgtz",
	"addi",     "addiu",    "slti",   "sltiu",  "andi",     "ori",      "xori",   "lui",
	"cop0",     "cop1",     "cop2",   "cop3",   "beql",     "bnel",     "bleql",  "bgtzl",
	"daddi",    "daddiu",   "ldl",    "ldr",    "",         "",         "",       "",
	"lb",       "lh",       "lwl",    "lw",     "lbu",      "lhu",      "lwr",    "lwu",
	"sb",       "sh",       "swl",    "sw",     "sdl",      "sdr",      "swr",    "cache",
	"ll",       "lwc1",     "",       "",       "lld",      "ldc1",     "",       "ld",
	"sc",       "swc1",     "",       "",       "scd",      "sdc1",     "",       "sd"
};

const char* Assembler::special_name[] = {
	"sll",      "",         "srl",      "sra",      "sllv",     "",         "srlv",     "srav",
	"jr",       "jalr",     "movz",     "movn",     "syscall",  "break",    "",         "sync",
	"mfhi",     "mthi",     "mflo",     "mtlo",     "dsll",     "",         "dsrl",     "dsra",
	"mult",     "multu",    "div",      "divu",     "dmult",    "dmultu",   "ddiv",     "ddivu",
	"add",      "addu",     "sub",      "subu",     "and",      "or",       "xor",      "nor",
	"",         "",         "slt",      "sltu",     "dadd",     "daddu",    "dsub",     "dsubu",
	"tge",      "tgeu",     "tlt",      "tltu",     "teq",      "",         "tne",      "",
	"dsll",     "",         "dsrl",     "dsra",     "dsll32",   "",         "dsrl32",   "dsra32"
};

const char* Assembler::special2_name[] = {
	"madd",     "",         "mul",      "",         "msub",     "",         "",         "",
	"",         "",         "",         "",         "",         "",         "",         "",
	"",         "gsdmult",  "",         "",         "gsdiv",    "gsddiv",   "",         "",
	"",         "",         "",         "",         "gsmod",    "gsdmod",   "",         "",
	"",         "",         "",         "",         "",         "",         "",         "",
	"",         "",         "",         "",         "",         "",         "",         "",
	"",         "",         "",         "",         "",         "",         "",         "",
	"",         "",         "",         "",         "",         "",         "",         ""
};

const char* Assembler::regimm_name[] = {
	"bltz",     "bgez",     "bltzl",    "bgezl",    "",         "",         "",         "",
	"tgei",     "tgeiu",    "tlti",     "tltiu",    "teqi",     "",         "tnei",     "",
	"bltzal",   "bgezal",   "bltzall",  "bgezall"
};
	
const char* Assembler::float_name[] = {
	"add",			"sub",			"mul",			"div",			"sqrt",			"abs",			"mov",			"neg",
	"round.l",	"trunc.l",	"ceil.l",		"floor.l",	"round.w",  "trunc.w",	"ceil.w",		"floor.w"
};

const char* Assembler::gs_ldc2_name[] = {
	"gslbx",    "gslhx",    "gslwx",    "gsldx",    "",         "",         "gslwxc1",  "gsldxc1"
};

const char* Assembler::gs_sdc2_name[] = {
	"gssbx",    "gsshx",    "gsswx",    "gssdx",    "",         "",         "gsswxc1",  "gssdxc1"
};

//misleading name, print only branch/jump instruction 
void Assembler::print_instruction(int inst) {
	const char *s;
	switch( opcode(inst) ) {
	default:
		s = ops_name[opcode(inst)];
		break;
	case special_op:
		s = special_name[special(inst)];
		break;
	case regimm_op:
		s = special_name[rt(inst)];
		break;
	}

	::tty->print("%s", s);
}

void MacroAssembler::pd_patch_instruction(address branch, address target) {
  jint& stub_inst = *(jint*) branch;

/* *
	move(AT, RA); // dadd
	emit_long(insn_ORRI(regimm_op, 0, bgezal_op, 1));
	nop();
        lui(T9, 0); // to be patched
        ori(T9, 0);
	daddu(T9, T9, RA);
	move(RA, AT);
	jr(T9);
 */
  if(special(stub_inst) == dadd_op) {
    jint *pc = (jint *)branch;

    assert(opcode(pc[3]) == lui_op
          && opcode(pc[4]) == ori_op
          && special(pc[5]) == daddu_op, "Not a branch label patch");
    if(!(opcode(pc[3]) == lui_op
          && opcode(pc[4]) == ori_op
          && special(pc[5]) == daddu_op)) { tty->print_cr("Not a branch label patch"); }

    int offset = target - branch;
    if (!is_simm16(offset))
    {
      pc[3] = (pc[3] & 0xffff0000) | high16(offset - 12);
      pc[4] = (pc[4] & 0xffff0000) | low16(offset - 12);
    }
    else
    {
      /* revert to "beq + nop" */
      CodeBuffer cb(branch, 4 * 10);
      MacroAssembler masm(&cb);
#define __ masm.
      __ b(target);
      __ nop();
      __ nop();
      __ nop();
      __ nop();
      __ nop();
      __ nop();
      __ nop();
    }
    return;
  }

#ifndef PRODUCT
  if (!is_simm16((target - branch - 4) >> 2))
  {
    tty->print_cr("Illegal patching: target=0x%lx", target);
    int *p = (int *)branch;
    for (int i = -10; i < 10; i++)
    {
       tty->print("0x%lx, ", p[i]);
    }
    tty->print_cr("");
  }
#endif

  stub_inst = patched_branch(target - branch, stub_inst, 0);
}

//without check, maybe fixed
int Assembler::patched_branch(int dest_pos, int inst, int inst_pos) {
	int v = (dest_pos - inst_pos - 4)>>2;
	switch(opcode(inst)) {
	case j_op:
	case jal_op:
		assert(false, "should not use j/jal here");
		break;
	default:
		assert(is_simm16(v), "must be simm16");
#ifndef PRODUCT
		if(!is_simm16(v))
		{ 
			tty->print_cr("must be simm16");
			tty->print_cr("Inst: %lx", inst);
		}
#endif
			
		v = low16(v);
		inst &= 0xffff0000;
		break;
	}

	return inst | v;
}

int Assembler::branch_destination(int inst, int pos) {
	int off;
	
	switch(opcode(inst)) {
	case j_op:
	case jal_op:
		assert(false, "should not use j/jal here");
		break;
	default:
		off = expand(low16(inst), 15);
		break;
	}
	
	return off ? pos + 4 + (off<<2) : 0;
}

int AbstractAssembler::code_fill_byte() {
	  return 0x00;                  // illegal instruction 0x00000000
}

// Now the Assembler instruction (identical for 32/64 bits)

void Assembler::lb(Register rt, Address src) {
	lb(rt, src.base(), src.disp());
}

void Assembler::lbu(Register rt, Address src) {
	lbu(rt, src.base(), src.disp());
}

void Assembler::ld(Register rt, Address src){
	ld(rt, src.base(), src.disp());
}

void Assembler::ldl(Register rt, Address src){
	ldl(rt, src.base(), src.disp());
}

void Assembler::ldr(Register rt, Address src){
	ldr(rt, src.base(), src.disp());
}

void Assembler::lh(Register rt, Address src){
	lh(rt, src.base(), src.disp());
}

void Assembler::lhu(Register rt, Address src){
	lhu(rt, src.base(), src.disp());
}

void Assembler::ll(Register rt, Address src){
	ll(rt, src.base(), src.disp());
}

void Assembler::lld(Register rt, Address src){
	lld(rt, src.base(), src.disp());
}

void Assembler::lw(Register rt, Address src){
	lw(rt, src.base(), src.disp());
}
void Assembler::lea(Register rt, Address src) {
#ifdef _LP64
  daddi(rt, src.base(), src.disp());
#else
  addi(rt, src.base(), src.disp());
#endif
}

void Assembler::lwl(Register rt, Address src){
	lwl(rt, src.base(), src.disp());
}

void Assembler::lwr(Register rt, Address src){
	lwr(rt, src.base(), src.disp());
}

void Assembler::lwu(Register rt, Address src){
	lwu(rt, src.base(), src.disp());
}

void Assembler::sb(Register rt, Address dst) {
	sb(rt, dst.base(), dst.disp());
}

void Assembler::sc(Register rt, Address dst) {
	sc(rt, dst.base(), dst.disp());
}

void Assembler::scd(Register rt, Address dst) {
	scd(rt, dst.base(), dst.disp());
}

void Assembler::sd(Register rt, Address dst) {
	sd(rt, dst.base(), dst.disp());
}

void Assembler::sdl(Register rt, Address dst) {
	sdl(rt, dst.base(), dst.disp());
}

void Assembler::sdr(Register rt, Address dst) {
	sdr(rt, dst.base(), dst.disp());
}

void Assembler::sh(Register rt, Address dst) {
	sh(rt, dst.base(), dst.disp());
}

void Assembler::sw(Register rt, Address dst) {
	sw(rt, dst.base(), dst.disp());
}

void Assembler::swl(Register rt, Address dst) {
	swl(rt, dst.base(), dst.disp());
}

void Assembler::swr(Register rt, Address dst) {
	swr(rt, dst.base(), dst.disp());
}

void Assembler::lwc1(FloatRegister rt, Address src) {
	lwc1(rt, src.base(), src.disp());
}

void Assembler::ldc1(FloatRegister rt, Address src) {
	ldc1(rt, src.base(), src.disp());
}

void Assembler::swc1(FloatRegister rt, Address dst) {
	swc1(rt, dst.base(), dst.disp());
}

void Assembler::sdc1(FloatRegister rt, Address dst) {
	sdc1(rt, dst.base(), dst.disp());
}

void Assembler::j(address entry) {
	int dest = ((intptr_t)entry - (((intptr_t)pc() + 4) & 0xf0000000))>>2;
	emit_long((j_op<<26) | dest); 
	has_delay_slot(); 
}

void Assembler::jal(address entry) {
	int dest = ((intptr_t)entry - (((intptr_t)pc() + 4) & 0xf0000000))>>2;
	emit_long((jal_op<<26) | dest); 
	has_delay_slot(); 
}

void MacroAssembler::beq_far(Register rs, Register rt, address entry)
{
  u_char * cur_pc = pc();

  /* Jin: Near/Far jump */
  if(is_simm16((entry - pc() - 4) / 4))
  {
    Assembler::beq(rs, rt, offset(entry));
  }
  else
  {
    Label not_jump;
    bne(rs, rt, not_jump);
    delayed()->nop();

    b_far(entry); 
    delayed()->nop();

    bind(not_jump);
    has_delay_slot();
  }
}

void MacroAssembler::beq_far(Register rs, Register rt, Label& L)
{
  if (L.is_bound()) {
    beq_far(rs, rt, target(L));
  } else {
    u_char * cur_pc = pc();
    Label not_jump;
    bne(rs, rt, not_jump);
    delayed()->nop();

    b_far(L); 
    delayed()->nop();

    bind(not_jump);
    has_delay_slot();
  }
}

void MacroAssembler::bne_far(Register rs, Register rt, address entry)
{
  u_char * cur_pc = pc();

  /* Jin: Near/Far jump */
  if(is_simm16((entry - pc() - 4) / 4))
  {
    Assembler::bne(rs, rt, offset(entry));
  }
  else
  {
    Label not_jump;
    beq(rs, rt, not_jump);
    delayed()->nop();

    b_far(entry); 
    delayed()->nop();

    bind(not_jump);
    has_delay_slot();
  }
}

void MacroAssembler::bne_far(Register rs, Register rt, Label& L)
{
  if (L.is_bound()) {
    bne_far(rs, rt, target(L));
  } else {
    u_char * cur_pc = pc();
    Label not_jump;
    beq(rs, rt, not_jump);
    delayed()->nop();

    b_far(L); 
    delayed()->nop();

    bind(not_jump);
    has_delay_slot();
  }
}

void MacroAssembler::b_far(Label& L)
{
  if (L.is_bound()) {
    b_far(target(L));
  } else {
	volatile address dest = target(L);
/*
MacroAssembler::pd_patch_instruction branch=55651ed514, target=55651ef6d8
   0x00000055651ed514: dadd at, ra, zero
   0x00000055651ed518: [4110001]bgezal zero, 0x00000055651ed520

   0x00000055651ed51c: sll zero, zero, 0
   0x00000055651ed520: lui t9, 0x0
   0x00000055651ed524: ori t9, t9, 0x21b8
   0x00000055651ed528: daddu t9, t9, ra
   0x00000055651ed52c: dadd ra, at, zero
   0x00000055651ed530: jr t9
   0x00000055651ed534: sll zero, zero, 0
*/
	move(AT, RA);
	emit_long(insn_ORRI(regimm_op, 0, bgezal_op, 1));
	nop();
        lui(T9, 0); // to be patched
        ori(T9, T9, 0);
	daddu(T9, T9, RA);
	move(RA, AT);
	jr(T9);
  }
}

void MacroAssembler::b_far(address entry)
{ 
	u_char * cur_pc = pc();

	/* Jin: Near/Far jump */
	if(is_simm16((entry - pc() - 4) / 4))
	{
		b(offset(entry));
	}
	else
	{
		/* address must be bounded */
		move(AT, RA);
	 	emit_long(insn_ORRI(regimm_op, 0, bgezal_op, 1));
		nop();
		li32(T9, entry - pc());
		daddu(T9, T9, RA);
		move(RA, AT);
		jr(T9);
	}
}

// Implementation of MacroAssembler

// First all the versions that have distinct versions depending on 32/64 bit
// Unless the difference is trivial (1 line or so).

//#ifndef _LP64

// 32bit versions

void MacroAssembler::ld_ptr(Register rt, Register offset, Register base) {
  addu_long(AT, base, offset);
  ld_ptr(rt, 0, AT);
}

void MacroAssembler::st_ptr(Register rt, Register offset, Register base) {
  addu_long(AT, base, offset);
  st_ptr(rt, 0, AT);
}

void MacroAssembler::ld_long(Register rt, Register offset, Register base) {
  addu_long(AT, base, offset);
  ld_long(rt, 0, AT);
}

void MacroAssembler::st_long(Register rt, Register offset, Register base) {
  addu_long(AT, base, offset);
  st_long(rt, 0, AT);
}

Address MacroAssembler::as_Address(AddressLiteral adr) {
  return Address(adr.target(), adr.rspec());
}

Address MacroAssembler::as_Address(ArrayAddress adr) {
  return Address::make_array(adr);
}

// tmp_reg1 and tmp_reg2 should be saved outside of atomic_inc32 (caller saved).
void MacroAssembler::atomic_inc32(address counter_addr, int inc, Register tmp_reg1, Register tmp_reg2) {
  Label again;

  bind(again);
  sync();
  li(tmp_reg1, counter_addr);
  ll(tmp_reg2, tmp_reg1, 0);
  addi(tmp_reg2, tmp_reg2, inc);
  sc(tmp_reg2, tmp_reg1, 0);
  beq(tmp_reg2, R0, again);
  delayed()->nop();
}
int MacroAssembler::biased_locking_enter(Register lock_reg,
                                         Register obj_reg,
                                         Register swap_reg,
                                         Register tmp_reg,
                                         bool swap_reg_contains_mark,
                                         Label& done,
                                         Label* slow_case,
                                         BiasedLockingCounters* counters) {
  assert(UseBiasedLocking, "why call this otherwise?");
  bool need_tmp_reg = false;
  if (tmp_reg == noreg) {
    need_tmp_reg = true;
    tmp_reg = T9;
  }
  assert_different_registers(lock_reg, obj_reg, swap_reg, tmp_reg, AT);
  assert(markOopDesc::age_shift == markOopDesc::lock_bits + markOopDesc::biased_lock_bits, "biased locking makes assumptions about bit layout");
  Address mark_addr      (obj_reg, oopDesc::mark_offset_in_bytes());
  Address saved_mark_addr(lock_reg, 0);

  // Biased locking
  // See whether the lock is currently biased toward our thread and
  // whether the epoch is still valid
  // Note that the runtime guarantees sufficient alignment of JavaThread
  // pointers to allow age to be placed into low bits
  // First check to see whether biasing is even enabled for this object
  Label cas_label;
  int null_check_offset = -1;
  if (!swap_reg_contains_mark) {
    null_check_offset = offset();
    ld_ptr(swap_reg, mark_addr);
  }

  if (need_tmp_reg) {
    push(tmp_reg);
  }
  move(tmp_reg, swap_reg);
  andi(tmp_reg, tmp_reg, markOopDesc::biased_lock_mask_in_place);
#ifdef _LP64
  daddi(AT, R0, markOopDesc::biased_lock_pattern);
  dsub(AT, AT, tmp_reg);
#else
  addi(AT, R0, markOopDesc::biased_lock_pattern);
  sub(AT, AT, tmp_reg);
#endif
  if (need_tmp_reg) {
    pop(tmp_reg);
  }

  bne(AT, R0, cas_label);
  delayed()->nop();


  // The bias pattern is present in the object's header. Need to check
  // whether the bias owner and the epoch are both still current.
  // Note that because there is no current thread register on MIPS we
  // need to store off the mark word we read out of the object to
  // avoid reloading it and needing to recheck invariants below. This
  // store is unfortunate but it makes the overall code shorter and
  // simpler.
  st_ptr(swap_reg, saved_mark_addr);
  if (need_tmp_reg) {
    push(tmp_reg);
  }
  if (swap_reg_contains_mark) {
    null_check_offset = offset();
  }
  load_prototype_header(tmp_reg, obj_reg);
  xorr(tmp_reg, tmp_reg, swap_reg);
  get_thread(swap_reg);
  xorr(swap_reg, swap_reg, tmp_reg);

  move(AT, ~((int) markOopDesc::age_mask_in_place));
  andr(swap_reg, swap_reg, AT);

  if (PrintBiasedLockingStatistics) {
    Label L;
    bne(swap_reg, R0, L);
    delayed()->nop();
    atomic_inc32((address)BiasedLocking::biased_lock_entry_count_addr(), 1, AT, tmp_reg);
    bind(L);
  }
  if (need_tmp_reg) {
    pop(tmp_reg);
  }
  beq(swap_reg, R0, done);
  delayed()->nop();
  Label try_revoke_bias;
  Label try_rebias;

  // At this point we know that the header has the bias pattern and
  // that we are not the bias owner in the current epoch. We need to
  // figure out more details about the state of the header in order to
  // know what operations can be legally performed on the object's
  // header.

  // If the low three bits in the xor result aren't clear, that means
  // the prototype header is no longer biased and we have to revoke
  // the bias on this object.

  move(AT, markOopDesc::biased_lock_mask_in_place);
  andr(AT, swap_reg, AT);
  bne(AT, R0, try_revoke_bias);
  delayed()->nop();
  // Biasing is still enabled for this data type. See whether the
  // epoch of the current bias is still valid, meaning that the epoch
  // bits of the mark word are equal to the epoch bits of the
  // prototype header. (Note that the prototype header's epoch bits
  // only change at a safepoint.) If not, attempt to rebias the object
  // toward the current thread. Note that we must be absolutely sure
  // that the current epoch is invalid in order to do this because
  // otherwise the manipulations it performs on the mark word are
  // illegal.

  move(AT, markOopDesc::epoch_mask_in_place);
  andr(AT,swap_reg, AT);
  bne(AT, R0, try_rebias);
  delayed()->nop();
  // The epoch of the current bias is still valid but we know nothing
  // about the owner; it might be set or it might be clear. Try to
  // acquire the bias of the object using an atomic operation. If this
  // fails we will go in to the runtime to revoke the object's bias.
  // Note that we first construct the presumed unbiased header so we
  // don't accidentally blow away another thread's valid bias.

  ld_ptr(swap_reg, saved_mark_addr);

  move(AT, markOopDesc::biased_lock_mask_in_place | markOopDesc::age_mask_in_place | markOopDesc::epoch_mask_in_place);  
  andr(swap_reg, swap_reg, AT);

  if (need_tmp_reg) {
    push(tmp_reg);
  }
  get_thread(tmp_reg);
  orr(tmp_reg, tmp_reg, swap_reg);
  //if (os::is_MP()) {
  // lock();
  //}
  cmpxchg(tmp_reg, Address(obj_reg, 0), swap_reg);
  if (need_tmp_reg) {
    pop(tmp_reg);
  }
  // If the biasing toward our thread failed, this means that
  // another thread succeeded in biasing it toward itself and we
  // need to revoke that bias. The revocation will occur in the
  // interpreter runtime in the slow case.
  if (PrintBiasedLockingStatistics) {
    Label L;
    bne(AT, R0, L);
    delayed()->nop();
    push(tmp_reg);
    push(A0);
    atomic_inc32((address)BiasedLocking::anonymously_biased_lock_entry_count_addr(), 1, A0, tmp_reg);
    pop(A0);
    pop(tmp_reg);
    bind(L);
  }
  if (slow_case != NULL) {
    beq_far(AT, R0, *slow_case);
    delayed()->nop();
  }
  b(done);
  delayed()->nop();

  bind(try_rebias);
  // At this point we know the epoch has expired, meaning that the
  // current "bias owner", if any, is actually invalid. Under these
  // circumstances _only_, we are allowed to use the current header's
  // value as the comparison value when doing the cas to acquire the
  // bias in the current epoch. In other words, we allow transfer of
  // the bias from one thread to another directly in this situation.
  //
  // FIXME: due to a lack of registers we currently blow away the age
  // bits in this situation. Should attempt to preserve them.
  if (need_tmp_reg) {
    push(tmp_reg);
  }
  load_prototype_header(tmp_reg, obj_reg);
  get_thread(swap_reg);
  orr(tmp_reg, tmp_reg, swap_reg);
  ld_ptr(swap_reg, saved_mark_addr);

  // if (os::is_MP()) {
  //  lock();
  //}
  cmpxchg(tmp_reg, Address(obj_reg, 0), swap_reg);
  if (need_tmp_reg) {
    pop(tmp_reg);
  }
  // If the biasing toward our thread failed, then another thread
  // succeeded in biasing it toward itself and we need to revoke that
  // bias. The revocation will occur in the runtime in the slow case.
  if (PrintBiasedLockingStatistics) {
    Label L;
    bne(AT, R0, L);
    delayed()->nop();
    push(AT);
    push(tmp_reg);
    atomic_inc32((address)BiasedLocking::rebiased_lock_entry_count_addr(), 1, AT, tmp_reg);
    pop(tmp_reg);
    pop(AT);
    bind(L);
  }
  if (slow_case != NULL) {
    beq_far(AT, R0, *slow_case);
    delayed()->nop();
  }

  b(done);
  delayed()->nop();
  bind(try_revoke_bias);
  // The prototype mark in the klass doesn't have the bias bit set any
  // more, indicating that objects of this data type are not supposed
  // to be biased any more. We are going to try to reset the mark of
  // this object to the prototype value and fall through to the
  // CAS-based locking scheme. Note that if our CAS fails, it means
  // that another thread raced us for the privilege of revoking the
  // bias of this particular object, so it's okay to continue in the
  // normal locking code.
  //
  // FIXME: due to a lack of registers we currently blow away the age
  // bits in this situation. Should attempt to preserve them.
  ld_ptr(swap_reg, saved_mark_addr);

  if (need_tmp_reg) {
    push(tmp_reg);
  }
  load_prototype_header(tmp_reg, obj_reg);
  //if (os::is_MP()) {
  // lock();
  //}    
  cmpxchg(tmp_reg, Address(obj_reg, 0), swap_reg);
  if (need_tmp_reg) {
    pop(tmp_reg);
  }
  // Fall through to the normal CAS-based lock, because no matter what
  // the result of the above CAS, some thread must have succeeded in
  // removing the bias bit from the object's header.
  if (PrintBiasedLockingStatistics) {
    Label L;
    bne(AT, R0, L);
    delayed()->nop();
    push(AT);
    push(tmp_reg);
    atomic_inc32((address)BiasedLocking::revoked_lock_entry_count_addr(), 1, AT, tmp_reg);
    pop(tmp_reg);
    pop(AT);
    bind(L);
  }

  bind(cas_label);
  return null_check_offset;
}

void MacroAssembler::biased_locking_exit(Register obj_reg, Register temp_reg, Label& done) {
  assert(UseBiasedLocking, "why call this otherwise?");

  // Check for biased locking unlock case, which is a no-op
  // Note: we do not have to check the thread ID for two reasons.
  // First, the interpreter checks for IllegalMonitorStateException at
  // a higher level. Second, if the bias was revoked while we held the
  // lock, the object could not be rebiased toward another thread, so
  // the bias bit would be clear.
#ifdef _LP64
  ld(temp_reg, Address(obj_reg, oopDesc::mark_offset_in_bytes()));
  andi(temp_reg, temp_reg, markOopDesc::biased_lock_mask_in_place);
  daddi(AT, R0, markOopDesc::biased_lock_pattern);
#else
  lw(temp_reg, Address(obj_reg, oopDesc::mark_offset_in_bytes()));
  andi(temp_reg, temp_reg, markOopDesc::biased_lock_mask_in_place);
  addi(AT, R0, markOopDesc::biased_lock_pattern);
#endif

  beq(AT, temp_reg, done);
  delayed()->nop();
}

// NOTE: we dont increment the SP after call like the x86 version, maybe this is a problem, FIXME. 
// by yjl 6/27/2005 
// the stack pointer adjustment is needed. see InterpreterMacroAssembler::super_call_VM_leaf
// by yjl 7/11/2005
// this method will handle the stack problem, you need not to preserve the stack space for the argument now
// by yjl 8/1/2005
void MacroAssembler::call_VM_leaf_base(address entry_point,
    int number_of_arguments) {
  //call(RuntimeAddress(entry_point));
  //increment(rsp, number_of_arguments * wordSize);
  Label L, E;

  assert(number_of_arguments <= 4, "just check");

  andi(AT, SP, 0xf);
  beq(AT, R0, L);
  delayed()->nop();
  daddi(SP, SP, -8);
  {
	call(entry_point, relocInfo::runtime_call_type);
	delayed()->nop();
  }
  daddi(SP, SP, 8);
  b(E);
  delayed()->nop();

  bind(L);
  {
	call(entry_point, relocInfo::runtime_call_type);
	delayed()->nop();
  }
  bind(E);
}


void MacroAssembler::jmp(address entry) {
  li48(T9, (long)entry);
  jr(T9);
}

void MacroAssembler::jmp(address entry, relocInfo::relocType rtype) {
  switch (rtype) {
    case relocInfo::runtime_call_type:
    case relocInfo::none:
      jmp(entry);
      break;
    default:
      {
	InstructionMark im(this);
	relocate(rtype);
	li48(T9, (long)entry);
	jr(T9);
      }
      break;
  }
}

void MacroAssembler::call(address entry) {
// c/c++ code assume T9 is entry point, so we just always move entry to t9
// maybe there is some more graceful method to handle this. FIXME 
// by yjl 6/27/2005
// For more info, see class NativeCall.
#ifndef _LP64
  move(T9, (int)entry);
#else
  li48(T9, (long)entry);
#endif
  jalr(T9);
}

void MacroAssembler::call(address entry, relocInfo::relocType rtype) {
  switch (rtype) {
    case relocInfo::runtime_call_type:
    case relocInfo::none:
      call(entry);
      break;
    default:
      {
	InstructionMark im(this);
	relocate(rtype);
	call(entry);
      }
      break;
  }
}

void MacroAssembler::call(address entry, RelocationHolder& rh)
{
  switch (rh.type()) {
    case relocInfo::runtime_call_type:
    case relocInfo::none:
      call(entry);
      break;
    default:
      {
	InstructionMark im(this);
	relocate(rh);
	call(entry);
      }
      break;
  }
}

void MacroAssembler::ic_call(address entry) {
	RelocationHolder rh = virtual_call_Relocation::spec(pc());
	li64(IC_Klass, (long)Universe::non_oop_word());
	assert(entry != NULL, "call most probably wrong");
	InstructionMark im(this);
	relocate(rh);
	li48(T9, (long)entry);
	jalr(T9);
	delayed()->nop();
}

void MacroAssembler::c2bool(Register r) {
  Label L;
  Assembler::beq(r, R0, L);
  delayed()->nop();
  move(r, 1);
  bind(L);
}

#ifndef PRODUCT
extern "C" void findpc(intptr_t x);
#endif

void MacroAssembler::debug32(int rdi, int rsi, int rbp, int rsp, int rbx, int rdx, int rcx, int rax, int eip, char* msg) {
  // In order to get locks to work, we need to fake a in_VM state
  JavaThread* thread = JavaThread::current();
  JavaThreadState saved_state = thread->thread_state();
  thread->set_thread_state(_thread_in_vm);
  if (ShowMessageBoxOnError) {
    JavaThread* thread = JavaThread::current();
    JavaThreadState saved_state = thread->thread_state();
    thread->set_thread_state(_thread_in_vm);
    if (CountBytecodes || TraceBytecodes || StopInterpreterAt) {
      ttyLocker ttyl;
      BytecodeCounter::print();
    }
    // To see where a verify_oop failed, get $ebx+40/X for this frame.
    // This is the value of eip which points to where verify_oop will return.
    if (os::message_box(msg, "Execution stopped, print registers?")) {
      ttyLocker ttyl;
      tty->print_cr("eip = 0x%08x", eip);
#ifndef PRODUCT
      tty->cr();
      findpc(eip);
      tty->cr();
#endif
      tty->print_cr("rax, = 0x%08x", rax);
      tty->print_cr("rbx, = 0x%08x", rbx);
      tty->print_cr("rcx = 0x%08x", rcx);
      tty->print_cr("rdx = 0x%08x", rdx);
      tty->print_cr("rdi = 0x%08x", rdi);
      tty->print_cr("rsi = 0x%08x", rsi);
      tty->print_cr("rbp, = 0x%08x", rbp);
      tty->print_cr("rsp = 0x%08x", rsp);
      BREAKPOINT;
    }
  } else {
    ttyLocker ttyl;
    ::tty->print_cr("=============== DEBUG MESSAGE: %s ================\n", msg);
    assert(false, "DEBUG MESSAGE");
  }
  ThreadStateTransition::transition(thread, _thread_in_vm, saved_state);
}

void MacroAssembler::debug(char* msg/*, RegistersForDebugging* regs*/) {
  if ( ShowMessageBoxOnError ) {
    JavaThreadState saved_state = JavaThread::current()->thread_state();
    JavaThread::current()->set_thread_state(_thread_in_vm);
    {
      // In order to get locks work, we need to fake a in_VM state
      ttyLocker ttyl;
      ::tty->print_cr("EXECUTION STOPPED: %s\n", msg);
      if (CountBytecodes || TraceBytecodes || StopInterpreterAt) {
	BytecodeCounter::print();
      }

      //			if (os::message_box(msg, "Execution stopped, print registers?"))
      //				regs->print(::tty);
    }
    ThreadStateTransition::transition(JavaThread::current(), _thread_in_vm, saved_state);
  }
  else
    ::tty->print_cr("=============== DEBUG MESSAGE: %s ================\n", msg);
}


void MacroAssembler::stop(const char* msg) {
  li(A0, (long)msg);
#ifndef _LP64
  //reserver space for argument. added by yjl 7/10/2005
  addiu(SP, SP, - 1 * wordSize);
#endif
  call(CAST_FROM_FN_PTR(address, MacroAssembler::debug), relocInfo::runtime_call_type);
  delayed()->nop();
#ifndef _LP64
  //restore space for argument
  addiu(SP, SP, 1 * wordSize);
#endif
  brk(17);
}

void MacroAssembler::warn(const char* msg) {
#ifdef _LP64
  pushad();
  li(A0, (long)msg);
  call(CAST_FROM_FN_PTR(address, MacroAssembler::debug), relocInfo::runtime_call_type);
  delayed()->nop();
  popad();
#else
  pushad();
  addi(SP, SP, -4);
  sw(A0, SP, -1 * wordSize);
  li(A0, (long)msg);
  addi(SP, SP, -1 * wordSize);
  call(CAST_FROM_FN_PTR(address, MacroAssembler::debug), relocInfo::runtime_call_type);
  delayed()->nop();
  addi(SP, SP, 1 * wordSize);
  lw(A0, SP, -1 * wordSize);
  addi(SP, SP, 4);
  popad();
#endif
}

void MacroAssembler::print_reg(Register reg) {
/*
char *s = getenv("PRINT_REG");
if (s == NULL)
  return;
if (strcmp(s, "1") != 0)
  return;
*/
  void * cur_pc = pc();
  pushad();
  NOT_LP64(push(FP);)

  li(A0, (long)reg->name());
  if (reg == SP)
    addiu(A1, SP, wordSize * 23); //23 registers saved in pushad()
  else if (reg == A0)
    ld(A1, SP, wordSize * 19); //A0 has been modified by li(A0, (long)reg->name()). Ugly Code!
  else
    move(A1, reg);
  li(A2, (long)cur_pc);
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_reg_with_pc),relocInfo::runtime_call_type);
  delayed()->nop();
  NOT_LP64(pop(FP);)
  popad();

/*
  pushad();
#ifdef _LP64
  if (reg == SP)
    addiu(A0, SP, wordSize * 23); //23 registers saved in pushad()
  else
    move(A0, reg);
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_long),relocInfo::runtime_call_type);
  delayed()->nop();
#else 
  push(FP);
  move(A0, reg);
  dsrl32(A1, reg, 0);
  //call(CAST_FROM_FN_PTR(address, SharedRuntime::print_int),relocInfo::runtime_call_type);
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_long),relocInfo::runtime_call_type);
  delayed()->nop();
  pop(FP);
#endif
  popad();
  pushad();
  NOT_LP64(push(FP);)
  char b[50];
  sprintf((char *)b, " pc: %p\n",cur_pc);
  li(A0, (long)(char *)b);
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_str),relocInfo::runtime_call_type);
  delayed()->nop();
  NOT_LP64(pop(FP);)
  popad();
*/
}

void MacroAssembler::print_reg(FloatRegister reg) {
  void * cur_pc = pc();
  pushad();
  NOT_LP64(push(FP);)
  li(A0, (long)reg->name());
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_str),relocInfo::runtime_call_type);
  delayed()->nop();
  NOT_LP64(pop(FP);)
  popad();

  pushad();
  NOT_LP64(push(FP);)
#if 1
  move(FP, SP);
  move(AT, -(StackAlignmentInBytes));	
  andr(SP , SP , AT);
  mov_d(F12, reg);
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_double),relocInfo::runtime_call_type);
  delayed()->nop();
  move(SP, FP);
#else
  mov_s(F12, reg);
  //call(CAST_FROM_FN_PTR(address, SharedRuntime::print_float),relocInfo::runtime_call_type);
  //delayed()->nop();
#endif
  NOT_LP64(pop(FP);)
  popad();

#if 0
  pushad();
  NOT_LP64(push(FP);)
  char* b = new char[50];
  sprintf(b, " pc: %p\n", cur_pc);
  li(A0, (long)b);
  call(CAST_FROM_FN_PTR(address, SharedRuntime::print_str),relocInfo::runtime_call_type);
  delayed()->nop();
  NOT_LP64(pop(FP);)
  popad();
#endif
}

void MacroAssembler::increment(Register reg, int imm) {
  if (!imm) return;
  if (is_simm16(imm)) {
#ifdef _LP64
    daddiu(reg, reg, imm);
#else
    addiu(reg, reg, imm);
#endif
  } else {
    move(AT, imm);
#ifdef _LP64
    daddu(reg, reg, AT);
#else
    addu(reg, reg, AT);
#endif
  }
}

void MacroAssembler::decrement(Register reg, int imm) {
	increment(reg, -imm);
}


void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             bool check_exceptions) {
  call_VM_helper(oop_result, entry_point, 0, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             Register arg_1,
                             bool check_exceptions) {
  if (arg_1!=A1) move(A1, arg_1);
  call_VM_helper(oop_result, entry_point, 1, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             bool check_exceptions) {
  if (arg_1!=A1) move(A1, arg_1);
  if (arg_2!=A2) move(A2, arg_2); 
  assert(arg_2 != A1, "smashed argument");
  call_VM_helper(oop_result, entry_point, 2, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             Register arg_3,
                             bool check_exceptions) {
  if (arg_1!=A1) move(A1, arg_1);
  if (arg_2!=A2) move(A2, arg_2); assert(arg_2 != A1, "smashed argument");
  if (arg_3!=A3) move(A3, arg_3); assert(arg_3 != A1 && arg_3 != A2, "smashed argument");
  call_VM_helper(oop_result, entry_point, 3, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             int number_of_arguments,
                             bool check_exceptions) {
  call_VM_base(oop_result, NOREG, last_java_sp, entry_point, number_of_arguments, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             Register arg_1,
                             bool check_exceptions) {
  if (arg_1 != A1) move(A1, arg_1);
  call_VM(oop_result, last_java_sp, entry_point, 1, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             bool check_exceptions) {
  if (arg_1 != A1) move(A1, arg_1);
  if (arg_2 != A2) move(A2, arg_2); assert(arg_2 != A1, "smashed argument");
  call_VM(oop_result, last_java_sp, entry_point, 2, check_exceptions);
}

void MacroAssembler::call_VM(Register oop_result,
                             Register last_java_sp,
                             address entry_point,
                             Register arg_1,
                             Register arg_2,
                             Register arg_3,
                             bool check_exceptions) {
  if (arg_1 != A1) move(A1, arg_1);
  if (arg_2 != A2) move(A2, arg_2); assert(arg_2 != A1, "smashed argument");
  if (arg_3 != A3) move(A3, arg_3); assert(arg_3 != A1 && arg_3 != A2, "smashed argument");
  call_VM(oop_result, last_java_sp, entry_point, 3, check_exceptions);
}

void MacroAssembler::call_VM_base(Register oop_result,
                                  Register java_thread,
                                  Register last_java_sp,
                                  address  entry_point,
                                  int      number_of_arguments,
				  bool     check_exceptions) {

  address before_call_pc;
  // determine java_thread register
  if (!java_thread->is_valid()) {
#ifndef OPT_THREAD
    java_thread = T2;
    get_thread(java_thread);
#else
    java_thread = TREG;
#endif
  }
  // determine last_java_sp register
  if (!last_java_sp->is_valid()) {
    last_java_sp = SP;
  }
  // debugging support
  assert(number_of_arguments >= 0   , "cannot have negative number of arguments");
  assert(number_of_arguments <= 4   , "cannot have negative number of arguments");
  assert(java_thread != oop_result  , "cannot use the same register for java_thread & oop_result");
  assert(java_thread != last_java_sp, "cannot use the same register for java_thread & last_java_sp");

  assert(last_java_sp != FP, "this code doesn't work for last_java_sp == fp, which currently can't portably work anyway since C2 doesn't save ebp");

  // set last Java frame before call
  before_call_pc = (address)pc();
  set_last_Java_frame(java_thread, last_java_sp, FP, before_call_pc);

  // do the call
  move(A0, java_thread);
  call(entry_point, relocInfo::runtime_call_type);
  delayed()->nop();

  // restore the thread (cannot use the pushed argument since arguments
  // may be overwritten by C code generated by an optimizing compiler);
  // however can use the register value directly if it is callee saved.
#ifndef OPT_THREAD
  if (java_thread >=S0 && java_thread <=S7) {
#ifdef ASSERT
    { Label L;
      get_thread(AT);
      beq(java_thread, AT, L);
      delayed()->nop();
      stop("MacroAssembler::call_VM_base: edi not callee saved?");
      bind(L);
    }
#endif
  } else {
    get_thread(java_thread);
  }
#endif

  // discard thread and arguments
  ld_ptr(SP, java_thread, in_bytes(JavaThread::last_Java_sp_offset())); 
  // reset last Java frame
  reset_last_Java_frame(java_thread, false, true);

  check_and_handle_popframe(java_thread);
  check_and_handle_earlyret(java_thread);
  if (check_exceptions) {
    // check for pending exceptions (java_thread is set upon return)
    Label L;
#ifdef _LP64
    ld(AT, java_thread, in_bytes(Thread::pending_exception_offset()));
#else
    lw(AT, java_thread, in_bytes(Thread::pending_exception_offset()));
#endif
    beq(AT, R0, L);
    delayed()->nop();
    li(AT, before_call_pc);
    push(AT);
    jmp(StubRoutines::forward_exception_entry(), relocInfo::runtime_call_type);
    delayed()->nop();
    bind(L);
  }

  // get oop result if there is one and reset the value in the thread
  if (oop_result->is_valid()) {
#ifdef _LP64
    ld(oop_result, java_thread, in_bytes(JavaThread::vm_result_offset()));
    sd(R0, java_thread, in_bytes(JavaThread::vm_result_offset()));
#else
    lw(oop_result, java_thread, in_bytes(JavaThread::vm_result_offset()));
    sw(R0, java_thread, in_bytes(JavaThread::vm_result_offset()));
#endif
    verify_oop(oop_result);
  }
}

void MacroAssembler::call_VM_helper(Register oop_result, address entry_point, int number_of_arguments, bool check_exceptions) {

  move(V0, SP);
  //we also reserve space for java_thread here
#ifndef _LP64
  daddi(SP, SP, (1 + number_of_arguments) * (- wordSize));
#endif
  move(AT, -(StackAlignmentInBytes));
  andr(SP, SP, AT);
  call_VM_base(oop_result, NOREG, V0, entry_point, number_of_arguments, check_exceptions);

}

void MacroAssembler::call_VM_leaf(address entry_point, int number_of_arguments) {
  call_VM_leaf_base(entry_point, number_of_arguments);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_0) {
  if (arg_0 != A0) move(A0, arg_0);
  call_VM_leaf(entry_point, 1);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_0, Register arg_1) {
  if (arg_0 != A0) move(A0, arg_0);
  if (arg_1 != A1) move(A1, arg_1); assert(arg_1 != A0, "smashed argument");
  call_VM_leaf(entry_point, 2);
}

void MacroAssembler::call_VM_leaf(address entry_point, Register arg_0, Register arg_1, Register arg_2) {
  if (arg_0 != A0) move(A0, arg_0);
  if (arg_1 != A1) move(A1, arg_1); assert(arg_1 != A0, "smashed argument");
  if (arg_2 != A2) move(A2, arg_2); assert(arg_2 != A0 && arg_2 != A1, "smashed argument");
  call_VM_leaf(entry_point, 3);
}
void MacroAssembler::super_call_VM_leaf(address entry_point) {
	MacroAssembler::call_VM_leaf_base(entry_point, 0);
}


void MacroAssembler::super_call_VM_leaf(address entry_point,
                                                   Register arg_1) {
  if (arg_1 != A0) move(A0, arg_1);
  MacroAssembler::call_VM_leaf_base(entry_point, 1);
}


void MacroAssembler::super_call_VM_leaf(address entry_point,
                                                   Register arg_1,
                                                   Register arg_2) {
  if (arg_1 != A0) move(A0, arg_1);
  if (arg_2 != A1) move(A1, arg_2); assert(arg_2 != A0, "smashed argument");
  MacroAssembler::call_VM_leaf_base(entry_point, 2);
}
void MacroAssembler::super_call_VM_leaf(address entry_point,
                                                   Register arg_1,
                                                   Register arg_2,
                                                   Register arg_3) {
  if (arg_1 != A0) move(A0, arg_1);
  if (arg_2 != A1) move(A1, arg_2); assert(arg_2 != A0, "smashed argument");
  if (arg_3 != A2) move(A2, arg_3); assert(arg_3 != A0 && arg_3 != A1, "smashed argument");
  MacroAssembler::call_VM_leaf_base(entry_point, 3);
}

void MacroAssembler::check_and_handle_earlyret(Register java_thread) {
}

void MacroAssembler::check_and_handle_popframe(Register java_thread) {
}

void MacroAssembler::null_check(Register reg, int offset) {
  if (needs_explicit_null_check(offset)) {
    // provoke OS NULL exception if reg = NULL by
    // accessing M[reg] w/o changing any (non-CC) registers
    // NOTE: cmpl is plenty here to provoke a segv
    lw(AT, reg, 0);
/* Jin
    nop();	
    nop();
    nop();
*/
    // Note: should probably use testl(rax, Address(reg, 0));
    //       may be shorter code (however, this version of
    //       testl needs to be implemented first)
  } else {
    // nothing to do, (later) access of M[reg + offset]
    // will provoke OS NULL exception if reg = NULL
  }
}

void MacroAssembler::enter() {
  push2(RA, FP);
  move(FP, SP);
}
 
void MacroAssembler::leave() {
#ifndef _LP64
  //move(SP, FP);
  //pop2(FP, RA);
  addi(SP, FP, 2 * wordSize);
  lw(RA, SP, - 1 * wordSize);
  lw(FP, SP, - 2 * wordSize);
#else
  daddi(SP, FP, 2 * wordSize);
  ld(RA, SP, - 1 * wordSize);
  ld(FP, SP, - 2 * wordSize);
#endif
}
/*
void MacroAssembler::os_breakpoint() {
  // instead of directly emitting a breakpoint, call os:breakpoint for better debugability
  // (e.g., MSVC can't call ps() otherwise)
  call(RuntimeAddress(CAST_FROM_FN_PTR(address, os::breakpoint)));
}
*/
void MacroAssembler::reset_last_Java_frame(Register java_thread, bool clear_fp, bool clear_pc) {
  // determine java_thread register
  if (!java_thread->is_valid()) {
#ifndef OPT_THREAD
    java_thread = T1;
    get_thread(java_thread);
#else
    java_thread = TREG;
#endif
  }
  // we must set sp to zero to clear frame
  st_ptr(R0, java_thread, in_bytes(JavaThread::last_Java_sp_offset()));
  // must clear fp, so that compiled frames are not confused; it is possible
  // that we need it only for debugging
  if(clear_fp)	
    st_ptr(R0, java_thread, in_bytes(JavaThread::last_Java_fp_offset()));

  if (clear_pc)
    st_ptr(R0, java_thread, in_bytes(JavaThread::last_Java_pc_offset()));
}

void MacroAssembler::reset_last_Java_frame(bool clear_fp,
                                           bool clear_pc) {
  Register thread = TREG;
#ifndef OPT_THREAD
  get_thread(thread);
#endif
  // we must set sp to zero to clear frame
  sd(R0, Address(thread, JavaThread::last_Java_sp_offset()));
  // must clear fp, so that compiled frames are not confused; it is
  // possible that we need it only for debugging
  if (clear_fp) {
    sd(R0, Address(thread, JavaThread::last_Java_fp_offset()));
  }

  if (clear_pc) {
    sd(R0, Address(thread, JavaThread::last_Java_pc_offset()));
  }
}

// Write serialization page so VM thread can do a pseudo remote membar.
// We use the current thread pointer to calculate a thread specific
// offset to write to within the page. This minimizes bus traffic
// due to cache line collision.
void MacroAssembler::serialize_memory(Register thread, Register tmp) {
  move(tmp, thread);
  srl(tmp, tmp,os::get_serialize_page_shift_count());
  move(AT, (os::vm_page_size() - sizeof(int))); 
  andr(tmp, tmp,AT);
  sw(tmp,Address(tmp, (intptr_t)os::get_memory_serialize_page()));
}

// Calls to C land
//
// When entering C land, the rbp, & rsp of the last Java frame have to be recorded
// in the (thread-local) JavaThread object. When leaving C land, the last Java fp
// has to be reset to 0. This is required to allow proper stack traversal.
void MacroAssembler::set_last_Java_frame(Register java_thread,
                                         Register last_java_sp,
                                         Register last_java_fp,
                                         address  last_java_pc) {
  // determine java_thread register
  if (!java_thread->is_valid()) {
#ifndef OPT_THREAD
    java_thread = T2;
    get_thread(java_thread);
#else
    java_thread = TREG;
#endif
  }
  // determine last_java_sp register
  if (!last_java_sp->is_valid()) {
    last_java_sp = SP;
  }

  // last_java_fp is optional

  if (last_java_fp->is_valid()) {
    st_ptr(last_java_fp, java_thread, in_bytes(JavaThread::last_Java_fp_offset()));
  }

  // last_java_pc is optional

  if (last_java_pc != NULL) {
    relocate(relocInfo::internal_pc_type);
    li48(AT, (long)last_java_pc);
    st_ptr(AT, java_thread, in_bytes(JavaThread::last_Java_pc_offset()));
  }
  st_ptr(last_java_sp, java_thread, in_bytes(JavaThread::last_Java_sp_offset()));
}

void MacroAssembler::set_last_Java_frame(Register last_java_sp,
                                         Register last_java_fp,
                                         address  last_java_pc) {
  // determine last_java_sp register
  if (!last_java_sp->is_valid()) {
    last_java_sp = SP; 
  }

  Register thread = TREG;
#ifndef OPT_THREAD
  get_thread(thread);
#endif
  // last_java_fp is optional
  if (last_java_fp->is_valid()) {
    sd(last_java_fp, Address(thread, JavaThread::last_Java_fp_offset()));
  }

  // last_java_pc is optional
  if (last_java_pc != NULL) {
    Address java_pc(thread,
                    JavaThread::frame_anchor_offset() + JavaFrameAnchor::last_Java_pc_offset());
    li(AT, (intptr_t)(last_java_pc));
    sd(AT, java_pc);
  }

  sd(last_java_sp, Address(thread, JavaThread::last_Java_sp_offset()));
}

//////////////////////////////////////////////////////////////////////////////////
#ifndef SERIALGC

void MacroAssembler::g1_write_barrier_pre(Register obj,
#ifndef _LP64
                                          Register thread,
#endif
                                          Register tmp,
                                          Register tmp2,
                                          bool tosca_live) {
/*  LP64_ONLY(Register thread = r15_thread;)
  Address in_progress(thread, in_bytes(JavaThread::satb_mark_queue_offset() +
                                       PtrQueue::byte_offset_of_active()));

  Address index(thread, in_bytes(JavaThread::satb_mark_queue_offset() +
                                       PtrQueue::byte_offset_of_index()));
  Address buffer(thread, in_bytes(JavaThread::satb_mark_queue_offset() +
                                       PtrQueue::byte_offset_of_buf()));


  Label done;
  Label runtime;

  // if (!marking_in_progress) goto done;
  if (in_bytes(PtrQueue::byte_width_of_active()) == 4) {
    cmpl(in_progress, 0);
  } else {
    assert(in_bytes(PtrQueue::byte_width_of_active()) == 1, "Assumption");
    cmpb(in_progress, 0);
  }
  jcc(Assembler::equal, done);

  // if (x.f == NULL) goto done;
  cmpptr(Address(obj, 0), NULL_WORD);
  jcc(Assembler::equal, done);

  // Can we store original value in the thread's buffer?

  LP64_ONLY(movslq(tmp, index);)
  movptr(tmp2, Address(obj, 0));
#ifdef _LP64
  cmpq(tmp, 0);
#else
  cmpl(index, 0);
#endif
  jcc(Assembler::equal, runtime);
#ifdef _LP64
  subq(tmp, wordSize);
  movl(index, tmp);
  addq(tmp, buffer);
#else
  subl(index, wordSize);
  movl(tmp, buffer);
  addl(tmp, index);
#endif
  movptr(Address(tmp, 0), tmp2);
  jmp(done);
  bind(runtime);
  // save the live input values
  if(tosca_live) push(rax);
  push(obj);
#ifdef _LP64
  movq(c_rarg0, Address(obj, 0));
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_pre), c_rarg0, r15_thread);
#else
  push(thread);
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_pre), tmp2, thread);
  pop(thread);
#endif
  pop(obj);
  if(tosca_live) pop(rax);
  bind(done);
*/
}

void MacroAssembler::g1_write_barrier_post(Register store_addr,
                                           Register new_val,
#ifndef _LP64
                                           Register thread,
#endif
                                           Register tmp,
                                           Register tmp2) {

  /*LP64_ONLY(Register thread = r15_thread;)
  Address queue_index(thread, in_bytes(JavaThread::dirty_card_queue_offset() +
                                       PtrQueue::byte_offset_of_index()));
  Address buffer(thread, in_bytes(JavaThread::dirty_card_queue_offset() +
                                       PtrQueue::byte_offset_of_buf()));
  BarrierSet* bs = Universe::heap()->barrier_set();
  CardTableModRefBS* ct = (CardTableModRefBS*)bs;
  Label done;
  Label runtime;

  // Does store cross heap regions?

  movptr(tmp, store_addr);
  xorptr(tmp, new_val);
  shrptr(tmp, HeapRegion::LogOfHRGrainBytes);
  jcc(Assembler::equal, done);

  // crosses regions, storing NULL?

  cmpptr(new_val, (int32_t) NULL_WORD);
  jcc(Assembler::equal, done);

  // storing region crossing non-NULL, is card already dirty?

  ExternalAddress cardtable((address) ct->byte_map_base);
  assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");
#ifdef _LP64
  const Register card_addr = tmp;

  movq(card_addr, store_addr);
  shrq(card_addr, CardTableModRefBS::card_shift);

  lea(tmp2, cardtable);

  // get the address of the card
  addq(card_addr, tmp2);
#else
  const Register card_index = tmp;

  movl(card_index, store_addr);
  shrl(card_index, CardTableModRefBS::card_shift);

  Address index(noreg, card_index, Address::times_1);
  const Register card_addr = tmp;
  lea(card_addr, as_Address(ArrayAddress(cardtable, index)));
#endif
  cmpb(Address(card_addr, 0), 0);
  jcc(Assembler::equal, done);

  // storing a region crossing, non-NULL oop, card is clean.
  // dirty card and log.

  movb(Address(card_addr, 0), 0);

  cmpl(queue_index, 0);
  jcc(Assembler::equal, runtime);
  subl(queue_index, wordSize);
  movptr(tmp2, buffer);
#ifdef _LP64
  movslq(rscratch1, queue_index);
  addq(tmp2, rscratch1);
  movq(Address(tmp2, 0), card_addr);
#else
  addl(tmp2, queue_index);
  movl(Address(tmp2, 0), card_index);
#endif
  jmp(done);

  bind(runtime);
  // save the live input values
  push(store_addr);
  push(new_val);
#ifdef _LP64
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_post), card_addr, r15_thread);
#else
  push(thread);
  call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::g1_wb_post), card_addr, thread);
  pop(thread);
#endif
  pop(new_val);
  pop(store_addr);

  bind(done);
*/
}

#endif // SERIALGC
//////////////////////////////////////////////////////////////////////////////////


void MacroAssembler::store_check(Register obj) {
  // Does a store check for the oop in register obj. The content of
  // register obj is destroyed afterwards.
  store_check_part_1(obj);
  store_check_part_2(obj);
}

void MacroAssembler::store_check(Register obj, Address dst) {
  store_check(obj);
}


// split the store check operation so that other instructions can be scheduled inbetween
void MacroAssembler::store_check_part_1(Register obj) {
  BarrierSet* bs = Universe::heap()->barrier_set();
  assert(bs->kind() == BarrierSet::CardTableModRef, "Wrong barrier set kind");
#ifdef _LP64
  dsrl(obj, obj, CardTableModRefBS::card_shift);
#else
  shr(obj, CardTableModRefBS::card_shift);
#endif
}

void MacroAssembler::store_check_part_2(Register obj) {
  BarrierSet* bs = Universe::heap()->barrier_set();
  assert(bs->kind() == BarrierSet::CardTableModRef, "Wrong barrier set kind");
  CardTableModRefBS* ct = (CardTableModRefBS*)bs;
  assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");

  li(AT, (long)ct->byte_map_base);
#ifdef _LP64
  dadd(AT, AT, obj);
#else
  add(AT, AT, obj);
#endif
  sb(R0, AT, 0);
}
/*
void MacroAssembler::subptr(Register dst, int32_t imm32) {
  LP64_ONLY(subq(dst, imm32)) NOT_LP64(subl(dst, imm32));
}

void MacroAssembler::subptr(Register dst, Register src) {
  LP64_ONLY(subq(dst, src)) NOT_LP64(subl(dst, src));
}

void MacroAssembler::test32(Register src1, AddressLiteral src2) {
  // src2 must be rval

  if (reachable(src2)) {
    testl(src1, as_Address(src2));
  } else {
    lea(rscratch1, src2);
    testl(src1, Address(rscratch1, 0));
  }
}

// C++ bool manipulation
void MacroAssembler::testbool(Register dst) {
  if(sizeof(bool) == 1)
    testb(dst, 0xff);
  else if(sizeof(bool) == 2) {
    // testw implementation needed for two byte bools
    ShouldNotReachHere();
  } else if(sizeof(bool) == 4)
    testl(dst, dst);
  else
    // unsupported
    ShouldNotReachHere();
}

void MacroAssembler::testptr(Register dst, Register src) {
  LP64_ONLY(testq(dst, src)) NOT_LP64(testl(dst, src));
}


*/

// Defines obj, preserves var_size_in_bytes, okay for t2 == var_size_in_bytes.
void MacroAssembler::tlab_allocate(Register obj, Register var_size_in_bytes, int con_size_in_bytes,
                                   Register t1, Register t2, Label& slow_case) {
  assert_different_registers(obj, var_size_in_bytes, t1, t2, AT);

  Register end = t2;
#ifndef OPT_THREAD
  Register thread = t1;
  get_thread(thread);
#else
  Register thread = TREG;
#endif
  verify_tlab(t1, t2);//blows t1&t2

  ld_ptr(obj, thread, in_bytes(JavaThread::tlab_top_offset()));

  if (var_size_in_bytes == NOREG) {
    // i dont think we need move con_size_in_bytes to a register first.
    // by yjl 8/17/2005
    assert(is_simm16(con_size_in_bytes), "fixme by moving imm to a register first");
    addi(end, obj, con_size_in_bytes);
  } else {
    add(end, obj, var_size_in_bytes);
  }

  ld_ptr(AT, thread, in_bytes(JavaThread::tlab_end_offset()));
  sltu(AT, AT, end);
  bne_far(AT, R0, slow_case);
  delayed()->nop();


  // update the tlab top pointer
  st_ptr(end, thread, in_bytes(JavaThread::tlab_top_offset()));

  // recover var_size_in_bytes if necessary
  /*if (var_size_in_bytes == end) {
    sub(var_size_in_bytes, end, obj);
    }*/

  verify_tlab(t1, t2);
}

// Defines obj, preserves var_size_in_bytes
void MacroAssembler::eden_allocate(Register obj, Register var_size_in_bytes, int con_size_in_bytes,
		Register t1, Register t2, Label& slow_case) {
  assert_different_registers(obj, var_size_in_bytes, t1, AT);
  if (CMSIncrementalMode || !Universe::heap()->supports_inline_contig_alloc()) { //by yyq
    // No allocation in the shared eden.
    b_far(slow_case);
    delayed()->nop();
  } else {

#ifndef _LP64
    Address heap_top(t1, Assembler::split_low((intptr_t)Universe::heap()->top_addr()));
    lui(t1, split_high((intptr_t)Universe::heap()->top_addr()));
#else
    Address heap_top(t1);
    li(t1, (long)Universe::heap()->top_addr());
#endif
    ld_ptr(obj, heap_top);

    Register end = t2;
    Label retry;

    bind(retry);
    if (var_size_in_bytes == NOREG) {
    // i dont think we need move con_size_in_bytes to a register first.
    // by yjl 8/17/2005
      assert(is_simm16(con_size_in_bytes), "fixme by moving imm to a register first");
      addi(end, obj, con_size_in_bytes);
    } else {
      add(end, obj, var_size_in_bytes);
    }
    // if end < obj then we wrapped around => object too long => slow case
    sltu(AT, end, obj);
    bne_far(AT, R0, slow_case);
    delayed()->nop();
    
    //lui(AT, split_high((int)Universe::heap()->end_addr()));
    //lw(AT, AT, split_low((int)Universe::heap()->end_addr()));
    li(AT, (long)Universe::heap()->end_addr());
    sltu(AT, AT, end);
    bne_far(AT, R0, slow_case);
    delayed()->nop();
    // Compare obj with the top addr, and if still equal, store the new top addr in
    // end at the address of the top addr pointer. Sets ZF if was equal, and clears
    // it otherwise. Use lock prefix for atomicity on MPs.
    if (os::is_MP()) {
    	///lock();
    }
    
    // if someone beat us on the allocation, try again, otherwise continue
    cmpxchg(end, heap_top, obj);
    beq_far(AT, R0, retry);    //by yyq
    delayed()->nop();

  }
}

void MacroAssembler::tlab_refill(Label& retry, Label& try_eden, Label& slow_case) {
	Register top = T0;
	Register t1  = T1;
/* Jin: tlab_refill() is called in 

     [c1_Runtime1_mips.cpp] Runtime1::generate_code_for(new_type_array_id);

  In generate_code_for(), T2 has been assigned as a register(length), which is used
 after calling tlab_refill();
  Therefore, tlab_refill() should not use T2.

 Source:

Exception in thread "main" java.lang.ArrayIndexOutOfBoundsException
        at java.lang.System.arraycopy(Native Method)
        at java.util.Arrays.copyOf(Arrays.java:2799)	<-- alloc_array
        at sun.misc.Resource.getBytes(Resource.java:117)
        at java.net.URLClassLoader.defineClass(URLClassLoader.java:273)
        at java.net.URLClassLoader.findClass(URLClassLoader.java:205)
        at java.lang.ClassLoader.loadClass(ClassLoader.java:321)
 */
	Register t2  = T9;
	Register t3  = T3;
	Register thread_reg = T8;
	Label do_refill, discard_tlab;
	if (CMSIncrementalMode || !Universe::heap()->supports_inline_contig_alloc()) { //by yyq
		// No allocation in the shared eden.
		b(slow_case);
		delayed()->nop();
	}

	get_thread(thread_reg);

	ld_ptr(top, thread_reg, in_bytes(JavaThread::tlab_top_offset()));
	ld_ptr(t1, thread_reg, in_bytes(JavaThread::tlab_end_offset()));

	// calculate amount of free space
	sub(t1, t1, top);
	shr(t1, LogHeapWordSize);

	// Retain tlab and allocate object in shared space if
	// the amount free in the tlab is too large to discard.
	ld_ptr(t2, thread_reg, in_bytes(JavaThread::tlab_refill_waste_limit_offset()));
	slt(AT, t2, t1);
	beq(AT, R0, discard_tlab);
	delayed()->nop();

	// Retain
	
#ifndef _LP64
	move(AT, ThreadLocalAllocBuffer::refill_waste_limit_increment());
#else
	li(AT, ThreadLocalAllocBuffer::refill_waste_limit_increment());
#endif
	add(t2, t2, AT);
	st_ptr(t2, thread_reg, in_bytes(JavaThread::tlab_refill_waste_limit_offset()));
	
	if (TLABStats) {
		// increment number of slow_allocations
		lw(AT, thread_reg, in_bytes(JavaThread::tlab_slow_allocations_offset()));
		addiu(AT, AT, 1);
		sw(AT, thread_reg, in_bytes(JavaThread::tlab_slow_allocations_offset()));
	}
	b(try_eden);
	delayed()->nop();

  bind(discard_tlab);
	if (TLABStats) {
		// increment number of refills
		lw(AT, thread_reg, in_bytes(JavaThread::tlab_number_of_refills_offset()));
		addi(AT, AT, 1);
		sw(AT, thread_reg, in_bytes(JavaThread::tlab_number_of_refills_offset()));
		// accumulate wastage -- t1 is amount free in tlab
		lw(AT, thread_reg, in_bytes(JavaThread::tlab_fast_refill_waste_offset()));
		add(AT, AT, t1);
		sw(AT, thread_reg, in_bytes(JavaThread::tlab_fast_refill_waste_offset()));
	}

	// if tlab is currently allocated (top or end != null) then
	// fill [top, end + alignment_reserve) with array object
	beq(top, R0, do_refill);
	delayed()->nop();

	// set up the mark word
	li(AT, (long)markOopDesc::prototype()->copy_set_hash(0x2));
	st_ptr(AT, top, oopDesc::mark_offset_in_bytes());

	// set the length to the remaining space
	addi(t1, t1, - typeArrayOopDesc::header_size(T_INT));
	addi(t1, t1, ThreadLocalAllocBuffer::alignment_reserve());
	shl(t1, log2_intptr(HeapWordSize/sizeof(jint)));
	sw(t1, top, arrayOopDesc::length_offset_in_bytes());

	// set klass to intArrayKlass
#ifndef _LP64
	lui(AT, split_high((intptr_t)Universe::intArrayKlassObj_addr()));
	lw(t1, AT, split_low((intptr_t)Universe::intArrayKlassObj_addr()));
#else
	li(AT, (intptr_t)Universe::intArrayKlassObj_addr());
	ld_ptr(t1, AT, 0);
#endif
	//st_ptr(t1, top, oopDesc::klass_offset_in_bytes());
	store_klass(top, t1);

	// refill the tlab with an eden allocation
	bind(do_refill);
	ld_ptr(t1, thread_reg, in_bytes(JavaThread::tlab_size_offset()));
	shl(t1, LogHeapWordSize);
	// add object_size ??
	eden_allocate(top, t1, 0, t2, t3, slow_case);

	// Check that t1 was preserved in eden_allocate.
#ifdef ASSERT
	if (UseTLAB) {
		Label ok;
		assert_different_registers(thread_reg, t1);
		ld_ptr(AT, thread_reg, in_bytes(JavaThread::tlab_size_offset()));
		shl(AT, LogHeapWordSize);
		beq(AT, t1, ok);
		delayed()->nop();
		stop("assert(t1 != tlab size)");
		should_not_reach_here();

		bind(ok);
	}
#endif
	st_ptr(top, thread_reg, in_bytes(JavaThread::tlab_start_offset()));
	st_ptr(top, thread_reg, in_bytes(JavaThread::tlab_top_offset()));
	add(top, top, t1);	
	addi(top, top, - ThreadLocalAllocBuffer::alignment_reserve_in_bytes());
	st_ptr(top, thread_reg, in_bytes(JavaThread::tlab_end_offset()));
	verify_tlab(t1, t2);
	b(retry);
	delayed()->nop();
}

static const double     pi_4 =  0.7853981633974483;

// the x86 version is to clumsy, i dont think we need that fuss. maybe i'm wrong, FIXME
// must get argument(a double) in F12/F13
//void MacroAssembler::trigfunc(char trig, bool preserve_cpu_regs, int num_fpu_regs_in_use) {
//We need to preseve the register which maybe modified during the Call @Jerome
void MacroAssembler::trigfunc(char trig, int num_fpu_regs_in_use) {
//save all modified register here
//	if (preserve_cpu_regs) {
//	}
//FIXME, in the disassembly of tirgfunc, only used V0,V1,T9, SP,RA,so we ony save V0,V1,T9 
	pushad();
//we should preserve the stack space before we call
	addi(SP, SP, -wordSize * 2);
        switch (trig){
		case 's' :
              		call( CAST_FROM_FN_PTR(address, SharedRuntime::dsin), relocInfo::runtime_call_type );
			delayed()->nop();
			break;
		case 'c':	
			call( CAST_FROM_FN_PTR(address, SharedRuntime::dcos), relocInfo::runtime_call_type );
			delayed()->nop();
			break;
		case 't':
			call( CAST_FROM_FN_PTR(address, SharedRuntime::dtan), relocInfo::runtime_call_type );
			delayed()->nop();
			break;
		default:assert (false, "bad intrinsic");
		break;
	
	}

	addi(SP, SP, wordSize * 2);
	popad();
//	if (preserve_cpu_regs) {
//	}
}
/*

void MacroAssembler::ucomisd(XMMRegister dst, AddressLiteral src) {
  ucomisd(dst, as_Address(src));
}

void MacroAssembler::ucomiss(XMMRegister dst, AddressLiteral src) {
  ucomiss(dst, as_Address(src));
}

void MacroAssembler::xorpd(XMMRegister dst, AddressLiteral src) {
  if (reachable(src)) {
    xorpd(dst, as_Address(src));
  } else {
    lea(rscratch1, src);
    xorpd(dst, Address(rscratch1, 0));
  }
}

void MacroAssembler::xorps(XMMRegister dst, AddressLiteral src) {
  if (reachable(src)) {
    xorps(dst, as_Address(src));
  } else {
    lea(rscratch1, src);
    xorps(dst, Address(rscratch1, 0));
  }
}
 */

#ifdef _LP64
void MacroAssembler::li(Register rd, long imm) {
  if (imm <= max_jint && imm >= min_jint) {
    li32(rd, (int)imm);
  } else if (julong(imm) <= 0xFFFFFFFF) {
    assert_not_delayed();
    // lui sign-extends, so we can't use that.
    ori(rd, R0, julong(imm) >> 16);
    dsll(rd, rd, 16);
    ori(rd, rd, split_low(imm));
  //aoqi_test
  //} else if ((imm > 0) && ((imm >> 48) == 0)) {
  } else if ((imm > 0) && is_simm16(imm >> 32)) {
    /* A 48-bit address */
    li48(rd, imm);
  } else {
    li64(rd, imm);
  }
}
#else
void MacroAssembler::li(Register rd, long imm) {
  li32(rd, (int)imm);
}
#endif

void MacroAssembler::li32(Register reg, int imm) {
  if (is_simm16(imm)) {
    /* Jin: for imm < 0, we should use addi instead of addiu.
     *
     *  java.lang.StringCoding$StringDecoder.decode(jobject, jint, jint)
     *
     *  78 move [int:-1|I] [a0|I]
     *    : daddi a0, zero, 0xffffffff  (correct)
     *    : daddiu a0, zero, 0xffffffff (incorrect)
     */
    if (imm >= 0)
      addiu(reg, R0, imm);
    else
      addi(reg, R0, imm);
  } else {
    lui(reg, split_low(imm >> 16));
    if (split_low(imm))
      ori(reg, reg, split_low(imm));
  }
}

#ifdef _LP64
void MacroAssembler::li64(Register rd, long imm) {
  assert_not_delayed();
  lui(rd, imm >> 48);
  ori(rd, rd, split_low(imm >> 32));
  dsll(rd, rd, 16);
  ori(rd, rd, split_low(imm >> 16));
  dsll(rd, rd, 16);
  ori(rd, rd, split_low(imm));
}

void MacroAssembler::li48(Register rd, long imm) {
  assert(is_simm16(imm >> 32), "Not a 48-bit address");
  lui(rd, imm >> 32);
  ori(rd, rd, split_low(imm >> 16));
  dsll(rd, rd, 16);
  ori(rd, rd, split_low(imm));
}
#endif
// NOTE: i dont push eax as i486.
// the x86 save eax for it use eax as the jump register
void MacroAssembler::verify_oop(Register reg, const char* s) {
  /*
     if (!VerifyOops) return;

  // Pass register number to verify_oop_subroutine
  char* b = new char[strlen(s) + 50];
  sprintf(b, "verify_oop: %s: %s", reg->name(), s);
  push(rax);                          // save rax,
  push(reg);                          // pass register argument
  ExternalAddress buffer((address) b);
  // avoid using pushptr, as it modifies scratch registers
  // and our contract is not to modify anything
  movptr(rax, buffer.addr());
  push(rax);
  // call indirectly to solve generation ordering problem
  movptr(rax, ExternalAddress(StubRoutines::verify_oop_subroutine_entry_address()));
  call(rax);
   */
  if (!VerifyOops) return;
  const char * b = NULL; 
  stringStream ss;
  ss.print("verify_oop: %s: %s", reg->name(), s);
  b = code_string(ss.as_string());
#ifdef _LP64
  pushad();
  move(A1, reg);
  li(A0, (long)b);
  li(AT, (long)StubRoutines::verify_oop_subroutine_entry_address());
  ld(T9, AT, 0);
  jalr(T9);
  delayed()->nop();
  popad();
#else
  // Pass register number to verify_oop_subroutine
  sw(T0, SP, - wordSize);
  sw(T1, SP, - 2*wordSize);
  sw(RA, SP, - 3*wordSize);
  sw(A0, SP ,- 4*wordSize);	
  sw(A1, SP ,- 5*wordSize);	
  sw(AT, SP ,- 6*wordSize);	
  sw(T9, SP ,- 7*wordSize);	
  addiu(SP, SP, - 7 * wordSize);
  move(A1, reg);
  li(A0, (long)b);
  // call indirectly to solve generation ordering problem
  li(AT, (long)StubRoutines::verify_oop_subroutine_entry_address());        	
  lw(T9, AT, 0);
  jalr(T9);
  delayed()->nop();
  lw(T0, SP, 6* wordSize);
  lw(T1, SP, 5* wordSize);
  lw(RA, SP, 4* wordSize);
  lw(A0, SP, 3* wordSize);
  lw(A1, SP, 2* wordSize);
  lw(AT, SP, 1* wordSize);
  lw(T9, SP, 0* wordSize);
  addiu(SP, SP, 7 * wordSize);
#endif
}


void MacroAssembler::verify_oop_addr(Address addr, const char* s) {
	if (!VerifyOops) {
		nop();
		return;
	}
	// Pass register number to verify_oop_subroutine
	const char * b = NULL;
	stringStream ss;
	ss.print("verify_oop_addr: %s",  s);
	b = code_string(ss.as_string());

	st_ptr(T0, SP, - wordSize);
	st_ptr(T1, SP, - 2*wordSize);
	st_ptr(RA, SP, - 3*wordSize);
	st_ptr(A0, SP, - 4*wordSize);	
	st_ptr(A1, SP, - 5*wordSize);	
	st_ptr(AT, SP, - 6*wordSize);	
	st_ptr(T9, SP, - 7*wordSize);	
	ld_ptr(A1, addr);   // addr may use SP, so load from it before change SP
	addiu(SP, SP, - 7 * wordSize);

	li(A0, (long)b);
	// call indirectly to solve generation ordering problem
	li(AT, (long)StubRoutines::verify_oop_subroutine_entry_address());        	
	ld_ptr(T9, AT, 0);
	jalr(T9);
	delayed()->nop();
	ld_ptr(T0, SP, 6* wordSize);
	ld_ptr(T1, SP, 5* wordSize);
	ld_ptr(RA, SP, 4* wordSize);
	ld_ptr(A0, SP, 3* wordSize);
	ld_ptr(A1, SP, 2* wordSize);
	ld_ptr(AT, SP, 1* wordSize);
	ld_ptr(T9, SP, 0* wordSize);
	addiu(SP, SP, 7 * wordSize);
}

// used registers :  T0, T1
void MacroAssembler::verify_oop_subroutine() {
  // RA: ra
  // A0: char* error message    
  // A1: oop   object to verify 

  Label exit, error;
  // increment counter
  li(T0, (long)StubRoutines::verify_oop_count_addr());
  lw(AT, T0, 0);
#ifdef _LP64
//FIXME, aoqi: rewrite addi, addu, etc in 64bits mode.
  daddi(AT, AT, 1);
#else
  addi(AT, AT, 1);
#endif
  sw(AT, T0, 0);

  // make sure object is 'reasonable'
  beq(A1, R0, exit);         // if obj is NULL it is ok
  delayed()->nop();

  // Check if the oop is in the right area of memory
  //const int oop_mask = Universe::verify_oop_mask();
  //const int oop_bits = Universe::verify_oop_bits();
  const uintptr_t oop_mask = Universe::verify_oop_mask();
  const uintptr_t oop_bits = Universe::verify_oop_bits();
  li(AT, oop_mask);
  andr(T0, A1, AT);
  li(AT, oop_bits);
  bne(T0, AT, error);
  delayed()->nop();

  // make sure klass is 'reasonable'
  //add for compressedoops
  reinit_heapbase();
  //add for compressedoops
  load_klass(T0, A1);
  beq(T0, R0, error);                        // if klass is NULL it is broken
  delayed()->nop();
  #if 0
  //FIXME:wuhui.
  // Check if the klass is in the right area of memory
  //const int klass_mask = Universe::verify_klass_mask();
  //const int klass_bits = Universe::verify_klass_bits();
  const uintptr_t klass_mask = Universe::verify_klass_mask();
  const uintptr_t klass_bits = Universe::verify_klass_bits();

  li(AT, klass_mask);
  andr(T1, T0, AT);
  li(AT, klass_bits);
  bne(T1, AT, error);
  delayed()->nop();
  // make sure klass' klass is 'reasonable'
  //add for compressedoops
  load_klass(T0, T0);
  beq(T0, R0, error);  // if klass' klass is NULL it is broken
  delayed()->nop();

  li(AT, klass_mask);
  andr(T1, T0, AT);
  li(AT, klass_bits);
  bne(T1, AT, error);
  delayed()->nop();     // if klass not in right area of memory it is broken too.
#endif
  // return if everything seems ok
  bind(exit);

  jr(RA);
  delayed()->nop();

  // handle errors
  bind(error);
  pushad();
#ifndef _LP64
  addi(SP, SP, (-1) * wordSize);
#endif
  call(CAST_FROM_FN_PTR(address, MacroAssembler::debug), relocInfo::runtime_call_type);
  delayed()->nop();
#ifndef _LP64
  addiu(SP, SP, 1 * wordSize);
#endif
  popad();	
  jr(RA);
  delayed()->nop();
}

void MacroAssembler::verify_tlab(Register t1, Register t2) {
#ifdef ASSERT
  assert_different_registers(t1, t2, AT);
  if (UseTLAB && VerifyOops) {
    Label next, ok;

    get_thread(t1);

    ld_ptr(t2, t1, in_bytes(JavaThread::tlab_top_offset()));
    ld_ptr(AT, t1, in_bytes(JavaThread::tlab_start_offset()));
    sltu(AT, t2, AT);
    beq(AT, R0, next);
    delayed()->nop();

    stop("assert(top >= start)");

    bind(next);
    ld_ptr(AT, t1, in_bytes(JavaThread::tlab_end_offset()));
    sltu(AT, AT, t2);
    beq(AT, R0, ok);
    delayed()->nop();

    stop("assert(top <= end)");

    bind(ok);

    /*
       Label next, ok;
       Register t1 = rsi;
       Register thread_reg = NOT_LP64(rbx) LP64_ONLY(r15_thread);

       push(t1);
       NOT_LP64(push(thread_reg));
       NOT_LP64(get_thread(thread_reg));

       movptr(t1, Address(thread_reg, in_bytes(JavaThread::tlab_top_offset())));
       cmpptr(t1, Address(thread_reg, in_bytes(JavaThread::tlab_start_offset())));
       jcc(Assembler::aboveEqual, next);
       stop("assert(top >= start)");
       should_not_reach_here();

       bind(next);
       movptr(t1, Address(thread_reg, in_bytes(JavaThread::tlab_end_offset())));
       cmpptr(t1, Address(thread_reg, in_bytes(JavaThread::tlab_top_offset())));
       jcc(Assembler::aboveEqual, ok);
       stop("assert(top <= end)");
       should_not_reach_here();

       bind(ok);
       NOT_LP64(pop(thread_reg));
       pop(t1);
     */
  }
#endif
}
 RegisterOrConstant MacroAssembler::delayed_value_impl(intptr_t* delayed_value_addr,
                                                       Register tmp,
                                                       int offset) {
   intptr_t value = *delayed_value_addr;
   if (value != 0)
   return RegisterOrConstant(value + offset);
   AddressLiteral a(delayed_value_addr);
   // load indirectly to solve generation ordering problem
   //movptr(tmp, ExternalAddress((address) delayed_value_addr));
   //ld(tmp, a);
  /* #ifdef ASSERT
   { Label L;
     testptr(tmp, tmp);
     if (WizardMode) {
            jcc(Assembler::notZero, L);
            char* buf = new char[40];
            sprintf(buf, "DelayedValue="INTPTR_FORMAT, delayed_value_addr[1]);
            STOP(buf);
                      } else {
            jccb(Assembler::notZero, L);
            hlt();
                             }
     bind(L);
   }
   #endif*/
   if (offset != 0)
     daddi(tmp,tmp, offset);
 
   return RegisterOrConstant(tmp);
 }

void MacroAssembler::hswap(Register reg) {
  //andi(reg, reg, 0xffff);
  srl(AT, reg, 8);
  sll(reg, reg, 24);
  sra(reg, reg, 16);
  orr(reg, reg, AT);
}

void MacroAssembler::huswap(Register reg) {
#ifdef _LP64
  dsrl(AT, reg, 8);
  dsll(reg, reg, 24);
  dsrl(reg, reg, 16);
  orr(reg, reg, AT);
  andi(reg, reg, 0xffff);
#else
  //andi(reg, reg, 0xffff);
  srl(AT, reg, 8);
  sll(reg, reg, 24);
  srl(reg, reg, 16);
  orr(reg, reg, AT);
#endif
}

// something funny to do this will only one more register AT
// by yjl 6/29/2005
void MacroAssembler::swap(Register reg) {
	srl(AT, reg, 8);
	sll(reg, reg, 24);
	orr(reg, reg, AT);
	//reg : 4 1 2 3
	srl(AT, AT, 16);
	xorr(AT, AT, reg);
	andi(AT, AT, 0xff);
	//AT : 0 0 0 1^3);
	xorr(reg, reg, AT);
	//reg : 4 1 2 1
	sll(AT, AT, 16);
	xorr(reg, reg, AT);
	//reg : 4 3 2 1
}

#ifdef _LP64

/* do 32-bit CAS using MIPS64 lld/scd

  Jin: cas_int should only compare 32-bits of the memory value.
       However, lld/scd will do 64-bit operation, which violates the intention of cas_int.
       To simulate a 32-bit atomic operation, the value loaded with LLD should be split into 
       tow halves, and only the low-32 bits is compared. If equals, the low-32 bits of newval,
       plus the high-32 bits or memory value, are stored togethor with SCD.

Example:

      double d = 3.1415926;
      System.err.println("hello" + d);
  
  sun.misc.FloatingDecimal$1.<init>()
   |
   `- java.util.concurrent.atomic.AtomicInteger::compareAndSet()

  38 cas_int [a7a7|J] [a0|I] [a6|I]   
// a0: 0xffffffffe8ea9f63 pc: 0x55647f3354
// a6: 0x4ab325aa

again:
   0x00000055647f3c5c: lld at, 0x0(a7)                          ; 64-bit load, "0xe8ea9f63"

   0x00000055647f3c60: sll t9, at, 0                            ; t9: low-32 bits (sign extended)
   0x00000055647f3c64: dsrl32 t8, at, 0                         ; t8: high-32 bits
   0x00000055647f3c68: dsll32 t8, t8, 0
   0x00000055647f3c6c: bne t9, a0, 0x00000055647f3c9c           ; goto nequal
   0x00000055647f3c70: sll zero, zero, 0

   0x00000055647f3c74: ori v1, zero, 0xffffffff                 ; v1: low-32 bits of newval (sign unextended)
   0x00000055647f3c78: dsll v1, v1, 16                          ; v1 = a6 & 0xFFFFFFFF;
   0x00000055647f3c7c: ori v1, v1, 0xffffffff
   0x00000055647f3c80: and v1, a6, v1 
   0x00000055647f3c84: or at, t8, v1 
   0x00000055647f3c88: scd at, 0x0(a7)
   0x00000055647f3c8c: beq at, zero, 0x00000055647f3c5c         ; goto again
   0x00000055647f3c90: sll zero, zero, 0
   0x00000055647f3c94: beq zero, zero, 0x00000055647f45ac       ; goto done
   0x00000055647f3c98: sll zero, zero, 0
nequal:
   0x00000055647f45a4: dadd a0, t9, zero
   0x00000055647f45a8: dadd at, zero, zero
done:
*/

void MacroAssembler::cmpxchg32(Register x_reg, Address dest, Register c_reg) {
#if 0
  Label done, again, nequal;
  bind(again);

  sync();
  lld(AT, dest);

  /* T9:  32 bits, sign extended
   * V1: low 32 bits, sign unextended
   * T8: high 32 bits (may be another variables's space)
   */
  sll(T9, AT, 0);	// Use 32-bit sll to extend bit 31
  dsrl32(T8, AT, 0);
  dsll32(T8, T8, 0);

  bne(T9, c_reg, nequal);
  delayed()->nop(); 

  ori(V1, R0, 0xFFFF);
  dsll(V1, V1, 16);
  ori(V1, V1, 0xFFFF);
  andr(V1, x_reg, V1);
  orr(AT, T8, V1);
  scd(AT, dest);
  beq(AT, R0, again);
  delayed()->nop();
  b(done);
  delayed()->nop();

  // not xchged
  bind(nequal);
  move(c_reg, T9);
  move(AT, R0);

  bind(done);
#else

  /* 2012/11/11 Jin: MIPS64 can use ll/sc for 32-bit atomic memory access */
  Label done, again, nequal;

  bind(again);

  sync();
  ll(AT, dest);
  bne(AT, c_reg, nequal);
  delayed()->nop(); 

  move(AT, x_reg);
  sc(AT, dest);
  beq(AT, R0, again);
  delayed()->nop();
  b(done);
  delayed()->nop();

  // not xchged
  bind(nequal);
  sync();
  move(c_reg, AT);
  move(AT, R0);

  bind(done);
#endif
}
#endif	// cmpxchg32

void MacroAssembler::cmpxchg(Register x_reg, Address dest, Register c_reg) {
  Label done, again, nequal;

  bind(again);
#ifdef _LP64
  sync();
  lld(AT, dest);
#else
  sync();
  ll(AT, dest);
#endif
  bne(AT, c_reg, nequal);
  delayed()->nop(); 

  move(AT, x_reg);
#ifdef _LP64
  scd(AT, dest);
#else
  sc(AT, dest);
#endif
  beq(AT, R0, again);
  delayed()->nop();
  b(done);
  delayed()->nop();

  // not xchged
  bind(nequal);
  sync();
  move(c_reg, AT);
  move(AT, R0);

  bind(done);
}

void MacroAssembler::cmpxchg8(Register x_regLo, Register x_regHi, Address dest, Register c_regLo, Register c_regHi) {
	Label done, again, nequal;

	Register x_reg = x_regLo;
	dsll32(x_regHi, x_regHi, 0);
	dsll32(x_regLo, x_regLo, 0);
	dsrl32(x_regLo, x_regLo, 0);
	orr(x_reg, x_regLo, x_regHi);

	Register c_reg = c_regLo;
	dsll32(c_regHi, c_regHi, 0);
	dsll32(c_regLo, c_regLo, 0);
	dsrl32(c_regLo, c_regLo, 0);
	orr(c_reg, c_regLo, c_regHi);

	bind(again);

	sync();
	lld(AT, dest);
	bne(AT, c_reg, nequal);
	delayed()->nop(); 

	//move(AT, x_reg);
	dadd(AT, x_reg, R0);
	scd(AT, dest);
	beq(AT, R0, again);
	delayed()->nop();
	b(done);
	delayed()->nop();

	// not xchged
	bind(nequal);
	sync();
	//move(c_reg, AT);
	//move(AT, R0);
	dadd(c_reg, AT, R0);
	dadd(AT, R0, R0);
	bind(done);
}

// be sure the three register is different
void MacroAssembler::rem_s(FloatRegister fd, FloatRegister fs, FloatRegister ft, FloatRegister tmp) {    
  assert_different_registers(tmp, fs, ft); 
	div_s(tmp, fs, ft); 
	trunc_l_s(tmp, tmp); 
	cvt_s_l(tmp, tmp); 
	mul_s(tmp, tmp, ft); 
	sub_s(fd, fs, tmp); 
}

// be sure the three register is different
void MacroAssembler::rem_d(FloatRegister fd, FloatRegister fs, FloatRegister ft, FloatRegister tmp) {    
	assert_different_registers(tmp, fs, ft); 
	div_d(tmp, fs, ft); 
	trunc_l_d(tmp, tmp); 
	cvt_d_l(tmp, tmp); 
	mul_d(tmp, tmp, ft); 
	sub_d(fd, fs, tmp); 
}

// Fast_Lock and Fast_Unlock used by C2

// Because the transitions from emitted code to the runtime
// monitorenter/exit helper stubs are so slow it's critical that
// we inline both the stack-locking fast-path and the inflated fast path.
//
// See also: cmpFastLock and cmpFastUnlock.
//
// What follows is a specialized inline transliteration of the code
// in slow_enter() and slow_exit().  If we're concerned about I$ bloat
// another option would be to emit TrySlowEnter and TrySlowExit methods
// at startup-time.  These methods would accept arguments as
// (rax,=Obj, rbx=Self, rcx=box, rdx=Scratch) and return success-failure
// indications in the icc.ZFlag.  Fast_Lock and Fast_Unlock would simply
// marshal the arguments and emit calls to TrySlowEnter and TrySlowExit.
// In practice, however, the # of lock sites is bounded and is usually small.
// Besides the call overhead, TrySlowEnter and TrySlowExit might suffer
// if the processor uses simple bimodal branch predictors keyed by EIP
// Since the helper routines would be called from multiple synchronization
// sites.
//
// An even better approach would be write "MonitorEnter()" and "MonitorExit()"
// in java - using j.u.c and unsafe - and just bind the lock and unlock sites
// to those specialized methods.  That'd give us a mostly platform-independent
// implementation that the JITs could optimize and inline at their pleasure.
// Done correctly, the only time we'd need to cross to native could would be
// to park() or unpark() threads.  We'd also need a few more unsafe operators
// to (a) prevent compiler-JIT reordering of non-volatile accesses, and
// (b) explicit barriers or fence operations.
//
// TODO:
//
// *  Arrange for C2 to pass "Self" into Fast_Lock and Fast_Unlock in one of the registers (scr).
//    This avoids manifesting the Self pointer in the Fast_Lock and Fast_Unlock terminals.
//    Given TLAB allocation, Self is usually manifested in a register, so passing it into
//    the lock operators would typically be faster than reifying Self.
//
// *  Ideally I'd define the primitives as:
//       fast_lock   (nax Obj, nax box, EAX tmp, nax scr) where box, tmp and scr are KILLED.
//       fast_unlock (nax Obj, EAX box, nax tmp) where box and tmp are KILLED
//    Unfortunately ADLC bugs prevent us from expressing the ideal form.
//    Instead, we're stuck with a rather awkward and brittle register assignments below.
//    Furthermore the register assignments are overconstrained, possibly resulting in
//    sub-optimal code near the synchronization site.
//
// *  Eliminate the sp-proximity tests and just use "== Self" tests instead.
//    Alternately, use a better sp-proximity test.
//
// *  Currently ObjectMonitor._Owner can hold either an sp value or a (THREAD *) value.
//    Either one is sufficient to uniquely identify a thread.
//    TODO: eliminate use of sp in _owner and use get_thread(tr) instead.
//
// *  Intrinsify notify() and notifyAll() for the common cases where the
//    object is locked by the calling thread but the waitlist is empty.
//    avoid the expensive JNI call to JVM_Notify() and JVM_NotifyAll().
//
// *  use jccb and jmpb instead of jcc and jmp to improve code density.
//    But beware of excessive branch density on AMD Opterons.
//
// *  Both Fast_Lock and Fast_Unlock set the ICC.ZF to indicate success
//    or failure of the fast-path.  If the fast-path fails then we pass
//    control to the slow-path, typically in C.  In Fast_Lock and
//    Fast_Unlock we often branch to DONE_LABEL, just to find that C2
//    will emit a conditional branch immediately after the node.
//    So we have branches to branches and lots of ICC.ZF games.
//    Instead, it might be better to have C2 pass a "FailureLabel"
//    into Fast_Lock and Fast_Unlock.  In the case of success, control
//    will drop through the node.  ICC.ZF is undefined at exit.
//    In the case of failure, the node will branch directly to the
//    FailureLabel


// obj: object to lock
// box: on-stack box address (displaced header location) - KILLED
// rax,: tmp -- KILLED
// scr: tmp -- KILLED
void MacroAssembler::fast_lock(Register objReg, Register boxReg, Register tmpReg, Register scrReg) {

  tmpReg = T8;
  scrReg = S7;

  // Ensure the register assignents are disjoint
  guarantee (objReg != boxReg, "") ;
  guarantee (objReg != tmpReg, "") ;
  guarantee (objReg != scrReg, "") ;
  guarantee (boxReg != tmpReg, "") ;
  guarantee (boxReg != scrReg, "") ;


  block_comment("FastLock");
  /*
     __ move(AT, 0x0);
     return;
     */
  if (PrintBiasedLockingStatistics) {
    push(tmpReg);
    atomic_inc32((address)BiasedLocking::total_entry_count_addr(), 1, AT, tmpReg);
    pop(tmpReg);
  }

  if (EmitSync & 1) {
    // set box->dhw = unused_mark (3)
    // Force all sync thru slow-path: slow_enter() and slow_exit()
    move (AT, (int32_t)intptr_t(markOopDesc::unused_mark()));
    sd(AT, Address(boxReg, 0));
    move (AT, (int32_t)0) ;	// Eflags.ZF = 0
  } else
    if (EmitSync & 2) {
      Label DONE_LABEL ;
      if (UseBiasedLocking) {
        // Note: tmpReg maps to the swap_reg argument and scrReg to the tmp_reg argument.
        biased_locking_enter(boxReg, objReg, tmpReg, scrReg, false, DONE_LABEL, NULL);
      }

      ld(tmpReg, Address(objReg, 0)) ;          // fetch markword
      ori(tmpReg, tmpReg, 0x1);
      sd(tmpReg, Address(boxReg, 0));           // Anticipate successful CAS

      cmpxchg(boxReg, Address(objReg, 0), tmpReg);          // Updates tmpReg
      bne(AT, R0, DONE_LABEL);
      delayed()->nop();

      // Recursive locking
      dsubu(tmpReg, tmpReg, SP);
      li(AT, (7 - os::vm_page_size() ));
      andr(tmpReg, tmpReg, AT);
      sd(tmpReg, Address(boxReg, 0));
      bind(DONE_LABEL) ;
    } else {
      // Possible cases that we'll encounter in fast_lock
      // ------------------------------------------------
      // * Inflated
      //    -- unlocked
      //    -- Locked
      //       = by self
      //       = by other
      // * biased
      //    -- by Self
      //    -- by other
      // * neutral
      // * stack-locked
      //    -- by self
      //       = sp-proximity test hits
      //       = sp-proximity test generates false-negative
      //    -- by other
      //

      Label IsInflated, DONE_LABEL, PopDone ;

      // TODO: optimize away redundant LDs of obj->mark and improve the markword triage
      // order to reduce the number of conditional branches in the most common cases.
      // Beware -- there's a subtle invariant that fetch of the markword
      // at [FETCH], below, will never observe a biased encoding (*101b).
      // If this invariant is not held we risk exclusion (safety) failure.
      if (UseBiasedLocking && !UseOptoBiasInlining) {
        biased_locking_enter(boxReg, objReg, tmpReg, scrReg, false, DONE_LABEL, NULL);
      }

      ld(tmpReg, Address(objReg, 0)) ;         //Fetch the markword of the object.
      andi(AT, tmpReg, 0x02);                  //If AT == 0x02 ==> the object is inflated, will not use the fast lock method.
      bne(AT, R0, IsInflated);                      // Inflated v (Stack-locked or neutral)
      delayed()->nop();

      // Attempt stack-locking ...
      ori (tmpReg, tmpReg, 0x1);
      sd(tmpReg, Address(boxReg, 0));          // Anticipate successful CAS

      cmpxchg(boxReg, Address(objReg, 0), tmpReg);           // Updates tmpReg

      if (PrintBiasedLockingStatistics) {
        Label L;
        beq(AT, R0, L);
        delayed()->nop();
        push(T0);
        push(T1);
        atomic_inc32((address)BiasedLocking::fast_path_entry_count_addr(), 1, T0, T1);
        pop(T1);
        pop(T0);
        bind(L);
      }
      bne(AT, R0, DONE_LABEL);
      delayed()->nop();

      // Recursive locking
      dsubu(tmpReg, tmpReg, SP);
      li(AT, 7 - os::vm_page_size() );
      andr(tmpReg, tmpReg, AT);
      sd(tmpReg, Address(boxReg, 0));
      if (PrintBiasedLockingStatistics) {
        Label L;
        // tmpReg == 0 => BiasedLocking::_fast_path_entry_count++
        bne(tmpReg, R0, L);
        delayed()->nop();
        push(T0);
        push(T1);
        atomic_inc32((address)BiasedLocking::fast_path_entry_count_addr(), 1, T0, T1);
        pop(T1);
        pop(T0);
        bind(L);
      }
      sltiu(AT, tmpReg, 1); /* AT = (tmpReg == 0) ? 1 : 0 */

      b(DONE_LABEL) ;
      delayed()->nop();

      bind(IsInflated) ;

      // TODO: someday avoid the ST-before-CAS penalty by
      // relocating (deferring) the following ST.
      // We should also think about trying a CAS without having
      // fetched _owner.  If the CAS is successful we may
      // avoid an RTO->RTS upgrade on the $line.
      // Without cast to int32_t a movptr will destroy r10 which is typically obj
      li(AT, (int32_t)intptr_t(markOopDesc::unused_mark()));
      sd(AT, Address(boxReg, 0));

      move(boxReg, tmpReg) ;
      ld(tmpReg, Address(tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
      sltiu(AT, tmpReg, 1);  /* Jin: AT = !tmpReg; */
      bne(tmpReg, R0, DONE_LABEL);
      delayed()->nop();

      cmpxchg(TREG, Address(boxReg, ObjectMonitor::owner_offset_in_bytes()-2), tmpReg) ;
      // Intentional fall-through into DONE_LABEL ...


      // DONE_LABEL is a hot target - we'd really like to place it at the
      // start of cache line by padding with NOPs.
      // See the AMD and Intel software optimization manuals for the
      // most efficient "long" NOP encodings.
      // Unfortunately none of our alignment mechanisms suffice.
      bind(DONE_LABEL);

      // Avoid branch-to-branch on AMD processors
      // This appears to be superstition.
      if (EmitSync & 32) nop() ;


      // At DONE_LABEL the icc ZFlag is set as follows ...
      // Fast_Unlock uses the same protocol.
      // ZFlag == 1 -> Success
      // ZFlag == 0 -> Failure - force control through the slow-path
    }
}

// obj: object to unlock
// box: box address (displaced header location), killed.  Must be EAX.
// rbx,: killed tmp; cannot be obj nor box.
//
// Some commentary on balanced locking:
//
// Fast_Lock and Fast_Unlock are emitted only for provably balanced lock sites.
// Methods that don't have provably balanced locking are forced to run in the
// interpreter - such methods won't be compiled to use fast_lock and fast_unlock.
// The interpreter provides two properties:
// I1:  At return-time the interpreter automatically and quietly unlocks any
//      objects acquired the current activation (frame).  Recall that the
//      interpreter maintains an on-stack list of locks currently held by
//      a frame.
// I2:  If a method attempts to unlock an object that is not held by the
//      the frame the interpreter throws IMSX.
//
// Lets say A(), which has provably balanced locking, acquires O and then calls B().
// B() doesn't have provably balanced locking so it runs in the interpreter.
// Control returns to A() and A() unlocks O.  By I1 and I2, above, we know that O
// is still locked by A().
//
// The only other source of unbalanced locking would be JNI.  The "Java Native Interface:
// Programmer's Guide and Specification" claims that an object locked by jni_monitorenter
// should not be unlocked by "normal" java-level locking and vice-versa.  The specification
// doesn't specify what will occur if a program engages in such mixed-mode locking, however.

void MacroAssembler::fast_unlock(Register objReg, Register boxReg, Register tmpReg) {

  tmpReg = T8;

  guarantee (objReg != boxReg, "") ;
  guarantee (objReg != tmpReg, "") ;
  guarantee (boxReg != tmpReg, "") ;



  block_comment("FastUnlock");

  /*
     move(AT, 0x0);
     return;
     */

  if (EmitSync & 4) {
    // Disable - inhibit all inlining.  Force control through the slow-path
    move(AT, R0);
  } else
    if (EmitSync & 8) {
      Label DONE_LABEL ;
      if (UseBiasedLocking) {
        biased_locking_exit(objReg, tmpReg, DONE_LABEL);
      }
      // classic stack-locking code ...
      ld(tmpReg, Address(boxReg, 0)) ;
      beq(tmpReg, R0, DONE_LABEL) ;
      move(AT, 0x1);  // delay slot

      cmpxchg(tmpReg, Address(objReg, 0), boxReg);          // Uses EAX which is box
      bind(DONE_LABEL);
    } else {
      Label DONE_LABEL, Stacked, CheckSucc, Inflated ;

      // Critically, the biased locking test must have precedence over
      // and appear before the (box->dhw == 0) recursive stack-lock test.
      if (UseBiasedLocking && !UseOptoBiasInlining) {
        biased_locking_exit(objReg, tmpReg, DONE_LABEL);
      }

      ld(tmpReg, Address(objReg, 0)) ;       // Examine the object's markword
      ld(AT, Address(boxReg, 0)) ;            // Examine the displaced header
      beq(AT, R0, DONE_LABEL) ;      // 0 indicates recursive stack-lock
      //move(AT, 0x1);
      //delayed()->nop();
      delayed()->daddiu(AT, R0, 0x1);

      andi(AT, tmpReg, markOopDesc::monitor_value) ;                     // Inflated?
      beq(AT, R0, Stacked) ;                     // Inflated?
      delayed()->nop();

      bind(Inflated) ;
      // It's inflated.
      // Despite our balanced locking property we still check that m->_owner == Self
      // as java routines or native JNI code called by this thread might
      // have released the lock.
      // Refer to the comments in synchronizer.cpp for how we might encode extra
      // state in _succ so we can avoid fetching EntryList|cxq.
      //
      // I'd like to add more cases in fast_lock() and fast_unlock() --
      // such as recursive enter and exit -- but we have to be wary of
      // I$ bloat, T$ effects and BP$ effects.
      //
      // If there's no contention try a 1-0 exit.  That is, exit without
      // a costly MEMBAR or CAS.  See synchronizer.cpp for details on how
      // we detect and recover from the race that the 1-0 exit admits.
      //
      // Conceptually Fast_Unlock() must execute a STST|LDST "release" barrier
      // before it STs null into _owner, releasing the lock.  Updates
      // to data protected by the critical section must be visible before
      // we drop the lock (and thus before any other thread could acquire
      // the lock and observe the fields protected by the lock).
      // IA32's memory-model is SPO, so STs are ordered with respect to
      // each other and there's no need for an explicit barrier (fence).
      // See also http://gee.cs.oswego.edu/dl/jmm/cookbook.html.
#ifdef OPT_THREAD
      move(boxReg, TREG);
#else
      get_thread (boxReg) ;
#endif

#ifndef _LP64

      // Note that we could employ various encoding schemes to reduce
      // the number of loads below (currently 4) to just 2 or 3.
      // Refer to the comments in synchronizer.cpp.
      // In practice the chain of fetches doesn't seem to impact performance, however.
      if ((EmitSync & 65536) == 0 && (EmitSync & 256)) {
        // Attempt to reduce branch density - AMD's branch predictor.
        ld(AT, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
        xorr(boxReg, boxReg, AT);

        ld(AT, Address (tmpReg, ObjectMonitor::recursions_offset_in_bytes()-2)) ;
        orr(boxReg, boxReg, AT);

        ld(AT, Address (tmpReg, ObjectMonitor::EntryList_offset_in_bytes()-2)) ;
        orr(boxReg, boxReg, AT);

        ld(AT, Address (tmpReg, ObjectMonitor::cxq_offset_in_bytes()-2)) ;
        orr(boxReg, boxReg, AT);

        bne(boxReg, R0, DONE_LABEL);
        move(AT, R0);	/* delay slot */

        sw(R0, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
        b(DONE_LABEL);
        move(AT, 0x1);	/* delay slot */
      } else {
        ld(AT, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
        xorr(boxReg, boxReg, AT);

        ld(AT, Address (tmpReg, ObjectMonitor::recursions_offset_in_bytes()-2)) ;
        orr(boxReg, boxReg, AT);

        bne(boxReg, R0, DONE_LABEL);
        move(AT, R0);	/* delay slot */

        ld(boxReg, Address (tmpReg, ObjectMonitor::EntryList_offset_in_bytes()-2)) ;
        ld(AT, Address (tmpReg, ObjectMonitor::cxq_offset_in_bytes()-2)) ;
        orr(boxReg, boxReg, AT);

        bne(boxReg, R0, CheckSucc);
        move(AT, R0);	/* delay slot */

        sd(R0, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
        b(DONE_LABEL);
        move(AT, 0x1);	/* delay slot */
      }

      // The Following code fragment (EmitSync & 65536) improves the performance of
      // contended applications and contended synchronization microbenchmarks.
      // Unfortunately the emission of the code - even though not executed - causes regressions
      // in scimark and jetstream, evidently because of $ effects.  Replacing the code
      // with an equal number of never-executed NOPs results in the same regression.
      // We leave it off by default.

      if ((EmitSync & 65536) != 0) {
        Label LSuccess, LGoSlowPath ;

        bind(CheckSucc) ;

        // Optional pre-test ... it's safe to elide this
        if ((EmitSync & 16) == 0) {
          ld(AT, Address (tmpReg, ObjectMonitor::succ_offset_in_bytes()-2)) ;
          beq(AT, R0, LGoSlowPath);
          delayed()->nop();
        }

        // We have a classic Dekker-style idiom:
        //    ST m->_owner = 0 ; MEMBAR; LD m->_succ
        // There are a number of ways to implement the barrier:
        // (1) lock:andl &m->_owner, 0
        //     is fast, but mask doesn't currently support the "ANDL M,IMM32" form.
        //     LOCK: ANDL [ebx+Offset(_Owner)-2], 0
        //     Encodes as 81 31 OFF32 IMM32 or 83 63 OFF8 IMM8
        // (2) If supported, an explicit MFENCE is appealing.
        //     In older IA32 processors MFENCE is slower than lock:add or xchg
        //     particularly if the write-buffer is full as might be the case if
        //     if stores closely precede the fence or fence-equivalent instruction.
        //     In more modern implementations MFENCE appears faster, however.
        // (3) In lieu of an explicit fence, use lock:addl to the top-of-stack
        //     The $lines underlying the top-of-stack should be in M-state.
        //     The locked add instruction is serializing, of course.
        // (4) Use xchg, which is serializing
        //     mov boxReg, 0; xchgl boxReg, [tmpReg + Offset(_owner)-2] also works
        // (5) ST m->_owner = 0 and then execute lock:orl &m->_succ, 0.
        //     The integer condition codes will tell us if succ was 0.
        //     Since _succ and _owner should reside in the same $line and
        //     we just stored into _owner, it's likely that the $line
        //     remains in M-state for the lock:orl.
        //
        // We currently use (3), although it's likely that switching to (2)
        // is correct for the future.

        sd(R0, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;

        // Ratify _succ remains non-null
        ld(AT, Address (tmpReg, ObjectMonitor::succ_offset_in_bytes()-2)) ;
        bne(AT, R0, LSuccess);
        delayed()->nop();		/* delay slot */
        /*
           masm.cmpptr(Address (tmpReg, ObjectMonitor::succ_offset_in_bytes()-2), 0) ;
           masm.jccb  (Assembler::notZero, LSuccess) ;
           */

        move(boxReg, R0) ;                  // box is really EAX

        cmpxchg(SP, Address(tmpReg, ObjectMonitor::owner_offset_in_bytes()-2), boxReg);
        beq(AT, R0, LSuccess);
        delayed()->nop();

        // Since we're low on registers we installed rsp as a placeholding in _owner.
        // Now install Self over rsp.  This is safe as we're transitioning from
        // non-null to non=null
        get_thread (boxReg) ;
        sd(boxReg, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
        // Intentional fall-through into LGoSlowPath ...

        bind(LGoSlowPath) ;
        ori(boxReg, boxReg, 1) ;                      // set ICC.ZF=0 to indicate failure
        b(DONE_LABEL) ;
        move(AT, R0) ;	/* delay slot */

        bind(LSuccess) ;
        move(boxReg, R0) ;                 // set ICC.ZF=1 to indicate success
        b(DONE_LABEL) ;
        move(AT, 0x1) ;	/* delay slot */
      }

      bind (Stacked) ;
      // It's not inflated and it's not recursively stack-locked and it's not biased.
      // It must be stack-locked.
      // Try to reset the header to displaced header.
      // The "box" value on the stack is stable, so we can reload
      // and be assured we observe the same value as above.
      ld(tmpReg, Address(boxReg, 0)) ;

      cmpxchg(tmpReg, Address(objReg, 0), boxReg); // Uses EAX which is box
      // Intention fall-thru into DONE_LABEL


      // DONE_LABEL is a hot target - we'd really like to place it at the
      // start of cache line by padding with NOPs.
      // See the AMD and Intel software optimization manuals for the
      // most efficient "long" NOP encodings.
      // Unfortunately none of our alignment mechanisms suffice.
      if ((EmitSync & 65536) == 0) {
        bind (CheckSucc) ;
      }
#else // _LP64
      // It's inflated
      ld(AT, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
      xorr(boxReg, boxReg, AT);

      ld(AT, Address (tmpReg, ObjectMonitor::recursions_offset_in_bytes()-2)) ;
      orr(boxReg, boxReg, AT);

      move(AT, R0);
      bne(boxReg, R0, DONE_LABEL);
      delayed()->nop();

      ld(boxReg, Address (tmpReg, ObjectMonitor::EntryList_offset_in_bytes()-2)) ;
      ld(AT, Address (tmpReg, ObjectMonitor::cxq_offset_in_bytes()-2)) ;
      orr(boxReg, boxReg, AT);

      move(AT, R0);
      bne(boxReg, R0, CheckSucc);
      delayed()->nop();

      sd(R0, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
      move(AT, 0x1);
      b(DONE_LABEL);
      delayed()->nop();


      if ((EmitSync & 65536) == 0) {
        Label LSuccess, LGoSlowPath ;
        bind (CheckSucc);
        ld(AT, Address (tmpReg, ObjectMonitor::succ_offset_in_bytes()-2)) ;
        beq(AT, R0, LGoSlowPath);
        delayed()->nop();

        // I'd much rather use lock:andl m->_owner, 0 as it's faster than the
        // the explicit ST;MEMBAR combination, but masm doesn't currently support
        // "ANDQ M,IMM".  Don't use MFENCE here.  lock:add to TOS, xchg, etc
        // are all faster when the write buffer is populated.
        sd(R0, Address (tmpReg, ObjectMonitor::owner_offset_in_bytes()-2)) ;
        if (os::is_MP()) {
          // lock (); 
          //addl (Address(rsp, 0), 0); //?
        }
        ld(AT, Address (tmpReg, ObjectMonitor::succ_offset_in_bytes()-2)) ;
        bne(AT, R0, LSuccess);
        delayed()->nop();

        move(boxReg, R0) ;                  // box is really EAX
        //if (os::is_MP()) { lock(); }
        cmpxchg(SP, Address(tmpReg, ObjectMonitor::owner_offset_in_bytes()-2), boxReg);
        beq(AT, R0, LSuccess);
        delayed()->nop();
        // Intentional fall-through into slow-path

        bind  (LGoSlowPath);
        ori(boxReg, boxReg, 1) ;                      // set ICC.ZF=0 to indicate failure
        move(AT, R0);
        b(DONE_LABEL) ;
        delayed()->nop();


        bind  (LSuccess);
        move(boxReg, R0) ;                 // set ICC.ZF=1 to indicate success
        move(AT, 0x1) ;
        b(DONE_LABEL) ;
        delayed()->nop();
      }

      bind  (Stacked);
      ld(tmpReg, Address(boxReg, 0)) ;
      //if (os::is_MP()) { lock(); }
      cmpxchg(tmpReg, Address(objReg, 0), boxReg); // Uses EAX which is box

      if (EmitSync & 65536) {
        bind (CheckSucc);
      }
#endif

      bind(DONE_LABEL);

      // Avoid branch to branch on AMD processors
      if (EmitSync & 32768) { nop() ; }
    }
}

class ControlWord {
				public:
								int32_t _value;

  int  rounding_control() const        { return  (_value >> 10) & 3      ; }
  int  precision_control() const       { return  (_value >>  8) & 3      ; }
  bool precision() const               { return ((_value >>  5) & 1) != 0; }
  bool underflow() const               { return ((_value >>  4) & 1) != 0; }
  bool overflow() const                { return ((_value >>  3) & 1) != 0; }
  bool zero_divide() const             { return ((_value >>  2) & 1) != 0; }
  bool denormalized() const            { return ((_value >>  1) & 1) != 0; }
  bool invalid() const                 { return ((_value >>  0) & 1) != 0; }

  void print() const {
    // rounding control
    const char* rc;
    switch (rounding_control()) {
      case 0: rc = "round near"; break;
      case 1: rc = "round down"; break;
      case 2: rc = "round up  "; break;
      case 3: rc = "chop      "; break;
    };
    // precision control
    const char* pc;
    switch (precision_control()) {
      case 0: pc = "24 bits "; break;
      case 1: pc = "reserved"; break;
      case 2: pc = "53 bits "; break;
      case 3: pc = "64 bits "; break;
    };
    // flags
    char f[9];
    f[0] = ' ';
    f[1] = ' ';
    f[2] = (precision   ()) ? 'P' : 'p';
    f[3] = (underflow   ()) ? 'U' : 'u';
    f[4] = (overflow    ()) ? 'O' : 'o';
    f[5] = (zero_divide ()) ? 'Z' : 'z';
    f[6] = (denormalized()) ? 'D' : 'd';
    f[7] = (invalid     ()) ? 'I' : 'i';
    f[8] = '\x0';
    // output
    printf("%04x  masks = %s, %s, %s", _value & 0xFFFF, f, rc, pc);
  }

};

class StatusWord {
 public:
  int32_t _value;

  bool busy() const                    { return ((_value >> 15) & 1) != 0; }
  bool C3() const                      { return ((_value >> 14) & 1) != 0; }
  bool C2() const                      { return ((_value >> 10) & 1) != 0; }
  bool C1() const                      { return ((_value >>  9) & 1) != 0; }
  bool C0() const                      { return ((_value >>  8) & 1) != 0; }
  int  top() const                     { return  (_value >> 11) & 7      ; }
  bool error_status() const            { return ((_value >>  7) & 1) != 0; }
  bool stack_fault() const             { return ((_value >>  6) & 1) != 0; }
  bool precision() const               { return ((_value >>  5) & 1) != 0; }
  bool underflow() const               { return ((_value >>  4) & 1) != 0; }
  bool overflow() const                { return ((_value >>  3) & 1) != 0; }
  bool zero_divide() const             { return ((_value >>  2) & 1) != 0; }
  bool denormalized() const            { return ((_value >>  1) & 1) != 0; }
  bool invalid() const                 { return ((_value >>  0) & 1) != 0; }

  void print() const {
    // condition codes
    char c[5];
    c[0] = (C3()) ? '3' : '-';
    c[1] = (C2()) ? '2' : '-';
    c[2] = (C1()) ? '1' : '-';
    c[3] = (C0()) ? '0' : '-';
    c[4] = '\x0';
    // flags
    char f[9];
    f[0] = (error_status()) ? 'E' : '-';
    f[1] = (stack_fault ()) ? 'S' : '-';
    f[2] = (precision   ()) ? 'P' : '-';
    f[3] = (underflow   ()) ? 'U' : '-';
    f[4] = (overflow    ()) ? 'O' : '-';
    f[5] = (zero_divide ()) ? 'Z' : '-';
    f[6] = (denormalized()) ? 'D' : '-';
    f[7] = (invalid     ()) ? 'I' : '-';
    f[8] = '\x0';
    // output
    printf("%04x  flags = %s, cc =  %s, top = %d", _value & 0xFFFF, f, c, top());
  }

};

class TagWord {
 public:
  int32_t _value;

  int tag_at(int i) const              { return (_value >> (i*2)) & 3; }

  void print() const {
    printf("%04x", _value & 0xFFFF);
  }

};

class FPU_Register {
 public:
  int32_t _m0;
  int32_t _m1;
  int16_t _ex;

  bool is_indefinite() const           {
    return _ex == -1 && _m1 == (int32_t)0xC0000000 && _m0 == 0;
  }

  void print() const {
    char  sign = (_ex < 0) ? '-' : '+';
    const char* kind = (_ex == 0x7FFF || _ex == (int16_t)-1) ? "NaN" : "   ";
    printf("%c%04hx.%08x%08x  %s", sign, _ex, _m1, _m0, kind);
  };

};

class FPU_State {
 public:
  enum {
    register_size       = 10,
    number_of_registers =  8,
    register_mask       =  7
  };

  ControlWord  _control_word;
  StatusWord   _status_word;
  TagWord      _tag_word;
  int32_t      _error_offset;
  int32_t      _error_selector;
  int32_t      _data_offset;
  int32_t      _data_selector;
  int8_t       _register[register_size * number_of_registers];

  int tag_for_st(int i) const          { return _tag_word.tag_at((_status_word.top() + i) & register_mask); }
  FPU_Register* st(int i) const        { return (FPU_Register*)&_register[register_size * i]; }

  const char* tag_as_string(int tag) const {
    switch (tag) {
      case 0: return "valid";
      case 1: return "zero";
      case 2: return "special";
      case 3: return "empty";
    }
    ShouldNotReachHere();
    return NULL;
  }

  void print() const {
    // print computation registers
    { int t = _status_word.top();
      for (int i = 0; i < number_of_registers; i++) {
        int j = (i - t) & register_mask;
        printf("%c r%d = ST%d = ", (j == 0 ? '*' : ' '), i, j);
        st(j)->print();
        printf(" %s\n", tag_as_string(_tag_word.tag_at(i)));
      }
    }
    printf("\n");
    // print control registers
    printf("ctrl = "); _control_word.print(); printf("\n");
    printf("stat = "); _status_word .print(); printf("\n");
    printf("tags = "); _tag_word    .print(); printf("\n");
  }

};

class Flag_Register {
 public:
  int32_t _value;

  bool overflow() const                { return ((_value >> 11) & 1) != 0; }
  bool direction() const               { return ((_value >> 10) & 1) != 0; }
  bool sign() const                    { return ((_value >>  7) & 1) != 0; }
  bool zero() const                    { return ((_value >>  6) & 1) != 0; }
  bool auxiliary_carry() const         { return ((_value >>  4) & 1) != 0; }
  bool parity() const                  { return ((_value >>  2) & 1) != 0; }
  bool carry() const                   { return ((_value >>  0) & 1) != 0; }

  void print() const {
    // flags
    char f[8];
    f[0] = (overflow       ()) ? 'O' : '-';
    f[1] = (direction      ()) ? 'D' : '-';
    f[2] = (sign           ()) ? 'S' : '-';
    f[3] = (zero           ()) ? 'Z' : '-';
    f[4] = (auxiliary_carry()) ? 'A' : '-';
    f[5] = (parity         ()) ? 'P' : '-';
    f[6] = (carry          ()) ? 'C' : '-';
    f[7] = '\x0';
    // output
    printf("%08x  flags = %s", _value, f);
  }

};

class IU_Register {
 public:
  int32_t _value;

  void print() const {
    printf("%08x  %11d", _value, _value);
  }

};

class IU_State {
 public:
  Flag_Register _eflags;
  IU_Register   _rdi;
  IU_Register   _rsi;
  IU_Register   _rbp;
  IU_Register   _rsp;
  IU_Register   _rbx;
  IU_Register   _rdx;
  IU_Register   _rcx;
  IU_Register   _rax;

  void print() const {
    // computation registers
    printf("rax,  = "); _rax.print(); printf("\n");
    printf("rbx,  = "); _rbx.print(); printf("\n");
    printf("rcx  = "); _rcx.print(); printf("\n");
    printf("rdx  = "); _rdx.print(); printf("\n");
    printf("rdi  = "); _rdi.print(); printf("\n");
    printf("rsi  = "); _rsi.print(); printf("\n");
    printf("rbp,  = "); _rbp.print(); printf("\n");
    printf("rsp  = "); _rsp.print(); printf("\n");
    printf("\n");
    // control registers
    printf("flgs = "); _eflags.print(); printf("\n");
  }
};


class CPU_State {
 public:
  FPU_State _fpu_state;
  IU_State  _iu_state;

  void print() const {
    printf("--------------------------------------------------\n");
    _iu_state .print();
    printf("\n");
    _fpu_state.print();
    printf("--------------------------------------------------\n");
  }

};


/*
static void _print_CPU_state(CPU_State* state) {
  state->print();
};

void MacroAssembler::print_CPU_state() {
  push_CPU_state();
  push(rsp);                // pass CPU state
  call(RuntimeAddress(CAST_FROM_FN_PTR(address, _print_CPU_state)));
  addptr(rsp, wordSize);       // discard argument
  pop_CPU_state();
}
*/

void MacroAssembler::align(int modulus) {
	while (offset() % modulus != 0) nop();
}

#if 0
static bool _verify_FPU(int stack_depth, char* s, CPU_State* state) {
  static int counter = 0;
  FPU_State* fs = &state->_fpu_state;
  counter++;
  // For leaf calls, only verify that the top few elements remain empty.
  // We only need 1 empty at the top for C2 code.
  if( stack_depth < 0 ) {
    if( fs->tag_for_st(7) != 3 ) {
      printf("FPR7 not empty\n");
      state->print();
      assert(false, "error");
      return false;
    }
    return true;                // All other stack states do not matter
  }

  assert((fs->_control_word._value & 0xffff) == StubRoutines::_fpu_cntrl_wrd_std,
         "bad FPU control word");

  // compute stack depth
  int i = 0;
  while (i < FPU_State::number_of_registers && fs->tag_for_st(i)  < 3) i++;
  int d = i;
  while (i < FPU_State::number_of_registers && fs->tag_for_st(i) == 3) i++;
  // verify findings
  if (i != FPU_State::number_of_registers) {
    // stack not contiguous
    printf("%s: stack not contiguous at ST%d\n", s, i);
    state->print();
    assert(false, "error");
    return false;
  }
  // check if computed stack depth corresponds to expected stack depth
  if (stack_depth < 0) {
    // expected stack depth is -stack_depth or less
    if (d > -stack_depth) {
      // too many elements on the stack
      printf("%s: <= %d stack elements expected but found %d\n", s, -stack_depth, d);
      state->print();
      assert(false, "error");
      return false;
    }
  } else {
    // expected stack depth is stack_depth
    if (d != stack_depth) {
      // wrong stack depth
      printf("%s: %d stack elements expected but found %d\n", s, stack_depth, d);
      state->print();
      assert(false, "error");
      return false;
    }
  }
  // everything is cool
  return true;
}
#endif


void MacroAssembler::verify_FPU(int stack_depth, const char* s) {
	//FIXME aoqi
	// %%%%% need to implement this
	//Unimplemented();
	/*
	if (!VerifyFPU) return;
  push_CPU_state();
  push(rsp);                // pass CPU state
  ExternalAddress msg((address) s);
  // pass message string s
  pushptr(msg.addr());
  push(stack_depth);        // pass stack depth
  call(RuntimeAddress(CAST_FROM_FN_PTR(address, _verify_FPU)));
  addptr(rsp, 3 * wordSize);   // discard arguments
  // check for error
  { Label L;
    testl(rax, rax);
    jcc(Assembler::notZero, L);
    int3();                  // break if error condition
    bind(L);
  }
  pop_CPU_state();
	*/
}

#ifdef _LP64
Register caller_saved_registers[] = {AT, V0, V1, A0, A1, A2, A3, A4, A5, A6, A7, T0, T1, T2, T3, T8, T9, GP, RA, FP};

/* FIXME: Jin: In MIPS64, F0~23 are all caller-saved registers */
FloatRegister caller_saved_fpu_registers[] = {F0, F12, F13};
#else
Register caller_saved_registers[] = {AT, V0, V1, A0, A1, A2, A3, T4, T5, T6, T7, T0, T1, T2, T3, T8, T9, GP, RA, FP};

Register caller_saved_fpu_registers[] = {};
#endif

//We preserve all caller-saved register
void  MacroAssembler::pushad(){
  int i;

  /* Fixed-point registers */
  int len = sizeof(caller_saved_registers) / sizeof(caller_saved_registers[0]);
  daddi(SP, SP, -1 * len * wordSize);
  for (i = 0; i < len; i++)
  {
#ifdef _LP64
    sd(caller_saved_registers[i], SP, (len - i - 1) * wordSize);
#else
    sw(caller_saved_registers[i], SP, (len - i - 1) * wordSize);
#endif
  }

  /* Floating-point registers */
  len = sizeof(caller_saved_fpu_registers) / sizeof(caller_saved_fpu_registers[0]);
  daddi(SP, SP, -1 * len * wordSize);
  for (i = 0; i < len; i++)
  {
#ifdef _LP64
    sdc1(caller_saved_fpu_registers[i], SP, (len - i - 1) * wordSize);
#else
    swc1(caller_saved_fpu_registers[i], SP, (len - i - 1) * wordSize);
#endif
  }
};

void  MacroAssembler::popad(){
  int i;

  /* Floating-point registers */
  int len = sizeof(caller_saved_fpu_registers) / sizeof(caller_saved_fpu_registers[0]);
  for (i = 0; i < len; i++)
  {
#ifdef _LP64
    ldc1(caller_saved_fpu_registers[i], SP, (len - i - 1) * wordSize);
#else
    lwc1(caller_saved_fpu_registers[i], SP, (len - i - 1) * wordSize);
#endif
  }
  daddi(SP, SP, len * wordSize);

  /* Fixed-point registers */
  len = sizeof(caller_saved_registers) / sizeof(caller_saved_registers[0]);
  for (i = 0; i < len; i++)
  {
#ifdef _LP64
    ld(caller_saved_registers[i], SP, (len - i - 1) * wordSize);
#else
    lw(caller_saved_registers[i], SP, (len - i - 1) * wordSize);
#endif
  }
  daddi(SP, SP, len * wordSize);
};

void MacroAssembler::push2(Register reg1, Register reg2) {
#ifdef _LP64
  daddi(SP, SP, -16);
  sd(reg2, SP, 0);
  sd(reg1, SP, 8);
#else
  addi(SP, SP, -8);
  sw(reg2, SP, 0);
  sw(reg1, SP, 4);
#endif
}   

void MacroAssembler::pop2(Register reg1, Register reg2) {
#ifdef _LP64
  ld(reg1, SP, 0);
  ld(reg2, SP, 8);
  daddi(SP, SP, 16);
#else
  lw(reg1, SP, 0);
  lw(reg2, SP, 4);
  addi(SP, SP, 8);
#endif
}

//for UseCompressedOops Option
void MacroAssembler::load_klass(Register dst, Register src) {
#ifdef _LP64
    if(UseCompressedClassPointers){
        lwu(dst, Address(src, oopDesc::klass_offset_in_bytes()));
		decode_klass_not_null(dst);
    } else 
#endif
        ld(dst, src, oopDesc::klass_offset_in_bytes());
}

void MacroAssembler::store_klass(Register dst, Register src) {
#ifdef _LP64
    if(UseCompressedClassPointers){
		encode_klass_not_null(src);
		sw(src, dst, oopDesc::klass_offset_in_bytes());
    } else {
#endif 
		sd(src, dst, oopDesc::klass_offset_in_bytes());
    }
}

void MacroAssembler::load_prototype_header(Register dst, Register src) {
  load_klass(dst, src);
  ld(dst, Address(dst, Klass::prototype_header_offset()));
}

#ifdef _LP64
void MacroAssembler::store_klass_gap(Register dst, Register src) {
  if (UseCompressedClassPointers) {
    sw(src, dst, oopDesc::klass_gap_offset_in_bytes());
  } 
}

void MacroAssembler::load_heap_oop(Register dst, Address src) {
    if(UseCompressedOops){
	lwu(dst, src); 
	decode_heap_oop(dst);
    } else{
	ld(dst, src); 
    }
}

void MacroAssembler::store_heap_oop(Address dst, Register src){
    if(UseCompressedOops){
       assert(!dst.uses(src), "not enough registers");
       encode_heap_oop(src); 
       sw(src, dst);
    } else{
       sd(src, dst);
    }
}

#ifdef ASSERT
void MacroAssembler::verify_heapbase(const char* msg) {
  assert (UseCompressedOops || UseCompressedClassPointers, "should be compressed");
  assert (Universe::heap() != NULL, "java heap should be initialized");
/*  if (CheckCompressedOops) {
    Label ok;
    push(rscratch1); // cmpptr trashes rscratch1
    cmpptr(r12_heapbase, ExternalAddress((address)Universe::narrow_ptrs_base_addr()));
    jcc(Assembler::equal, ok);
    STOP(msg);
    bind(ok);
    pop(rscratch1);
  }*/
}
#endif


// Algorithm must match oop.inline.hpp encode_heap_oop.
void MacroAssembler::encode_heap_oop(Register r) {
#ifdef ASSERT
  verify_heapbase("MacroAssembler::encode_heap_oop:heap base corrupted?");
#endif
  verify_oop(r, "broken oop in encode_heap_oop");
  if (Universe::narrow_oop_base() == NULL) {
    if (Universe::narrow_oop_shift() != 0) { 
      assert (LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
      shr(r, LogMinObjAlignmentInBytes);
    }    
    return;
  }

    Label done;
    beq(r, R0, done);
    delayed()->nop();
    dsub(r, r, S5_heapbase);
    shr(r, LogMinObjAlignmentInBytes);
    bind(done);
}

void MacroAssembler::encode_heap_oop_not_null(Register r) {
    assert (UseCompressedOops, "should be compressed");
#ifdef ASSERT
    if (CheckCompressedOops) {
	Label ok;
	bne(r, R0, ok);
	delayed()->nop();
	stop("null oop passed to encode_heap_oop_not_null");
	bind(ok);
    }
#endif
	verify_oop(r, "broken oop in encode_heap_oop_not_null");
	if (Universe::narrow_oop_base() != NULL) {
		dsub(r, r, S5_heapbase);
	}
	if (Universe::narrow_oop_shift() != 0) {
		assert (LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
		shr(r, LogMinObjAlignmentInBytes);
	}

}

void MacroAssembler::encode_heap_oop_not_null(Register dst, Register src) {
    assert (UseCompressedOops, "should be compressed");
#ifdef ASSERT
    if (CheckCompressedOops) {
	Label ok;
	bne(src, R0, ok);
	delayed()->nop();
	stop("null oop passed to encode_heap_oop_not_null2");
	bind(ok);
    }
#endif
    verify_oop(src, "broken oop in encode_heap_oop_not_null2");
    if (dst != src) {
	move(dst, src);
    }

	if (Universe::narrow_oop_base() != NULL) {
		dsub(dst, dst, S5_heapbase);
	}
	if (Universe::narrow_oop_shift() != 0) {
		assert (LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
		shr(dst, LogMinObjAlignmentInBytes);
	}

}

void  MacroAssembler::decode_heap_oop(Register r) {
#ifdef ASSERT
  verify_heapbase("MacroAssembler::decode_heap_oop corrupted?");
#endif
  if (Universe::narrow_oop_base() == NULL) {
    if (Universe::narrow_oop_shift() != 0) {
      assert (LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
      shl(r, LogMinObjAlignmentInBytes);
    }
  } else {
    Label done;
    shl(r, LogMinObjAlignmentInBytes);
    beq(r, R0, done);
    delayed()->nop();
    dadd(r, r, S5_heapbase);
    bind(done);
  }
  verify_oop(r, "broken oop in decode_heap_oop");
}

void  MacroAssembler::decode_heap_oop_not_null(Register r) {
  // Note: it will change flags
  assert (UseCompressedOops, "should only be used for compressed headers");
  assert (Universe::heap() != NULL, "java heap should be initialized");
  // Cannot assert, unverified entry point counts instructions (see .ad file)
  // vtableStubs also counts instructions in pd_code_size_limit.
  // Also do not verify_oop as this is called by verify_oop.
  if (Universe::narrow_oop_shift() != 0) {
    assert(LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
    shl(r, LogMinObjAlignmentInBytes);
    if (Universe::narrow_oop_base() != NULL) {
      dadd(r, r, S5_heapbase);
    }
  } else {
    assert (Universe::narrow_oop_base() == NULL, "sanity");
  }
}

void  MacroAssembler::decode_heap_oop_not_null(Register dst, Register src) {
  assert (UseCompressedOops, "should only be used for compressed headers");
  assert (Universe::heap() != NULL, "java heap should be initialized");

  // Cannot assert, unverified entry point counts instructions (see .ad file)
  // vtableStubs also counts instructions in pd_code_size_limit.
  // Also do not verify_oop as this is called by verify_oop.
  //lea(dst, Address(S5_heapbase, src, Address::times_8, 0));
  if (Universe::narrow_oop_shift() != 0) {
    assert(LogMinObjAlignmentInBytes == Universe::narrow_oop_shift(), "decode alg wrong");
    if (LogMinObjAlignmentInBytes == Address::times_8) {
      dsll(dst, src, LogMinObjAlignmentInBytes);
      dadd(dst, dst, S5_heapbase);
    } else {
      if (dst != src) {
        move(dst, src);
      }
      shl(dst, LogMinObjAlignmentInBytes);
      if (Universe::narrow_oop_base() != NULL) {
        dadd(dst, dst, S5_heapbase);
      }
    }
  } else {
    assert (Universe::narrow_oop_base() == NULL, "sanity");
    if (dst != src) {
      move(dst, src);
    }
  }
}

void MacroAssembler::encode_klass_not_null(Register r) {
  if (Universe::narrow_klass_base() != NULL) {
    // Use r12 as a scratch register in which to temporarily load the narrow_klass_base.
    assert(r != S5_heapbase, "Encoding a klass in r12");
    li48(S5_heapbase, (int64_t)Universe::narrow_klass_base());
    dsub(r, r, S5_heapbase);
  }
  if (Universe::narrow_klass_shift() != 0) {
    assert (LogKlassAlignmentInBytes == Universe::narrow_klass_shift(), "decode alg wrong");
    shr(r, LogKlassAlignmentInBytes);
  }
  if (Universe::narrow_klass_base() != NULL) {
    reinit_heapbase();
  }
}

void MacroAssembler::encode_klass_not_null(Register dst, Register src) {
  if (dst == src) {
    encode_klass_not_null(src);
  } else {
    if (Universe::narrow_klass_base() != NULL) {
      li48(dst, (int64_t)Universe::narrow_klass_base());
      dsub(dst, src, dst);
    } else {
      move(dst, src);
    }
    if (Universe::narrow_klass_shift() != 0) {
      assert (LogKlassAlignmentInBytes == Universe::narrow_klass_shift(), "decode alg wrong");
      shr(dst, LogKlassAlignmentInBytes);
    }
  }
}

// Function instr_size_for_decode_klass_not_null() counts the instructions
// generated by decode_klass_not_null(register r) and reinit_heapbase(),
// when (Universe::heap() != NULL).  Hence, if the instructions they
// generate change, then this method needs to be updated.
int MacroAssembler::instr_size_for_decode_klass_not_null() {
  assert (UseCompressedClassPointers, "only for compressed klass ptrs");
  if (Universe::narrow_klass_base() != NULL) {
    // mov64 + addq + shlq? + mov64  (for reinit_heapbase()).
    return (Universe::narrow_klass_shift() == 0 ? 4 * 9 : 4 * 10);
  } else {
    // longest load decode klass function, mov64, leaq
    return (Universe::narrow_klass_shift() == 0 ? 4 * 0 : 4 * 1);
  }
}

void  MacroAssembler::decode_klass_not_null(Register r) { 
  // Note: it will change flags
  assert (UseCompressedClassPointers, "should only be used for compressed headers");
  assert(r != S5_heapbase, "Decoding a klass in r12");
  // Cannot assert, unverified entry point counts instructions (see .ad file)
  // vtableStubs also counts instructions in pd_code_size_limit.
  // Also do not verify_oop as this is called by verify_oop.
  if (Universe::narrow_klass_shift() != 0) { 
    assert(LogKlassAlignmentInBytes == Universe::narrow_klass_shift(), "decode alg wrong");
    shl(r, LogKlassAlignmentInBytes);
  }
  if (Universe::narrow_klass_base() != NULL) {
    li48(S5_heapbase, (int64_t)Universe::narrow_klass_base());
    dadd(r, r, S5_heapbase);
    reinit_heapbase();
  }
}

void  MacroAssembler::decode_klass_not_null(Register dst, Register src) {
  assert (UseCompressedClassPointers, "should only be used for compressed headers");

  if (dst == src) {
    decode_klass_not_null(dst);
  } else {
    // Cannot assert, unverified entry point counts instructions (see .ad file)
    // vtableStubs also counts instructions in pd_code_size_limit.
    // Also do not verify_oop as this is called by verify_oop.
    li48(S5_heapbase, (int64_t)Universe::narrow_klass_base());
    if (Universe::narrow_klass_shift() != 0) {
      assert(LogKlassAlignmentInBytes == Universe::narrow_klass_shift(), "decode alg wrong");
      assert(LogKlassAlignmentInBytes == Address::times_8, "klass not aligned on 64bits?");
      dsll(dst, src, Address::times_8);
      dadd(dst, dst, S5_heapbase);
    } else {
      dadd(dst, src, S5_heapbase);
    }
    reinit_heapbase();
  }
}

/*
void  MacroAssembler::set_narrow_oop(Register dst, jobject obj) {
  assert(oop_recorder() != NULL, "this assembler needs an OopRecorder");
  int oop_index = oop_recorder()->find_index(obj);
  RelocationHolder rspec = oop_Relocation::spec(oop_index);
  mov_literal32(dst, oop_index, rspec, narrow_oop_operand);
}
*/

void MacroAssembler::incrementl(Register reg, int value) {
  if (value == min_jint) {
     move(AT, value);
     LP64_ONLY(addu32(reg, reg, AT)) NOT_LP64(addu(reg, reg, AT));
     return; 
  }
  if (value <  0) { decrementl(reg, -value); return; }
  if (value == 0) {                        ; return; }

  if(Assembler::is_simm16(value)) {
     NOT_LP64(addiu(reg, reg, value));
     LP64_ONLY(move(AT, value); addu32(reg, reg, AT));
  } else {
     move(AT, value);
     LP64_ONLY(addu32(reg, reg, AT)) NOT_LP64(addu(reg, reg, AT));
  }
}

void MacroAssembler::decrementl(Register reg, int value) {
  if (value == min_jint) {
     move(AT, value);
     LP64_ONLY(subu32(reg, reg, AT)) NOT_LP64(subu(reg, reg, AT));
     return;
  }
  if (value <  0) { incrementl(reg, -value); return; }
  if (value == 0) {                        ; return; }

  if(Assembler::is_simm16(value)) {
     NOT_LP64(addiu(reg, reg, -value));
     LP64_ONLY(move(AT, value); subu32(reg, reg, AT));
  } else {
     move(AT, value);
     LP64_ONLY(subu32(reg, reg, AT)) NOT_LP64(subu(reg, reg, AT));
  }
}

void MacroAssembler::reinit_heapbase() {
  if (UseCompressedOops || UseCompressedClassPointers) {
    if (Universe::heap() != NULL) {
      if (Universe::narrow_oop_base() == NULL) {
        move(S5_heapbase, R0);
      } else {
        li48(S5_heapbase, (int64_t)Universe::narrow_ptrs_base());
      }
    } else {
      li48(S5_heapbase, (intptr_t)Universe::narrow_ptrs_base_addr());
      ld(S5_heapbase, S5_heapbase, 0);
    }
  }
}
#endif // _LP64

void MacroAssembler::check_klass_subtype(Register sub_klass,
                           Register super_klass,
                           Register temp_reg,
                           Label& L_success) {
//implement ind   gen_subtype_check
  Label L_failure;
  check_klass_subtype_fast_path(sub_klass, super_klass, temp_reg,        &L_success, &L_failure, NULL);
  check_klass_subtype_slow_path(sub_klass, super_klass, temp_reg, noreg, &L_success, NULL);
  bind(L_failure);
}

SkipIfEqual::SkipIfEqual(
    MacroAssembler* masm, const bool* flag_addr, bool value) {
  _masm = masm;
  _masm->li(AT, (address)flag_addr);
  _masm->lb(AT,AT,0);
  _masm->addi(AT,AT,-value);
  _masm->beq(AT,R0,_label);
  _masm->delayed()->nop();
}
void MacroAssembler::check_klass_subtype_fast_path(Register sub_klass,
                                                   Register super_klass,
                                                   Register temp_reg,
                                                   Label* L_success,
                                                   Label* L_failure,
                                                   Label* L_slow_path,
                                        RegisterOrConstant super_check_offset) {
  assert_different_registers(sub_klass, super_klass, temp_reg);
  bool must_load_sco = (super_check_offset.constant_or_zero() == -1);
  if (super_check_offset.is_register()) {
    assert_different_registers(sub_klass, super_klass,
                               super_check_offset.as_register());
  } else if (must_load_sco) {
    assert(temp_reg != noreg, "supply either a temp or a register offset");
  }

  Label L_fallthrough;
  int label_nulls = 0;
  if (L_success == NULL)   { L_success   = &L_fallthrough; label_nulls++; }
  if (L_failure == NULL)   { L_failure   = &L_fallthrough; label_nulls++; }
  if (L_slow_path == NULL) { L_slow_path = &L_fallthrough; label_nulls++; }
  assert(label_nulls <= 1, "at most one NULL in the batch");

  int sc_offset = in_bytes(Klass::secondary_super_cache_offset());
  int sco_offset = in_bytes(Klass::super_check_offset_offset());
  // If the pointers are equal, we are done (e.g., String[] elements).
  // This self-check enables sharing of secondary supertype arrays among
  // non-primary types such as array-of-interface.  Otherwise, each such
  // type would need its own customized SSA.
  // We move this check to the front of the fast path because many
  // type checks are in fact trivially successful in this manner,
  // so we get a nicely predicted branch right at the start of the check.
  //cmpptr(sub_klass, super_klass);
  //local_jcc(Assembler::equal, *L_success);
  beq(sub_klass, super_klass, *L_success);
  delayed()->nop();
  // Check the supertype display:
  if (must_load_sco) {
    // Positive movl does right thing on LP64.
	lwu(temp_reg, super_klass, sco_offset);
    super_check_offset = RegisterOrConstant(temp_reg);
  }
  dsll(AT, super_check_offset.register_or_noreg(), Address::times_1);
  daddu(AT, sub_klass, AT);
  ld(AT, AT, super_check_offset.constant_or_zero()*Address::times_1);

  // This check has worked decisively for primary supers.
  // Secondary supers are sought in the super_cache ('super_cache_addr').
  // (Secondary supers are interfaces and very deeply nested subtypes.)
  // This works in the same check above because of a tricky aliasing
  // between the super_cache and the primary super display elements.
  // (The 'super_check_addr' can address either, as the case requires.)
  // Note that the cache is updated below if it does not help us find
  // what we need immediately.
  // So if it was a primary super, we can just fail immediately.
  // Otherwise, it's the slow path for us (no success at this point).

  if (super_check_offset.is_register()) {
	beq(super_klass, AT, *L_success);
	delayed()->nop();
	addi(AT, super_check_offset.as_register(), -sc_offset);
    if (L_failure == &L_fallthrough) {
	  beq(AT, R0, *L_slow_path);
	  delayed()->nop();
    } else {
	  bne(AT, R0, *L_failure);
	  delayed()->nop();
	  b(*L_slow_path);
	  delayed()->nop();
    }
  } else if (super_check_offset.as_constant() == sc_offset) {
    // Need a slow path; fast failure is impossible.
    if (L_slow_path == &L_fallthrough) {
		beq(super_klass, AT, *L_success);
		delayed()->nop();
    } else {
		bne(super_klass, AT, *L_slow_path);
		delayed()->nop();
		b(*L_success);
		delayed()->nop();
    }
  } else {
    // No slow path; it's a fast decision.
    if (L_failure == &L_fallthrough) {
		beq(super_klass, AT, *L_success);
		delayed()->nop();
    } else {
		bne(super_klass, AT, *L_failure);
		delayed()->nop();
		b(*L_success);
		delayed()->nop();
    }
  }

  bind(L_fallthrough);

}


void MacroAssembler::check_klass_subtype_slow_path(Register sub_klass,
                                                   Register super_klass,
                                                   Register temp_reg,
                                                   Register temp2_reg,
                                                   Label* L_success,
                                                   Label* L_failure,
                                                   bool set_cond_codes) {
  assert_different_registers(sub_klass, super_klass, temp_reg);
  if (temp2_reg != noreg)
    assert_different_registers(sub_klass, super_klass, temp_reg, temp2_reg);
  else
    temp2_reg = T9;
#define IS_A_TEMP(reg) ((reg) == temp_reg || (reg) == temp2_reg)

  Label L_fallthrough;
  int label_nulls = 0;
  if (L_success == NULL)   { L_success   = &L_fallthrough; label_nulls++; }
  if (L_failure == NULL)   { L_failure   = &L_fallthrough; label_nulls++; }
  assert(label_nulls <= 1, "at most one NULL in the batch");

  // a couple of useful fields in sub_klass:
  int ss_offset = in_bytes(Klass::secondary_supers_offset());
  int sc_offset = in_bytes(Klass::secondary_super_cache_offset());
  Address secondary_supers_addr(sub_klass, ss_offset);
  Address super_cache_addr(     sub_klass, sc_offset);

  // Do a linear scan of the secondary super-klass chain.
  // This code is rarely used, so simplicity is a virtue here.
  // The repne_scan instruction uses fixed registers, which we must spill.
  // Don't worry too much about pre-existing connections with the input regs.

#if 0
  assert(sub_klass != T9, "killed reg"); // killed by mov(rax, super)
  assert(sub_klass != T1, "killed reg"); // killed by lea(rcx, &pst_counter)
#endif

  // Get super_klass value into rax (even if it was in rdi or rcx).
/*
  bool pushed_rax = false, pushed_rcx = false, pushed_rdi = false;
  if (super_klass != rax || UseCompressedOops) {
    if (!IS_A_TEMP(rax)) { push(rax); pushed_rax = true; }
    mov(rax, super_klass);
  }
  if (!IS_A_TEMP(rcx)) { push(rcx); pushed_rcx = true; }
  if (!IS_A_TEMP(rdi)) { push(rdi); pushed_rdi = true; }
*/
#ifndef PRODUCT
  int* pst_counter = &SharedRuntime::_partial_subtype_ctr;
  ExternalAddress pst_counter_addr((address) pst_counter);
  NOT_LP64(  incrementl(pst_counter_addr) );
  //LP64_ONLY( lea(rcx, pst_counter_addr) );
  //LP64_ONLY( incrementl(Address(rcx, 0)) );
#endif //PRODUCT

  // We will consult the secondary-super array.
  ld(temp_reg, secondary_supers_addr);
  // Load the array length.  (Positive movl does right thing on LP64.)
  lw(temp2_reg, Address(temp_reg, Array<Klass*>::length_offset_in_bytes()));
  // Skip to start of data.
  daddiu(temp_reg, temp_reg, Array<Klass*>::base_offset_in_bytes());

  // Scan RCX words at [RDI] for an occurrence of RAX.
  // Set NZ/Z based on last compare.
  // Z flag value will not be set by 'repne' if RCX == 0 since 'repne' does
  // not change flags (only scas instruction which is repeated sets flags).
  // Set Z = 0 (not equal) before 'repne' to indicate that class was not found.

  /* 2013/4/3 Jin: OpenJDK8 never compresses klass pointers in secondary-super array. */
  Label Loop, subtype;
  bind(Loop);
  beq(temp2_reg, R0, *L_failure);
  delayed()->nop();
  ld(AT, temp_reg, 0);
  beq(AT, super_klass, subtype);
  delayed()->daddi(temp_reg, temp_reg, 1 * wordSize);
  b(Loop);
  delayed()->daddi(temp2_reg, temp2_reg, -1); 

  bind(subtype);
  sd(super_klass, super_cache_addr);
  if (L_success != &L_fallthrough) {
	  b(*L_success);
	  delayed()->nop();
  }

/*
  if (set_cond_codes) {
    // Special hack for the AD files:  rdi is guaranteed non-zero.
    assert(!pushed_rdi, "rdi must be left non-NULL");
    // Also, the condition codes are properly set Z/NZ on succeed/failure.
  }
*/
  // Success.  Cache the super we found and proceed in triumph.
#undef IS_A_TEMP

  bind(L_fallthrough);
}
void MacroAssembler::get_vm_result(Register oop_result, Register java_thread) {
  ld(oop_result, Address(java_thread, JavaThread::vm_result_offset()));
  sd(R0, Address(java_thread, JavaThread::vm_result_offset()));
  verify_oop(oop_result, "broken oop in call_VM_base");
}

void MacroAssembler::get_vm_result_2(Register metadata_result, Register java_thread) {
  ld(metadata_result, Address(java_thread, JavaThread::vm_result_2_offset()));
  sd(R0, Address(java_thread, JavaThread::vm_result_2_offset()));
}

Address MacroAssembler::argument_address(RegisterOrConstant arg_slot,
                                         int extra_slot_offset) {
  // cf. TemplateTable::prepare_invoke(), if (load_receiver).
  int stackElementSize = Interpreter::stackElementSize;
  int offset = Interpreter::expr_offset_in_bytes(extra_slot_offset+0);
#ifdef ASSERT
  int offset1 = Interpreter::expr_offset_in_bytes(extra_slot_offset+1);
  assert(offset1 - offset == stackElementSize, "correct arithmetic");
#endif
  Register             scale_reg    = NOREG;
  Address::ScaleFactor scale_factor = Address::no_scale;
  if (arg_slot.is_constant()) {
    offset += arg_slot.as_constant() * stackElementSize;
  } else {
    scale_reg    = arg_slot.as_register();
    scale_factor = Address::times_8;
  }
  // 2014/07/31 Fu: We don't push RA on stack in prepare_invoke.
  //  offset += wordSize;           // return PC is on stack
  if(scale_reg==NOREG) return Address(SP, offset);
  else {
	dsll(scale_reg, scale_reg, scale_factor);
	daddu(scale_reg, SP, scale_reg);
	return Address(scale_reg, offset);
  }
}

SkipIfEqual::~SkipIfEqual() {
  _masm->bind(_label);
}

void MacroAssembler::load_sized_value(Register dst, Address src, size_t size_in_bytes, bool is_signed, Register dst2) {
  switch (size_in_bytes) {
#ifndef _LP64
  case  8:
    assert(dst2 != noreg, "second dest register required");
    lw(dst,  src);
    lw(dst2, src.plus_disp(BytesPerInt));
    break;
#else
  case  8:  ld(dst, src); break;
#endif
  case  4:  lw(dst, src); break;
  case  2:  is_signed ? lh(dst, src) : lhu(dst, src); break;
  case  1:  is_signed ? lb( dst, src) : lbu( dst, src); break;
  default:  ShouldNotReachHere();
  }
}

void MacroAssembler::store_sized_value(Address dst, Register src, size_t size_in_bytes, Register src2) {
  switch (size_in_bytes) {
#ifndef _LP64
  case  8:
    assert(src2 != noreg, "second source register required");
    sw(src, dst);
    sw(src2, dst.plus_disp(BytesPerInt));
    break;
#else
  case  8:  sd(src, dst); break;
#endif
  case  4:  sw(src, dst); break;
  case  2:  sh(src, dst); break;
  case  1:  sb(src, dst); break;
  default:  ShouldNotReachHere();
  }
}

// Look up the method for a megamorphic invokeinterface call.
// The target method is determined by <intf_klass, itable_index>.
// The receiver klass is in recv_klass.
// On success, the result will be in method_result, and execution falls through.
// On failure, execution transfers to the given label.
void MacroAssembler::lookup_interface_method(Register recv_klass,
                                             Register intf_klass,
                                             RegisterOrConstant itable_index,
                                             Register method_result,
                                             Register scan_temp,
                                             Label& L_no_such_interface) {
  assert_different_registers(recv_klass, intf_klass, method_result, scan_temp);
  assert(itable_index.is_constant() || itable_index.as_register() == method_result,
         "caller must use same register for non-constant itable index as for method");

  // Compute start of first itableOffsetEntry (which is at the end of the vtable)
  int vtable_base = InstanceKlass::vtable_start_offset() * wordSize;
  int itentry_off = itableMethodEntry::method_offset_in_bytes();
  int scan_step   = itableOffsetEntry::size() * wordSize;
  int vte_size    = vtableEntry::size() * wordSize;
  Address::ScaleFactor times_vte_scale = Address::times_ptr;
  assert(vte_size == wordSize, "else adjust times_vte_scale");

  lw(scan_temp, Address(recv_klass, InstanceKlass::vtable_length_offset() * wordSize));

  // %%% Could store the aligned, prescaled offset in the klassoop.
//  lea(scan_temp, Address(recv_klass, scan_temp, times_vte_scale, vtable_base));
  dsll(scan_temp, scan_temp, times_vte_scale);
  daddu(scan_temp, recv_klass, scan_temp);
  daddiu(scan_temp, scan_temp, vtable_base);
  if (HeapWordsPerLong > 1) {
    // Round up to align_object_offset boundary
    // see code for InstanceKlass::start_of_itable!
    round_to(scan_temp, BytesPerLong);
  }

  // Adjust recv_klass by scaled itable_index, so we can free itable_index.
  assert(itableMethodEntry::size() * wordSize == wordSize, "adjust the scaling in the code below");
//  lea(recv_klass, Address(recv_klass, itable_index, Address::times_ptr, itentry_off));
  if (itable_index.is_constant()) {
    li48(AT, (int)itable_index.is_constant());
    dsll(AT, AT, (int)Address::times_ptr);
  } else {
    dsll(AT, itable_index.as_register(), (int)Address::times_ptr);
  }
  daddu(AT, AT, recv_klass);
  daddiu(recv_klass, AT, itentry_off);

  // for (scan = klass->itable(); scan->interface() != NULL; scan += scan_step) {
  //   if (scan->interface() == intf) {
  //     result = (klass + scan->offset() + itable_index);
  //   }
  // }
  Label search, found_method;

  for (int peel = 1; peel >= 0; peel--) {
    ld(method_result, Address(scan_temp, itableOffsetEntry::interface_offset_in_bytes()));

    if (peel) {
      beq(intf_klass, method_result, found_method);
      nop();
    } else {
      bne(intf_klass, method_result, search);
      nop();
      // (invert the test to fall through to found_method...)
    }

    if (!peel)  break;

    bind(search);

    // Check that the previous entry is non-null.  A null entry means that
    // the receiver class doesn't implement the interface, and wasn't the
    // same as when the caller was compiled.
    beq(method_result, R0, L_no_such_interface);
    nop();
    daddiu(scan_temp, scan_temp, scan_step);
  }

  bind(found_method);

  // Got a hit.
  lw(scan_temp, Address(scan_temp, itableOffsetEntry::offset_offset_in_bytes()));
  ld(method_result, Address(recv_klass, scan_temp, Address::times_1));
}


// virtual method calling
void MacroAssembler::lookup_virtual_method(Register recv_klass,
                                           RegisterOrConstant vtable_index,
                                           Register method_result) {
  Register tmp = GP;
  push(tmp);

  if (vtable_index.is_constant()) {
    assert_different_registers(recv_klass, method_result, tmp);
  } else {
    assert_different_registers(recv_klass, method_result, vtable_index.as_register(), tmp);
  }
  const int base = InstanceKlass::vtable_start_offset() * wordSize;
  assert(vtableEntry::size() * wordSize == wordSize, "else adjust the scaling in the code below");
/*
  Address vtable_entry_addr(recv_klass,
                            vtable_index, Address::times_ptr,
                            base + vtableEntry::method_offset_in_bytes());
*/
  if (vtable_index.is_constant()) {
    li48(AT, vtable_index.as_constant());
    dsll(AT, AT, (int)Address::times_ptr);
  } else {
    dsll(AT, vtable_index.as_register(), (int)Address::times_ptr);
  }
  li48(tmp, base + vtableEntry::method_offset_in_bytes());
  daddu(tmp, tmp, AT);
  daddu(tmp, tmp, recv_klass);
  ld(method_result, tmp, 0);

  pop(tmp);
}

