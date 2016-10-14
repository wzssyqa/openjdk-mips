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
#include "asm/macroAssembler.hpp"
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


// Implementation of AddressLiteral

AddressLiteral::AddressLiteral(address target, relocInfo::relocType rtype) {
  _is_lval = false;
  _target = target;
  _rspec = rspec_from_rtype(rtype, target);
}

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


