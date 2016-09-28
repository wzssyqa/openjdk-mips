/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_MIPS_VM_ASSEMBLER_MIPS_HPP
#define CPU_MIPS_VM_ASSEMBLER_MIPS_HPP

#include "asm/register.hpp"

class BiasedLockingCounters;


// Note: A register location is represented via a Register, not
//       via an address for efficiency & simplicity reasons.

class ArrayAddress;

class Address VALUE_OBJ_CLASS_SPEC {
  
public:
  enum ScaleFactor {
    no_scale = -1,
    times_1  =  0,
    times_2  =  1,
    times_4  =  2,
    times_8  =  3,
    times_ptr = LP64_ONLY(times_8) NOT_LP64(times_4)
  };

  static ScaleFactor times(int size) {
    assert(size >= 1 && size <= 8 && is_power_of_2(size), "bad scale size");
    if (size == 8)  return times_8;
    if (size == 4)  return times_4;
    if (size == 2)  return times_2;
    return times_1;
  }

 private:
  Register         _base;
  Register         _index;
  ScaleFactor      _scale;
  int              _disp;
  RelocationHolder _rspec;

  // Easily misused constructors make them private
  // %%% can we make these go away?
  //FIXME aoqi
  //NOT_LP64(Address(address loc, RelocationHolder spec);)
  Address(address loc, RelocationHolder spec);
  Address(int disp, address loc, relocInfo::relocType rtype);
  Address(int disp, address loc, RelocationHolder spec);

 public:

 int disp() { return _disp; }
  // creation
  Address()
    : _base(noreg),
      _index(noreg),
      _scale(no_scale),
      _disp(0) {
  }

  // No default displacement otherwise Register can be implicitly
  // converted to 0(Register) which is quite a different animal.

  Address(Register base, int disp)
    : _base(base),
      _index(noreg),
      _scale(no_scale),
      _disp(disp) {
  }

  Address(Register base) 
   : _base(base),
     _index(noreg),
     _scale(no_scale),
     _disp(0) {
  }

  Address(Register base, Register index, ScaleFactor scale, int disp = 0)
    : _base (base),
      _index(index),
      _scale(scale),
      _disp (disp) {
    assert(!index->is_valid() == (scale == Address::no_scale),
           "inconsistent address");
  }

  // The following two overloads are used in connection with the
  // ByteSize type (see sizes.hpp).  They simplify the use of
  // ByteSize'd arguments in assembly code. Note that their equivalent
  // for the optimized build are the member functions with int disp
  // argument since ByteSize is mapped to an int type in that case.
  //
  // Note: DO NOT introduce similar overloaded functions for WordSize
  // arguments as in the optimized mode, both ByteSize and WordSize
  // are mapped to the same type and thus the compiler cannot make a
  // distinction anymore (=> compiler errors).

#ifdef ASSERT
  Address(Register base, ByteSize disp)
    : _base(base),
      _index(noreg),
      _scale(no_scale),
      _disp(in_bytes(disp)) {
  }

  Address(Register base, Register index, ScaleFactor scale, ByteSize disp)
    : _base(base),
      _index(index),
      _scale(scale),
      _disp(in_bytes(disp)) {
    assert(!index->is_valid() == (scale == Address::no_scale),
           "inconsistent address");
  }
#endif // ASSERT

  // accessors
  bool        uses(Register reg) const { return _base == reg || _index == reg; }
  Register    base()             const { return _base;  }
  Register    index()            const { return _index; }
  ScaleFactor scale()            const { return _scale; }
  int         disp()             const { return _disp;  }

  // Convert the raw encoding form into the form expected by the constructor for
  // Address.  An index of 4 (rsp) corresponds to having no index, so convert
  // that to noreg for the Address constructor.
  //static Address make_raw(int base, int index, int scale, int disp);

  static Address make_array(ArrayAddress);

/*
 private:
  bool base_needs_rex() const {
    return _base != noreg && _base->encoding() >= 8;
  }

  bool index_needs_rex() const {
    return _index != noreg &&_index->encoding() >= 8;
  }

  relocInfo::relocType reloc() const { return _rspec.type(); }
*/
  friend class Assembler;
  friend class MacroAssembler;
  friend class LIR_Assembler; // base/index/scale/disp
};


// Calling convention
class Argument VALUE_OBJ_CLASS_SPEC {
 private:
	int _number;
 public:
	enum {
#ifdef _LP64
	  n_register_parameters = 8,   // 8 integer registers used to pass parameters
	  n_float_register_parameters = 8   // 4 float registers used to pass parameters
#else
	    n_register_parameters = 4,   // 4 integer registers used to pass parameters
	  n_float_register_parameters = 4   // 4 float registers used to pass parameters
#endif
	};
	
	Argument(int number):_number(number){ }
	Argument successor() {return Argument(number() + 1);}

	int number()const {return _number;}
	bool is_Register()const {return _number < n_register_parameters;}
	bool is_FloatRegister()const {return _number < n_float_register_parameters;}

	Register as_Register()const {
		assert(is_Register(), "must be a register argument");
		return ::as_Register(A0->encoding() + _number);
	}
	FloatRegister  as_FloatRegister()const {
		assert(is_FloatRegister(), "must be a float register argument");
		return ::as_FloatRegister(F12->encoding() + _number);
	}
	
	Address as_caller_address()const {return Address(SP, (number() LP64_ONLY( -n_register_parameters)) * wordSize);}
};



//
// AddressLiteral has been split out from Address because operands of this type
// need to be treated specially on 32bit vs. 64bit platforms. By splitting it out
// the few instructions that need to deal with address literals are unique and the
// MacroAssembler does not have to implement every instruction in the Assembler
// in order to search for address literals that may need special handling depending
// on the instruction and the platform. As small step on the way to merging i486/amd64
// directories.
//
class AddressLiteral VALUE_OBJ_CLASS_SPEC {
  friend class ArrayAddress;
  RelocationHolder _rspec;
  // Typically we use AddressLiterals we want to use their rval
  // However in some situations we want the lval (effect address) of the item.
  // We provide a special factory for making those lvals.
  bool _is_lval;

  // If the target is far we'll need to load the ea of this to
  // a register to reach it. Otherwise if near we can do rip
  // relative addressing.

  address          _target;

 protected:
  // creation
  AddressLiteral()
    : _is_lval(false),
      _target(NULL)
  {}

  public:

  AddressLiteral(address target, relocInfo::relocType rtype);

  AddressLiteral(address target, RelocationHolder const& rspec)
    : _rspec(rspec),
      _is_lval(false),
      _target(target)
  {}
#ifdef _LP64
   // 32-bit complains about a multiple declaration for int*.
   AddressLiteral(intptr_t* addr, relocInfo::relocType rtype = relocInfo::none)
     : _target((address) addr),
       _rspec(rspec_from_rtype(rtype, (address) addr)) {}
#endif


  AddressLiteral addr() {
    AddressLiteral ret = *this;
    ret._is_lval = true;
    return ret;
  }


 private:

  address target() { return _target; }
  bool is_lval() { return _is_lval; }

  relocInfo::relocType reloc() const { return _rspec.type(); }
  const RelocationHolder& rspec() const { return _rspec; }

  friend class Assembler;
  friend class MacroAssembler;
  friend class Address;
  friend class LIR_Assembler;
 RelocationHolder rspec_from_rtype(relocInfo::relocType rtype, address addr) {
   switch (rtype) {
   case relocInfo::external_word_type:
     return external_word_Relocation::spec(addr);
   case relocInfo::internal_word_type:
    return internal_word_Relocation::spec(addr);
   case relocInfo::opt_virtual_call_type:
    return opt_virtual_call_Relocation::spec();
   case relocInfo::static_call_type:
     return static_call_Relocation::spec();
   case relocInfo::runtime_call_type:
     return runtime_call_Relocation::spec();
   case relocInfo::poll_type:
   case relocInfo::poll_return_type:
     return Relocation::spec_simple(rtype);
   case relocInfo::none:
   case relocInfo::oop_type:
     // Oops are a special case. Normally they would be their own section
     // but in cases like icBuffer they are literals in the code stream that
     // we don't have a section for. We use none so that we get a literal address
     // which is always patchable.
     return RelocationHolder();
   default: 
     ShouldNotReachHere();
     return RelocationHolder();
 }
}

};

// Convience classes
class RuntimeAddress: public AddressLiteral {

  public:

  RuntimeAddress(address target) : AddressLiteral(target, relocInfo::runtime_call_type) {}

};

class OopAddress: public AddressLiteral {

  public:

  OopAddress(address target) : AddressLiteral(target, relocInfo::oop_type){}

};

class ExternalAddress: public AddressLiteral {

  public:

  ExternalAddress(address target) : AddressLiteral(target, relocInfo::external_word_type){}

};

class InternalAddress: public AddressLiteral {

  public:

  InternalAddress(address target) : AddressLiteral(target, relocInfo::internal_word_type) {}

};

// x86 can do array addressing as a single operation since disp can be an absolute
// address amd64 can't. We create a class that expresses the concept but does extra
// magic on amd64 to get the final result

class ArrayAddress VALUE_OBJ_CLASS_SPEC {
  private:

  AddressLiteral _base;
  Address        _index;

  public:

  ArrayAddress() {};
  ArrayAddress(AddressLiteral base, Address index): _base(base), _index(index) {};
  AddressLiteral base() { return _base; }
  Address index() { return _index; }

};

const int FPUStateSizeInWords = NOT_LP64(27) LP64_ONLY( 512 / wordSize);

// The MIPS LOONGSON Assembler: Pure assembler doing NO optimizations on the instruction
// level ; i.e., what you write is what you get. The Assembler is generating code into 
// a CodeBuffer.

class Assembler : public AbstractAssembler  {
  friend class AbstractAssembler; // for the non-virtual hack
  friend class LIR_Assembler; // as_Address()
  friend class StubGenerator;

  public:
  enum ops {
		special_op  = 0x00,
		regimm_op   = 0x01,
  	j_op        = 0x02,
  	jal_op      = 0x03,
	  beq_op      = 0x04,
	  bne_op      = 0x05,
	  blez_op     = 0x06,
	  bgtz_op     = 0x07,
	  addi_op     = 0x08,
	  addiu_op    = 0x09,
	  slti_op     = 0x0a,
	  sltiu_op    = 0x0b,
	  andi_op     = 0x0c,
	  ori_op      = 0x0d,
	  xori_op     = 0x0e,
	  lui_op      = 0x0f,
	  cop0_op     = 0x10,
	  cop1_op     = 0x11,
	  cop2_op     = 0x12,
	  cop3_op     = 0x13,
	  beql_op     = 0x14,
	  bnel_op     = 0x15,
	  blezl_op    = 0x16,
	  bgtzl_op    = 0x17,
	  daddi_op    = 0x18,
	  daddiu_op   = 0x19,
	  ldl_op      = 0x1a,
	  ldr_op      = 0x1b,
	  special2_op = 0x1c,
	  lb_op       = 0x20,
	  lh_op       = 0x21,
	  lwl_op      = 0x22,
	  lw_op       = 0x23,
	  lbu_op      = 0x24,
	  lhu_op      = 0x25,
    lwr_op      = 0x26,
    lwu_op      = 0x27,
    sb_op       = 0x28,
    sh_op       = 0x29,
    swl_op      = 0x2a,
	  sw_op       = 0x2b,
	  sdl_op      = 0x2c,
	  sdr_op      = 0x2d,
	  swr_op      = 0x2e,
	  cache_op    = 0x2f,
	  ll_op       = 0x30,
	  lwc1_op     = 0x31,
	  lld_op      = 0x34,
	  ldc1_op     = 0x35,
	  ld_op       = 0x37,
	  sc_op       = 0x38,
	  swc1_op     = 0x39,
	  scd_op      = 0x3c,
	  sdc1_op     = 0x3d,
	  sd_op       = 0x3f
  };
	
	static	const char *ops_name[];

	//special family, the opcode is in low 6 bits. 
	enum special_ops {
		sll_op			= 0x00,
		movci_op		= 0x01,
		srl_op			= 0x02,
		sra_op			= 0x03,
		sllv_op			= 0x04,
		srlv_op			= 0x06,
		srav_op 		= 0x07,
		jr_op				= 0x08,
		jalr_op			= 0x09,
		movz_op			= 0x0a,
		movn_op			= 0x0b,
		syscall_op	= 0x0c,
		break_op		= 0x0d,
		sync_op			= 0x0f,
		mfhi_op			= 0x10,
		mthi_op			= 0x11,
		mflo_op			= 0x12,
		mtlo_op			= 0x13,
		dsllv_op		= 0x14,
		dsrlv_op		= 0x16,
		dsrav_op		= 0x17,
		mult_op			= 0x18,
		multu_op 		= 0x19,
		div_op			= 0x1a,
		divu_op			= 0x1b,
		dmult_op		= 0x1c,
		dmultu_op		= 0x1d,
		ddiv_op			= 0x1e,
		ddivu_op		= 0x1f,
		add_op			= 0x20,
		addu_op			= 0x21,
		sub_op			= 0x22,
		subu_op			= 0x23,
		and_op			= 0x24,
		or_op				= 0x25,
		xor_op			= 0x26,
		nor_op			= 0x27,
		slt_op			= 0x2a,
		sltu_op			= 0x2b,
		dadd_op			= 0x2c,
		daddu_op		= 0x2d,
		dsub_op			= 0x2e,
		dsubu_op		= 0x2f,
		tge_op			= 0x30,
		tgeu_op			= 0x31,
		tlt_op			= 0x32,
		tltu_op			= 0x33,
		teq_op			= 0x34,
		tne_op			= 0x36,
		dsll_op			= 0x38,
		dsrl_op			= 0x3a,
		dsra_op			= 0x3b,
		dsll32_op		= 0x3c,
		dsrl32_op		= 0x3e,
		dsra32_op		= 0x3f
	};
	
	static	const char* special_name[]; 
	
	//special family, the opcode is in low 6 bits. 
	enum special2_ops {
		madd_op			= 0x00,
		mul_op			= 0x02,
		msub_op			= 0x04,
		gsdmult_op		= 0x11,
		gsdiv_op		= 0x14,
		gsddiv_op		= 0x15,
		gsmod_op		= 0x1c,
		gsdmod_op		= 0x1d,
        };

	static	const char* special2_name[]; 

	//regimm family, the opcode is in rt[16...20], 5 bits
	enum regimm_ops {
		bltz_op			= 0x00,
		bgez_op			= 0x01,
		bltzl_op		= 0x02,
		bgezl_op		= 0x03,
		tgei_op			= 0x08,
		tgeiu_op		= 0x09,
		tlti_op			= 0x0a,
		tltiu_op		= 0x0b,
		teqi_op			= 0x0c,
		tnei_op			= 0x0e,
		bltzal_op		= 0x10,
		bgezal_op		= 0x11,
		bltzall_op	= 0x12,
		bgezall_op	= 0x13,
	};

	static	const char* regimm_name[]; 

	//copx family,the op in rs, 5 bits
	enum cop_ops {
		mf_op				= 0x00,
		dmf_op			= 0x01,
		cf_op				= 0x02,
		mt_op				= 0x04,
		dmt_op			= 0x05,
		ct_op				= 0x06,
		bc_op				= 0x08,
		single_fmt	= 0x10,
		double_fmt	= 0x11,
		word_fmt		= 0x14,
		long_fmt		= 0x15
	};

	enum bc_ops {
		bcf_op			= 0x00,
		bct_op			= 0x01,
		bcfl_op			= 0x02,
		bctl_op			= 0x03,
	};

	enum c_conds {
		f_cond			= 0x30,
		un_cond			= 0x31,
		eq_cond			= 0x32,
		ueq_cond		= 0x33,
		olt_cond		= 0x34,
		ult_cond		= 0x35,
		ole_cond		= 0x36,
		ule_cond		= 0x37,
		sf_cond			= 0x38,
		ngle_cond		= 0x39,
		seq_cond		= 0x3a,
		ngl_cond		= 0x3b,
		lt_cond			= 0x3c,
		nge_cond		= 0x3d,
		le_cond			= 0x3e,
		ngt_cond		= 0x3f
	};

	//low 6 bits of cp1 instruction
	enum float_ops {
		fadd_op			= 0x00,
		fsub_op			= 0x01,
		fmul_op			= 0x02,
		fdiv_op			= 0x03,
		fsqrt_op		= 0x04,
		fabs_op			= 0x05,
		fmov_op			= 0x06,
		fneg_op			= 0x07,
		froundl_op	= 0x08,
		ftruncl_op	= 0x09,
		fceill_op		= 0x0a,
		ffloorl_op	= 0x0b,
		froundw_op 	= 0x0c,
		ftruncw_op	= 0x0d,
		fceilw_op 	= 0x0e,
		ffloorw_op	= 0x0f,
		fcvts_op		= 0x20,
		fcvtd_op		= 0x21,
		fcvtw_op		= 0x24,
		fcvtl_op		= 0x25,
		fpll_op		=0x2c,
		fplu_op		=0x2d,
		fpul_op		=0x2e,
		fpuu_op		=0x2f,

	};
	
	static const char* float_name[]; 

  /* 2013.10.16 Jin: merge from OpenJDK 8 */
  enum WhichOperand {
    // input to locate_operand, and format code for relocations
    imm_operand  = 0,            // embedded 32-bit|64-bit immediate operand
    disp32_operand = 1,          // embedded 32-bit displacement or address
    call32_operand = 2,          // embedded 32-bit self-relative displacement
#ifndef _LP64
    _WhichOperand_limit = 3
#else
     narrow_oop_operand = 3,     // embedded 32-bit immediate narrow oop
    _WhichOperand_limit = 4
#endif
  };

        /* Godson3 extension */
        enum godson3_ops {
                gs_ldc2_op      = 0x36, 
                gs_sdc2_op      = 0x3e, 
        };
 
 
        enum gs_ldc2_ops {
                gslbx_op        =  0x0,
                gslhx_op        =  0x1,
                gslwx_op        =  0x2,
                gsldx_op        =  0x3,
                gslwxc1_op      =  0x6,
                gsldxc1_op      =  0x7
        };

	static const char* gs_ldc2_name[]; 

        enum gs_sdc2_ops {
                gssbx_op        =  0x0,
                gsshx_op        =  0x1,
                gsswx_op        =  0x2,
                gssdx_op        =  0x3,
                gsswxc1_op      =  0x6,
                gssdxc1_op      =  0x7
        };

	static const char* gs_sdc2_name[]; 

	static int opcode(int insn) { return (insn>>26)&0x3f; }
	static int rs(int insn) { return (insn>>21)&0x1f; }
	static int rt(int insn) { return (insn>>16)&0x1f; }
	static int rd(int insn) { return (insn>>11)&0x1f; }
	static int sa(int insn) { return (insn>>6)&0x1f; }
	static int special(int insn) { return insn&0x3f; }
	static int imm_off(int insn) { return (short)low16(insn); }

	static int low  (int x, int l) { return bitfield(x, 0, l); }
	static int low16(int x)        { return low(x, 16); }
	static int low26(int x)        { return low(x, 26); }
	
protected:
	//help methods for instruction ejection

	//I-Type (Immediate)
	// 31				 26 25        21 20      16 15    													0
	//|   opcode   |			rs		|    rt    |						immediat						 |
	//| 					 |						|					 |																 |
	//			6							5					  5					 					16
	static int insn_ORRI(int op, int rs, int rt, int imm) { return (op<<26) | (rs<<21) | (rt<<16) | low16(imm); } 

	//R-Type (Register)
	// 31				  26 25       21 20      16 15      11 10				 6 5			  0
	//|   special   |			rs		|    rt    |	  rd	  | 		0		  |	 opcode  |
	//| 0 0 0 0 0 0 |						|					 |					| 0 0 0 0 0 | 				 |
	//			6							5					  5					 5					5						6
	static int insn_RRRO(int rs, int rt, int rd,   int op) { return (rs<<21) | (rt<<16) | (rd<<11)  | op; }
	static int insn_RRSO(int rt, int rd, int sa,   int op) { return (rt<<16) | (rd<<11) | (sa<<6)   | op; }
	static int insn_RRCO(int rs, int rt, int code, int op) { return (rs<<21) | (rt<<16) | (code<<6) | op; }
	
	static int insn_COP0(int op, int rt, int rd) { return (cop0_op<<26) | (op<<21) | (rt<<16) | (rd<<11); }
	static int insn_COP1(int op, int rt, int fs) { return (cop1_op<<26) | (op<<21) | (rt<<16) | (fs<<11); }

	static int insn_F3RO(int fmt, int ft, int fs, int fd, int func) { 
		return (cop1_op<<26) | (fmt<<21) | (ft<<16) | (fs<<11) | (fd<<6) | func;
	}
	

	//static int low  (int x, int l) { return bitfield(x, 0, l); }
	//static int low16(int x)        { return low(x, 16); }
	//static int low26(int x)        { return low(x, 26); }
	
	static int high  (int x, int l) { return bitfield(x, 32-l, l); }
	static int high16(int x)        { return high(x, 16); }
	static int high6 (int x)        { return high(x, 6); }

	//get the offset field of jump/branch instruction
	int offset(address entry) { 
		assert(is_simm16((entry - pc() - 4) / 4), "change this code");
		if (!is_simm16((entry - pc() - 4) / 4)) {
			tty->print_cr("!!! is_simm16: %lx", (entry - pc() - 4) / 4);
		}
		return (entry - pc() - 4) / 4; 
	}
	

public:
	using AbstractAssembler::offset;

	//sign expand with the sign bit is h
	static int expand(int x, int h) { return -(x & (1<<h)) | x;	}

	// mips lui/addiu is both sign extended, so if you wan't to use off32/imm32, you have to use the follow three
	// by yjl 6/22/2005
	static int split_low(int x) {
		return (x & 0xffff);
	}

	static int split_high(int x) {
		return ( (x >> 16) + ((x & 0x8000) != 0) ) & 0xffff;
	}

	static int merge(int low, int high) {
		return expand(low, 15) + (high<<16);
	}

#ifdef _LP64
	static intptr_t merge(intptr_t x0, intptr_t x16, intptr_t x32, intptr_t x48) {
	  return (x48 << 48) | (x32 << 32) | (x16 << 16) | x0;
	  /*
	     return ((intptr_t)(long_at(0) & 0xffff) << 48) 
	     + expand((intptr_t)(long_at(4) & 0xffff) << 32, 47)
	     + expand((intptr_t)(long_at(12) & 0xffff) << 16, 31)
	     + expand((intptr_t)(long_at(20) & 0xffff), 15);
	     return expand(low, 15) + (high<<16);
	   */
	}
#endif

	// modified by spark 2005/08/18
	static bool is_simm  (int x, int nbits) { return -( 1 << nbits-1 )  <= x   &&   x  <  ( 1 << nbits-1 ); }
	static bool is_simm16(int x)            { return is_simm(x, 16); }
	
	// test if imm can be coded in a instruction with 16-bit imm/off
	// by yjl 6/23/2005
	/*static bool fit_in_insn(int imm) {
		return imm == (short)imm;
	}*/

	static bool fit_in_jal(int offset) {
		return is_simm(offset, 26);
	}
	
	
	// test if entry can be filled in the jl/jal, 
	// must be used just before you emit jl/jal 
	// by yjl 6/27/2005 
	bool fit_int_jal(address entry) {
		return fit_in_jal(offset(entry));
	}
	
	bool fit_int_branch(address entry) {
		return is_simm16(offset(entry));
	}

protected:
#ifdef ASSERT
	  #define CHECK_DELAY
#endif
#ifdef CHECK_DELAY
	enum Delay_state { no_delay, at_delay_slot, filling_delay_slot } delay_state;
#endif
		
public:
	void assert_not_delayed() {
#ifdef CHECK_DELAY
		assert_not_delayed("next instruction should not be a delay slot");
#endif
	}

	void assert_not_delayed(const char* msg) {
#ifdef CHECK_DELAY
		//guarantee( delay_state == no_delay, msg );
		//aoqi_test
		if(delay_state != no_delay){
		tty->print_cr("%s:%d, pc: %lx", __func__, __LINE__, pc());
		}
		assert(delay_state == no_delay, msg);
#endif		
	}

protected:
	// Delay slot helpers
	// cti is called when emitting control-transfer instruction,
	// BEFORE doing the emitting.
	// Only effective when assertion-checking is enabled.
	
	// called when emitting cti with a delay slot, AFTER emitting
	void has_delay_slot() {
#ifdef CHECK_DELAY
		assert_not_delayed("just checking");
		delay_state = at_delay_slot;
#endif
	}

public:
	Assembler* delayed() {
#ifdef CHECK_DELAY
		guarantee( delay_state == at_delay_slot, "delayed instructition is not in delay slot");
		delay_state = filling_delay_slot;
#endif
		return this;						
	}

	void flush() {
#ifdef CHECK_DELAY
		guarantee( delay_state == no_delay, "ending code with a delay slot");
#endif
		AbstractAssembler::flush();
	}
	
	inline void emit_long(int);  // shadows AbstractAssembler::emit_long
	inline void emit_data(int x) { emit_long(x); }
	inline void emit_data(int, RelocationHolder const&);
	inline void emit_data(int, relocInfo::relocType rtype);		
	inline void check_delay();
 

  // Generic instructions
  // Does 32bit or 64bit as needed for the platform. In some sense these
  // belong in macro assembler but there is no need for both varieties to exist

#ifndef _LP64
	void add(Register rd, Register rs, Register rt)  { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), add_op)); }
	void addi(Register rt, Register rs, int imm)     { emit_long(insn_ORRI(addi_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }
	void addiu(Register rt, Register rs, int imm)    { emit_long(insn_ORRI(addiu_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }
	void addu(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), addu_op)); }
#else
	void add(Register rd, Register rs, Register rt)  { dadd	  (rd, rs, rt); }
	void add32(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), add_op)); }
	void addu32(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), addu_op)); }
	void addiu32(Register rt, Register rs, int imm)    { emit_long(insn_ORRI(addiu_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }
	void addi(Register rt, Register rs, int imm)     { daddi  (rt, rs, imm);}
	void addiu(Register rt, Register rs, int imm)    { daddiu (rt, rs, imm);}
	void addu(Register rd, Register rs, Register rt) { daddu  (rd, rs, rt);	}
#endif

	void andr(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), and_op)); }
	void andi(Register rt, Register rs, int imm)     { emit_long(insn_ORRI(andi_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }
	
	void beq    (Register rs, Register rt, int off) { emit_long(insn_ORRI(beq_op, (int)rs->encoding(), (int)rt->encoding(), off)); has_delay_slot(); }
	void beql   (Register rs, Register rt, int off) { emit_long(insn_ORRI(beql_op, (int)rs->encoding(), (int)rt->encoding(), off)); has_delay_slot(); }
	void bgez   (Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bgez_op, off)); has_delay_slot(); }
	void bgezal (Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bgezal_op, off)); has_delay_slot(); }
	void bgezall(Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bgezall_op, off)); has_delay_slot(); }
	void bgezl  (Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bgezl_op, off)); has_delay_slot(); }
	void bgtz   (Register rs, int off) { emit_long(insn_ORRI(bgtz_op,   (int)rs->encoding(), 0, off)); has_delay_slot(); }
	void bgtzl  (Register rs, int off) { emit_long(insn_ORRI(bgtzl_op,  (int)rs->encoding(), 0, off)); has_delay_slot(); }
	void blez   (Register rs, int off) { emit_long(insn_ORRI(blez_op,   (int)rs->encoding(), 0, off)); has_delay_slot(); }
	void blezl  (Register rs, int off) { emit_long(insn_ORRI(blezl_op,  (int)rs->encoding(), 0, off)); has_delay_slot(); }
	void bltz   (Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bltz_op, off)); has_delay_slot(); }
	void bltzal (Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bltzal_op, off)); has_delay_slot(); }
	void bltzall(Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bltzall_op, off)); has_delay_slot(); }
	void bltzl  (Register rs, int off) { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), bltzl_op, off)); has_delay_slot(); }
	void bne    (Register rs, Register rt, int off) { emit_long(insn_ORRI(bne_op,  (int)rs->encoding(), (int)rt->encoding(), off)); has_delay_slot(); }
	void bnel   (Register rs, Register rt, int off) { emit_long(insn_ORRI(bnel_op, (int)rs->encoding(), (int)rt->encoding(), off)); has_delay_slot(); }
	void brk    (int code) { emit_long(break_op | (code<<16)); }
	
	void beq    (Register rs, Register rt, address entry) { beq(rs, rt, offset(entry)); }
	void beql   (Register rs, Register rt, address entry) { beql(rs, rt, offset(entry));}
	void bgez   (Register rs, address entry) { bgez   (rs, offset(entry)); }
	void bgezal (Register rs, address entry) { bgezal (rs, offset(entry)); }
	void bgezall(Register rs, address entry) { bgezall(rs, offset(entry)); }
	void bgezl  (Register rs, address entry) { bgezl  (rs, offset(entry)); }
	void bgtz   (Register rs, address entry) { bgtz   (rs, offset(entry)); }
	void bgtzl  (Register rs, address entry) { bgtzl  (rs, offset(entry)); }
	void blez   (Register rs, address entry) { blez   (rs, offset(entry)); }
	void blezl  (Register rs, address entry) { blezl  (rs, offset(entry)); }
	void bltz   (Register rs, address entry) { bltz   (rs, offset(entry)); }
	void bltzal (Register rs, address entry) { bltzal (rs, offset(entry)); }
	void bltzall(Register rs, address entry) { bltzall(rs, offset(entry)); }
	void bltzl  (Register rs, address entry) { bltzl  (rs, offset(entry)); }
	void bne    (Register rs, Register rt, address entry) { bne(rs, rt, offset(entry)); }
	void bnel   (Register rs, Register rt, address entry) { bnel(rs, rt, offset(entry)); }
	
	void beq    (Register rs, Register rt, Label& L) { beq(rs, rt, target(L)); }
	void beql   (Register rs, Register rt, Label& L) { beql(rs, rt, target(L)); }
	void bgez   (Register rs, Label& L){ bgez   (rs, target(L)); }
	void bgezal (Register rs, Label& L){ bgezal (rs, target(L)); }
	void bgezall(Register rs, Label& L){ bgezall(rs, target(L)); }
	void bgezl  (Register rs, Label& L){ bgezl  (rs, target(L)); }
	void bgtz   (Register rs, Label& L){ bgtz   (rs, target(L)); }
	void bgtzl  (Register rs, Label& L){ bgtzl  (rs, target(L)); }
	void blez   (Register rs, Label& L){ blez   (rs, target(L)); }
	void blezl  (Register rs, Label& L){ blezl  (rs, target(L)); }
	void bltz   (Register rs, Label& L){ bltz   (rs, target(L)); }
	void bltzal (Register rs, Label& L){ bltzal (rs, target(L)); }
	void bltzall(Register rs, Label& L){ bltzall(rs, target(L)); }
	void bltzl  (Register rs, Label& L){ bltzl  (rs, target(L)); }
	void bne    (Register rs, Register rt, Label& L){ bne(rs, rt, target(L)); }
	void bnel   (Register rs, Register rt, Label& L){ bnel(rs, rt, target(L)); }

	void dadd  (Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), dadd_op)); }
	void daddi (Register rt, Register rs, int imm)     { emit_long(insn_ORRI(daddi_op,  (int)rs->encoding(), (int)rt->encoding(), imm)); }
	void daddiu(Register rt, Register rs, int imm)     { emit_long(insn_ORRI(daddiu_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }
	void daddu (Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), daddu_op)); }
	void ddiv  (Register rs, Register rt)              { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, ddiv_op));	}
	void ddivu (Register rs, Register rt)              { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, ddivu_op)); }

	void movz  (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), movz_op)); }	
	void movn  (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), movn_op)); }	

	void movt  (Register rd, Register rs) { emit_long(((int)rs->encoding() << 21) | (1 << 16) | ((int)rd->encoding() << 11) | movci_op); }	
	void movf  (Register rd, Register rs) { emit_long(((int)rs->encoding() << 21) | ((int)rd->encoding() << 11) | movci_op); }	

	void mul   (Register rd, Register rs,   Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)rd->encoding() << 11) | mul_op); }	
	void madd  (Register rs,   Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | madd_op); }	
	void msub  (Register rs,   Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | msub_op); }	

	void gsdiv   (Register rd, Register rs, Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)rd->encoding() << 11) | gsdiv_op); }	
	void gsddiv  (Register rd, Register rs, Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)rd->encoding() << 11) | gsddiv_op); }	
	void gsmod   (Register rd, Register rs, Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)rd->encoding() << 11) | gsmod_op); }	
	void gsdmod  (Register rd, Register rs, Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)rd->encoding() << 11) | gsdmod_op); }	
	void gsdmult (Register rd, Register rs, Register rt) { emit_long((special2_op << 26) | ((int)rs->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)rd->encoding() << 11) | gsdmult_op); }	

// Do mult and div need both 32-bit and 64-bit version? FIXME aoqi
//#ifndef _LP64
#if 1
	void div   (Register rs, Register rt)              { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, div_op)); }
	void divu  (Register rs, Register rt)              { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, divu_op)); }
#else
	void div   (Register rs, Register rt)              { ddiv (rs, rt);}
	void divu  (Register rs, Register rt)              { ddivu(rs, rt);}
#endif
	void dmfc0 (Register rt, FloatRegister rd)         { emit_long(insn_COP0(dmf_op, (int)rt->encoding(), (int)rd->encoding())); }
	void dmtc0 (Register rt, FloatRegister rd)         { emit_long(insn_COP0(dmt_op, (int)rt->encoding(), (int)rd->encoding())); }
	void dmult (Register rs, Register rt)              { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, dmult_op)); }
	void dmultu(Register rs, Register rt)              { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, dmultu_op)); }
	void dsll  (Register rd, Register rt , int sa)     { emit_long(insn_RRSO((int)rt->encoding(), (int)rd->encoding(), sa, dsll_op)); }
	void dsllv (Register rd, Register rt, Register rs) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), dsllv_op)); }	
	void dsll32(Register rd, Register rt , int sa)     { emit_long(insn_RRSO((int)rt->encoding(), (int)rd->encoding(), sa, dsll32_op)); }
	void dsra  (Register rd, Register rt , int sa)     { emit_long(insn_RRSO((int)rt->encoding(), (int)rd->encoding(), sa, dsra_op)); }
	void dsrav (Register rd, Register rt, Register rs) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), dsrav_op)); }	
	void dsra32(Register rd, Register rt , int sa)     { emit_long(insn_RRSO((int)rt->encoding(), (int)rd->encoding(), sa, dsra32_op)); }
	void dsrl  (Register rd, Register rt , int sa)     { emit_long(insn_RRSO((int)rt->encoding(), (int)rd->encoding(), sa, dsrl_op)); }
	void dsrlv (Register rd, Register rt, Register rs)  { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), dsrlv_op)); }	
	void dsrl32(Register rd, Register rt , int sa)     { emit_long(insn_RRSO((int)rt->encoding(), (int)rd->encoding(), sa, dsrl32_op)); }
	void dsub  (Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), dsub_op)); }
	void dsubu (Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), dsubu_op)); }

	void b(int off)       { beq(R0, R0, off); }
	void b(address entry) { b(offset(entry)); }
	void b(Label& L)      { b(target(L)); }
	
	void j(address entry);
	void jal(address entry);
	
	void jalr(Register rd, Register rs) { emit_long( ((int)rs->encoding()<<21) | ((int)rd->encoding()<<11) | jalr_op); has_delay_slot(); }
	void jalr(Register rs)              { jalr(RA, rs); }
	void jalr()                         { jalr(T9); }

	void jr(Register rs) { emit_long(((int)rs->encoding()<<21) | jr_op); has_delay_slot(); }

	void lb (Register rt, Register base, int off) { emit_long(insn_ORRI(lb_op,  (int)base->encoding(), (int)rt->encoding(), off)); }
	void lbu(Register rt, Register base, int off) { emit_long(insn_ORRI(lbu_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void ld (Register rt, Register base, int off) { emit_long(insn_ORRI(ld_op,  (int)base->encoding(), (int)rt->encoding(), off)); }
	void ldl(Register rt, Register base, int off) { emit_long(insn_ORRI(ldl_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void ldr(Register rt, Register base, int off) { emit_long(insn_ORRI(ldr_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void lh (Register rt, Register base, int off) { emit_long(insn_ORRI(lh_op,  (int)base->encoding(), (int)rt->encoding(), off)); }
	void lhu(Register rt, Register base, int off) { emit_long(insn_ORRI(lhu_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void ll (Register rt, Register base, int off) { emit_long(insn_ORRI(ll_op,  (int)base->encoding(), (int)rt->encoding(), off)); }
	void lld(Register rt, Register base, int off) { emit_long(insn_ORRI(lld_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void lui(Register rt, int imm)                { emit_long(insn_ORRI(lui_op, 0, (int)rt->encoding(), imm)); }
	void lw (Register rt, Register base, int off) { emit_long(insn_ORRI(lw_op,  (int)base->encoding(), (int)rt->encoding(), off)); }
	void lwl(Register rt, Register base, int off) { emit_long(insn_ORRI(lwl_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void lwr(Register rt, Register base, int off) { emit_long(insn_ORRI(lwr_op, (int)base->encoding(), (int)rt->encoding(), off)); }
	void lwu(Register rt, Register base, int off) { emit_long(insn_ORRI(lwu_op, (int)base->encoding(), (int)rt->encoding(), off)); }

	void lb (Register rt, Address src);
	void lbu(Register rt, Address src);
	void ld (Register rt, Address src);
	void ldl(Register rt, Address src);
	void ldr(Register rt, Address src);
	void lh (Register rt, Address src);
	void lhu(Register rt, Address src);
	void ll (Register rt, Address src);
	void lld(Register rt, Address src);
	void lw (Register rt, Address src);
	void lwl(Register rt, Address src);
	void lwr(Register rt, Address src);
	void lwu(Register rt, Address src);
	void lea(Register rt, Address src);
	
	void mfc0 (Register rt, Register rd) { emit_long(insn_COP0(mf_op, (int)rt->encoding(), (int)rd->encoding())); }
	void mfhi (Register rd)              { emit_long( ((int)rd->encoding()<<11) | mfhi_op ); }	
	void mflo (Register rd)              { emit_long( ((int)rd->encoding()<<11) | mflo_op ); }	
	void mtc0 (Register rt, Register rd) { emit_long(insn_COP0(mt_op, (int)rt->encoding(), (int)rd->encoding())); }
	void mthi (Register rs)              { emit_long( ((int)rs->encoding()<<21) | mthi_op ); }	
	void mtlo (Register rs)              { emit_long( ((int)rs->encoding()<<21) | mtlo_op ); }	
// Do mult and div need both 32-bit and 64-bit version? FIXME aoqi
//#ifndef _LP64
#if 1
	void mult (Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, mult_op)); }
	void multu(Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), 0, multu_op)); }
#else
	void mult (Register rs, Register rt) { dmult  (rs, rt); }
	void multu(Register rs, Register rt) { dmultu (rs, rt); }
#endif
	
	void nor(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), nor_op)); }
	
	void orr(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), or_op)); }
	void ori(Register rt, Register rs, int imm)     { emit_long(insn_ORRI(ori_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }

	void sb   (Register rt, Register base, int off)     { emit_long(insn_ORRI(sb_op,    (int)base->encoding(), (int)rt->encoding(), off)); }
	void sc   (Register rt, Register base, int off)     { emit_long(insn_ORRI(sc_op,    (int)base->encoding(), (int)rt->encoding(), off)); }
	void scd  (Register rt, Register base, int off)     { emit_long(insn_ORRI(scd_op,   (int)base->encoding(), (int)rt->encoding(), off)); }
	void sd   (Register rt, Register base, int off)     { emit_long(insn_ORRI(sd_op,    (int)base->encoding(), (int)rt->encoding(), off)); }
	void sdl  (Register rt, Register base, int off)     { emit_long(insn_ORRI(sdl_op,   (int)base->encoding(), (int)rt->encoding(), off)); }
	void sdr  (Register rt, Register base, int off)     { emit_long(insn_ORRI(sdr_op,   (int)base->encoding(), (int)rt->encoding(), off)); }
	void sh   (Register rt, Register base, int off)     { emit_long(insn_ORRI(sh_op,    (int)base->encoding(), (int)rt->encoding(), off)); }
//#ifndef _LP64
#if 1
	void sll  (Register rd, Register rt ,  int sa)      { emit_long(insn_RRSO((int)rt->encoding(),  (int)rd->encoding(),   sa,      sll_op)); }
	void sllv (Register rd, Register rt,   Register rs) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), sllv_op)); }
#else
	void sll  (Register rd, Register rt ,  int sa)      { dsll  (rd, rt, sa);}
	void sllv (Register rd, Register rt,   Register rs) { dsllv (rd, rt, rs); }
#endif
	void slt  (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), slt_op)); }	
	void slti (Register rt, Register rs,   int imm)     { emit_long(insn_ORRI(slti_op,  (int)rs->encoding(),   (int)rt->encoding(), imm)); }
	void sltiu(Register rt, Register rs,   int imm)     { emit_long(insn_ORRI(sltiu_op, (int)rs->encoding(),   (int)rt->encoding(), imm)); }
	void sltu (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), sltu_op)); }	
//#ifndef _LP64
#if 1
	void sra  (Register rd, Register rt ,  int sa)      { emit_long(insn_RRSO((int)rt->encoding(),  (int)rd->encoding(),   sa,      sra_op)); }
	void srav (Register rd, Register rt,   Register rs) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), srav_op)); }	
	void srl  (Register rd, Register rt ,  int sa)      { emit_long(insn_RRSO((int)rt->encoding(),  (int)rd->encoding(),   sa,      srl_op)); }
	void srlv (Register rd, Register rt,   Register rs) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), srlv_op)); }	
#else
	void sra  (Register rd, Register rt ,  int sa)      { dsra  (rd, rt, sa); }
	void srav (Register rd, Register rt,   Register rs) { dsrav (rd, rt, rs); }
	void srl  (Register rd, Register rt ,  int sa)      { dsrl  (rd, rt, sa); }
	void srlv (Register rd, Register rt,   Register rs) { dsrlv (rd, rt, rs); }	
#endif
#ifndef _LP64
	void sub  (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), sub_op)); }
	void subu (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), subu_op)); }
#else
	void sub  (Register rd, Register rs,   Register rt) { dsub  (rd, rs, rt); }
	void subu (Register rd, Register rs,   Register rt) { dsubu (rd, rs, rt); }
	void subu32 (Register rd, Register rs,   Register rt) { emit_long(insn_RRRO((int)rs->encoding(),  (int)rt->encoding(),   (int)rd->encoding(), subu_op)); }
#endif
	void sw   (Register rt, Register base, int off)     { emit_long(insn_ORRI(sw_op,    (int)base->encoding(), (int)rt->encoding(), off)); }
	void swl  (Register rt, Register base, int off)     { emit_long(insn_ORRI(swl_op,   (int)base->encoding(), (int)rt->encoding(), off)); }
	void swr  (Register rt, Register base, int off)     { emit_long(insn_ORRI(swr_op,   (int)base->encoding(), (int)rt->encoding(), off)); }
	void sync ()                                        { emit_long(sync_op); }
	void syscall(int code)                              { emit_long( (code<<6) | syscall_op ); }

	void sb(Register rt, Address dst);
	void sc(Register rt, Address dst);
	void scd(Register rt, Address dst);
	void sd(Register rt, Address dst);
	void sdl(Register rt, Address dst);
	void sdr(Register rt, Address dst);
	void sh(Register rt, Address dst);
	void sw(Register rt, Address dst);
	void swl(Register rt, Address dst);
	void swr(Register rt, Address dst);
	
	void teq  (Register rs, Register rt, int code) { emit_long(insn_RRCO((int)rs->encoding(),   (int)rt->encoding(), code, teq_op)); }
	void teqi (Register rs, int imm)               { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), teqi_op, imm)); }
	void tge  (Register rs, Register rt, int code) { emit_long(insn_RRCO((int)rs->encoding(),   (int)rt->encoding(), code, tge_op)); }
	void tgei (Register rs, int imm)               { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), tgei_op, imm)); }
	void tgeiu(Register rs, int imm)               { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), tgeiu_op, imm)); }
	void tgeu (Register rs, Register rt, int code) { emit_long(insn_RRCO((int)rs->encoding(),   (int)rt->encoding(), code, tgeu_op)); }
	void tlt  (Register rs, Register rt, int code) { emit_long(insn_RRCO((int)rs->encoding(),   (int)rt->encoding(), code, tlt_op)); }
	void tlti (Register rs, int imm)               { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), tlti_op, imm)); }
	void tltiu(Register rs, int imm)               { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), tltiu_op, imm)); }
	void tltu (Register rs, Register rt, int code) { emit_long(insn_RRCO((int)rs->encoding(),   (int)rt->encoding(), code, tltu_op)); }
	void tne  (Register rs, Register rt, int code) { emit_long(insn_RRCO((int)rs->encoding(),   (int)rt->encoding(), code, tne_op)); }
	void tnei (Register rs, int imm)               { emit_long(insn_ORRI(regimm_op, (int)rs->encoding(), tnei_op, imm)); }
	
	void xorr(Register rd, Register rs, Register rt) { emit_long(insn_RRRO((int)rs->encoding(), (int)rt->encoding(), (int)rd->encoding(), xor_op)); }
	void xori(Register rt, Register rs, int imm) { emit_long(insn_ORRI(xori_op, (int)rs->encoding(), (int)rt->encoding(), imm)); }

	void nop() 				      { emit_long(0); }
	//float instructions for mips
	void abs_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fabs_op));}
	void abs_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fabs_op));}
	void add_s(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fadd_op));}
	void add_d(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fadd_op));}
	
	void bc1f (int off) { emit_long(insn_ORRI(cop1_op, bc_op, bcf_op, off)); has_delay_slot(); }
	void bc1fl(int off) {	emit_long(insn_ORRI(cop1_op, bc_op, bcfl_op, off)); has_delay_slot(); }
	void bc1t (int off) { emit_long(insn_ORRI(cop1_op, bc_op, bct_op, off)); has_delay_slot(); }
	void bc1tl(int off) {	emit_long(insn_ORRI(cop1_op, bc_op, bctl_op, off));	has_delay_slot(); }

	void bc1f (address entry) { bc1f(offset(entry)); }
	void bc1fl(address entry) {	bc1fl(offset(entry)); }
	void bc1t (address entry) { bc1t(offset(entry)); }
	void bc1tl(address entry) {	bc1tl(offset(entry)); }
	
	void bc1f (Label& L) { bc1f(target(L)); }
	void bc1fl(Label& L) {	bc1fl(target(L)); }
	void bc1t (Label& L) { bc1t(target(L)); }
	void bc1tl(Label& L) {	bc1tl(target(L)); }
	
	void c_f_s   (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, f_cond)); }
	void c_f_d   (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, f_cond)); }
	void c_un_s  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, un_cond)); }
	void c_un_d  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, un_cond)); }
	void c_eq_s  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, eq_cond)); }
	void c_eq_d  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, eq_cond)); }
	void c_ueq_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ueq_cond)); }
	void c_ueq_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ueq_cond)); }
	void c_olt_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, olt_cond)); }
	void c_olt_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, olt_cond)); }
	void c_ult_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ult_cond)); }
	void c_ult_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ult_cond)); }
	void c_ole_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ole_cond)); }
	void c_ole_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ole_cond)); }
	void c_ule_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ule_cond)); }
	void c_ule_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ule_cond)); }
	void c_sf_s  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, sf_cond)); }
	void c_sf_d  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, sf_cond)); }
	void c_ngle_s(FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ngle_cond)); }
	void c_ngle_d(FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ngle_cond)); }
	void c_seq_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, seq_cond)); }
	void c_seq_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, seq_cond)); }
	void c_ngl_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ngl_cond)); }
	void c_ngl_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ngl_cond)); }
	void c_lt_s  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, lt_cond)); }
	void c_lt_d  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, lt_cond)); }
	void c_nge_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, nge_cond)); }
	void c_nge_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, nge_cond)); }
	void c_le_s  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, le_cond)); }
	void c_le_d  (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, le_cond)); }
	void c_ngt_s (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ngt_cond)); }
	void c_ngt_d (FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), 0, ngt_cond)); }
	
	void ceil_l_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fceill_op)); }
	void ceil_l_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fceill_op)); }
	void ceil_w_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fceilw_op)); }
	void ceil_w_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fceilw_op)); }
	void cfc1(Register rt, FloatRegister fs) { emit_long(insn_COP1(cf_op, (int)rt->encoding(), (int)fs->encoding())); }
	void ctc1(Register rt, FloatRegister fs) { emit_long(insn_COP1(ct_op, (int)rt->encoding(), (int)fs->encoding())); }
	void cfc1(Register rt, int fs) { emit_long(insn_COP1(cf_op, (int)rt->encoding(), fs)); }
	void ctc1(Register rt, int fs) { emit_long(insn_COP1(ct_op, (int)rt->encoding(), fs)); }

	void cvt_d_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtd_op)); }
	void cvt_d_w(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(word_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtd_op)); }
	void cvt_d_l(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(long_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtd_op)); }
	void cvt_l_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtl_op)); }
	void cvt_l_w(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(word_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtl_op)); }
	void cvt_l_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtl_op)); }
	void cvt_s_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvts_op)); }
	void cvt_s_w(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(word_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvts_op)); }
	void cvt_s_l(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(long_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvts_op)); }
	void cvt_w_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtw_op)); }
	void cvt_w_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtw_op)); }
	void cvt_w_l(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(long_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fcvtw_op)); }
	void pll(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(long_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fpll_op)); }
	void plu(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(long_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fplu_op)); }
	void pul(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(long_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fpul_op)); }
	void puu(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(long_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fpuu_op)); }
	
	void div_s(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fdiv_op)); }
	void div_d(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fdiv_op)); }
	void dmfc1(Register rt, FloatRegister fs) { emit_long(insn_COP1(dmf_op, (int)rt->encoding(), (int)fs->encoding())); }
	void dmtc1(Register rt, FloatRegister fs) { emit_long(insn_COP1(dmt_op, (int)rt->encoding(), (int)fs->encoding())); }

	void floor_l_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ffloorl_op)); }
	void floor_l_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ffloorl_op)); }
	void floor_w_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ffloorw_op)); }
	void floor_w_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ffloorw_op)); }
	
	void ldc1(FloatRegister ft, Register base, int off) { emit_long(insn_ORRI(ldc1_op, (int)base->encoding(), (int)ft->encoding(), off)); }
	void lwc1(FloatRegister ft, Register base, int off) { emit_long(insn_ORRI(lwc1_op, (int)base->encoding(), (int)ft->encoding(), off)); }
	void ldc1(FloatRegister ft, Address src);
	void lwc1(FloatRegister ft, Address src);
	
	void mfc1(Register rt, FloatRegister fs) { emit_long(insn_COP1(mf_op, (int)rt->encoding(), (int)fs->encoding())); }
	void mov_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fmov_op)); }
	void mov_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fmov_op)); }
	void mtc1(Register rt, FloatRegister fs) { emit_long(insn_COP1(mt_op, (int)rt->encoding(), (int)fs->encoding())); }
	void mul_s(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fmul_op)); }
	void mul_d(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fmul_op)); }

	void neg_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fneg_op)); }
	void neg_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fneg_op)); }
	
	void round_l_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), froundl_op)); }
	void round_l_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), froundl_op)); }
	void round_w_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), froundw_op)); }
	void round_w_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), froundw_op)); }
	
	void sdc1(FloatRegister ft, Register base, int off) { emit_long(insn_ORRI(sdc1_op, (int)base->encoding(), (int)ft->encoding(), off)); }
	void sdc1(FloatRegister ft, Address dst);
	void sqrt_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fsqrt_op)); }
	void sqrt_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), fsqrt_op)); }
	void sub_s(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(single_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fsub_op)); }
	void sub_d(FloatRegister fd, FloatRegister fs, FloatRegister ft) { emit_long(insn_F3RO(double_fmt, (int)ft->encoding(), (int)fs->encoding(), (int)fd->encoding(), fsub_op)); }
	void swc1(FloatRegister ft, Register base, int off) { emit_long(insn_ORRI(swc1_op, (int)base->encoding(), (int)ft->encoding(), off)); }
	void swc1(FloatRegister ft, Address dst);

	void trunc_l_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ftruncl_op)); }
	void trunc_l_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ftruncl_op)); }
	void trunc_w_s(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(single_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ftruncw_op)); }
	void trunc_w_d(FloatRegister fd, FloatRegister fs) { emit_long(insn_F3RO(double_fmt, 0, (int)fs->encoding(), (int)fd->encoding(), ftruncw_op)); }
  
	void int3();
	static void print_instruction(int);
	int patched_branch(int dest_pos, int inst, int inst_pos);
	int branch_destination(int inst, int pos);

	/* Godson3 extension */
	void gsldxc1(FloatRegister rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gsldxc1: off exceeds 8 bits");
		emit_long((gs_ldc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gsldxc1_op);
	}

	void gslwxc1(FloatRegister rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gslwxc1: off exceeds 8 bits");
		emit_long((gs_ldc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gslwxc1_op);
	}

	void gsldx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gsldx: off exceeds 8 bits");
		emit_long((gs_ldc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gsldx_op);
	}

	void gslwx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gslwx: off exceeds 8 bits");
		emit_long((gs_ldc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gslwx_op);
	}

	void gslhx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gslhx: off exceeds 8 bits");
		emit_long((gs_ldc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gslhx_op);
	}

	void gslbx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gslbx: off exceeds 8 bits");
		emit_long((gs_ldc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gslbx_op);
	}

	void gssdxc1(FloatRegister rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gssdxc1: off exceeds 8 bits");
		emit_long((gs_sdc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gssdxc1_op);
	}

	void gsswxc1(FloatRegister rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gsswxc1: off exceeds 8 bits");
		emit_long((gs_sdc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gsswxc1_op);
	}

	void gssdx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gssdx: off exceeds 8 bits");
		emit_long((gs_sdc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gssdx_op);
	}

	void gsswx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gsswx: off exceeds 8 bits");
		emit_long((gs_sdc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gsswx_op);
	}

	void gsshx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gsshx: off exceeds 8 bits");
		emit_long((gs_sdc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gsshx_op);
	}

	void gssbx(Register rt, Register base, Register index, int off) {
		assert(is_simm(off, 8), "gssbx: off exceeds 8 bits");
		emit_long((gs_sdc2_op << 26) | ((int)base->encoding() << 21) | ((int)rt->encoding() << 16) | ((int)index->encoding() << 11) | (off << 3) | gssbx_op);
	}

public:
	// Creation
	Assembler(CodeBuffer* code) : AbstractAssembler(code) {
#ifdef CHECK_DELAY
	  delay_state = no_delay;
#endif
	}

  // Decoding
  static address locate_operand(address inst, WhichOperand which);
  static address locate_next_instruction(address inst);
};


// MacroAssembler extends Assembler by frequently used macros.
//
// Instructions for which a 'better' code sequence exists depending
// on arguments should also go in here.

class MacroAssembler: public Assembler {
  friend class LIR_Assembler;
  friend class Runtime1;      // as_Address()

public:
static intptr_t	i[32];
static float	f[32];
static void print(outputStream *s);

static int i_offset(unsigned int k);
static int f_offset(unsigned int k);
	
static void save_registers(MacroAssembler *masm);
static void restore_registers(MacroAssembler *masm);

 protected:

  Address as_Address(AddressLiteral adr);
  Address as_Address(ArrayAddress adr);

  // Support for VM calls
  //
  // This is the base routine called by the different versions of call_VM_leaf. The interpreter
  // may customize this version by overriding it for its purposes (e.g., to save/restore
  // additional registers when doing a VM call).
#ifdef CC_INTERP
  // c++ interpreter never wants to use interp_masm version of call_VM
  #define VIRTUAL
#else
  #define VIRTUAL virtual
#endif

  VIRTUAL void call_VM_leaf_base(
    address entry_point,               // the entry point
    int     number_of_arguments        // the number of arguments to pop after the call
  );

  // This is the base routine called by the different versions of call_VM. The interpreter
  // may customize this version by overriding it for its purposes (e.g., to save/restore
  // additional registers when doing a VM call).
  //
  // If no java_thread register is specified (noreg) than rdi will be used instead. call_VM_base
  // returns the register which contains the thread upon return. If a thread register has been
  // specified, the return value will correspond to that register. If no last_java_sp is specified
  // (noreg) than rsp will be used instead.
  VIRTUAL void call_VM_base(           // returns the register containing the thread upon return
    Register oop_result,               // where an oop-result ends up if any; use noreg otherwise
    Register java_thread,              // the thread if computed before     ; use noreg otherwise
    Register last_java_sp,             // to set up last_Java_frame in stubs; use noreg otherwise
    address  entry_point,              // the entry point
    int      number_of_arguments,      // the number of arguments (w/o thread) to pop after the call
    bool     check_exceptions          // whether to check for pending exceptions after return
  );

  // These routines should emit JVMTI PopFrame and ForceEarlyReturn handling code.
  // The implementation is only non-empty for the InterpreterMacroAssembler,
  // as only the interpreter handles PopFrame and ForceEarlyReturn requests.
  virtual void check_and_handle_popframe(Register java_thread);
  virtual void check_and_handle_earlyret(Register java_thread);

  void call_VM_helper(Register oop_result, address entry_point, int number_of_arguments, bool check_exceptions = true);

  // helpers for FPU flag access
  // tmp is a temporary register, if none is available use noreg
  //void save_rax   (Register tmp);
  //void restore_rax(Register tmp);

 public:
  MacroAssembler(CodeBuffer* code) : Assembler(code) {}

  // Support for NULL-checks
  //
  // Generates code that causes a NULL OS exception if the content of reg is NULL.
  // If the accessed location is M[reg + offset] and the offset is known, provide the
  // offset. No explicit code generation is needed if the offset is within a certain
  // range (0 <= offset <= page_size).
  // use "teq 83, reg" in mips now, by yjl 6/20/2005
  void null_check(Register reg, int offset = -1);
  static bool needs_explicit_null_check(intptr_t offset);
	
	// Required platform-specific helpers for Label::patch_instructions.
  // They _shadow_ the declarations in AbstractAssembler, which are undefined.
  void pd_patch_instruction(address branch, address target);

  // Alignment
  void align(int modulus);

  // Misc
  //void fat_nop(); // 5 byte nop

  // Stack frame creation/removal
  void enter();
  void leave();

  // Support for getting the JavaThread pointer (i.e.; a reference to thread-local information)
  // The pointer will be loaded into the thread register.
  void get_thread(Register thread);


  // Support for VM calls
  //
  // It is imperative that all calls into the VM are handled via the call_VM macros.
  // They make sure that the stack linkage is setup correctly. call_VM's correspond
  // to ENTRY/ENTRY_X entry points while call_VM_leaf's correspond to LEAF entry points.


  void call_VM(Register oop_result,
               address entry_point,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1, Register arg_2,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1, Register arg_2, Register arg_3,
               bool check_exceptions = true);
  // Super call_VM calls - correspond to MacroAssembler::call_VM(_leaf) calls
  void super_call_VM_leaf(address entry_point);
  void super_call_VM_leaf(address entry_point, Register arg_1);
  void super_call_VM_leaf(address entry_point, Register arg_1, Register arg_2);
  void super_call_VM_leaf(address entry_point,
                          Register arg_1, Register arg_2, Register arg_3);

  // Overloadings with last_Java_sp
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               int number_of_arguments = 0,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, bool
               check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, Register arg_2,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, Register arg_2, Register arg_3,
               bool check_exceptions = true);

  void call_VM_leaf(address entry_point,
                    int number_of_arguments = 0);
  void call_VM_leaf(address entry_point,
                    Register arg_1);
  void call_VM_leaf(address entry_point,
                    Register arg_1, Register arg_2);
  void call_VM_leaf(address entry_point,
                    Register arg_1, Register arg_2, Register arg_3);

  // last Java Frame (fills frame anchor)
  void set_last_Java_frame(Register thread,
                           Register last_java_sp,
                           Register last_java_fp,
                           address last_java_pc);

  // thread in the default location (r15_thread on 64bit)
  void set_last_Java_frame(Register last_java_sp,
                           Register last_java_fp,
                           address last_java_pc);

  void reset_last_Java_frame(Register thread, bool clear_fp, bool clear_pc);

  // thread in the default location (r15_thread on 64bit)
  void reset_last_Java_frame(bool clear_fp, bool clear_pc);

  // Stores
  void store_check(Register obj);                // store check for obj - register is destroyed afterwards
  void store_check(Register obj, Address dst);   // same as above, dst is exact store location (reg. is destroyed)


  void g1_write_barrier_pre(Register obj,
#ifndef _LP64
                            Register thread,
#endif
                            Register tmp,
                            Register tmp2,
                            bool     tosca_live);
  void g1_write_barrier_post(Register store_addr,
                             Register new_val,
#ifndef _LP64
                             Register thread,
#endif
                             Register tmp,
                             Register tmp2);



  // split store_check(Register obj) to enhance instruction interleaving
  void store_check_part_1(Register obj);
  void store_check_part_2(Register obj);

  // C 'boolean' to Java boolean: x == 0 ? 0 : 1
  void c2bool(Register x);
  //add for compressedoops
  void load_klass(Register dst, Register src);
  void store_klass(Register dst, Register src);
  void load_prototype_header(Register dst, Register src);
/*
  // C++ bool manipulation

  void movbool(Register dst, Address src);
  void movbool(Address dst, bool boolconst);
  void movbool(Address dst, Register src);
  void testbool(Register dst);

  // oop manipulations
  void load_klass(Register dst, Register src);
  void store_klass(Register dst, Register src);

  void load_prototype_header(Register dst, Register src);*/

#ifdef _LP64
  void store_klass_gap(Register dst, Register src);

  void load_heap_oop(Register dst, Address src);
  void store_heap_oop(Address dst, Register src);
  void encode_heap_oop(Register r);
  void decode_heap_oop(Register r);
  void encode_heap_oop_not_null(Register r);
  void decode_heap_oop_not_null(Register r);
  void encode_heap_oop_not_null(Register dst, Register src);
  void decode_heap_oop_not_null(Register dst, Register src);

  void encode_klass_not_null(Register r);
  void decode_klass_not_null(Register r);
  void encode_klass_not_null(Register dst, Register src);
  void decode_klass_not_null(Register dst, Register src);

  //void set_narrow_oop(Register dst, jobject obj);

  // Returns the byte size of the instructions generated by decode_klass_not_null()
  // when compressed klass pointers are being used.
  static int instr_size_for_decode_klass_not_null();

  // if heap base register is used - reinit it with the correct value
  void reinit_heapbase();
  DEBUG_ONLY(void verify_heapbase(const char* msg);)

#endif // _LP64

  void incrementl(Register reg, int value = 1);

  void decrementl(Register reg, int value = 1);

/*
  // Int division/remainder for Java
  // (as idivl, but checks for special case as described in JVM spec.)
  // returns idivl instruction offset for implicit exception handling
  int corrected_idivl(Register reg);

  // Long division/remainder for Java
  // (as idivq, but checks for special case as described in JVM spec.)
  // returns idivq instruction offset for implicit exception handling
  int corrected_idivq(Register reg);
*/

  void int3();
/*
  // Long operation macros for a 32bit cpu
  // Long negation for Java
  void lneg(Register hi, Register lo);

  // Long multiplication for Java
  // (destroys contents of eax, ebx, ecx and edx)
  void lmul(int x_rsp_offset, int y_rsp_offset); // rdx:rax = x * y

  // Long shifts for Java
  // (semantics as described in JVM spec.)
  void lshl(Register hi, Register lo);                               // hi:lo << (rcx & 0x3f)
  void lshr(Register hi, Register lo, bool sign_extension = false);  // hi:lo >> (rcx & 0x3f)

  // Long compare for Java
  // (semantics as described in JVM spec.)
  void lcmp2int(Register x_hi, Register x_lo, Register y_hi, Register y_lo); // x_hi = lcmp(x, y)


  // misc
*/
  // Sign extension
#ifdef _LP64
  void sign_extend_short(Register reg) 	{ dsll32(reg, reg, 16); dsra32(reg, reg, 16); }
  void sign_extend_byte(Register reg)	{ dsll32(reg, reg, 24); dsra32(reg, reg, 24); }
#else
  void sign_extend_short(Register reg) 	{ sll(reg, reg, 16); sra(reg, reg, 16); }
  void sign_extend_byte(Register reg)	{ sll(reg, reg, 24); sra(reg, reg, 24); }
#endif
  void rem_s(FloatRegister fd, FloatRegister fs, FloatRegister ft, FloatRegister tmp);
	void rem_d(FloatRegister fd, FloatRegister fs, FloatRegister ft, FloatRegister tmp);

  // Inlined sin/cos generator for Java; must not use CPU instruction
  // directly on Intel as it does not have high enough precision
  // outside of the range [-pi/4, pi/4]. Extra argument indicate the
  // number of FPU stack slots in use; all but the topmost will
  // require saving if a slow case is necessary. Assumes argument is
  // on FP TOS; result is on FP TOS.  No cpu registers are changed by
  // this code.
  void trigfunc(char trig, int num_fpu_regs_in_use = 1);
/*
  // branch to L if FPU flag C2 is set/not set
  // tmp is a temporary register, if none is available use noreg
  void jC2 (Register tmp, Label& L);
  void jnC2(Register tmp, Label& L);

  // Pop ST (ffree & fincstp combined)
  void fpop();

  // pushes double TOS element of FPU stack on CPU stack; pops from FPU stack
  void push_fTOS();

  // pops double TOS element from CPU stack and pushes on FPU stack
  void pop_fTOS();

  void empty_FPU_stack();

  void push_IU_state();
  void pop_IU_state();

  void push_FPU_state();
  void pop_FPU_state();

  void push_CPU_state();
  void pop_CPU_state();

  // Round up to a power of two
  void round_to(Register reg, int modulus);

  // Callee saved registers handling
  void push_callee_saved_registers();
  void pop_callee_saved_registers();
*/
  // allocation
  void eden_allocate(
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Register t2,
    Label&   slow_case                 // continuation point if fast allocation fails
  );
  void tlab_allocate(
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Register t2,                       // temp register
    Label&   slow_case                 // continuation point if fast allocation fails
    );
  void tlab_refill(Label& retry_tlab, Label& try_eden, Label& slow_case);

  //----
  //  void set_word_if_not_zero(Register reg); // sets reg to 1 if not zero, otherwise 0


  // Debugging

  // only if +VerifyOops
  void verify_oop(Register reg, const char* s = "broken oop");
  void verify_oop_addr(Address addr, const char * s = "broken oop addr");
  void verify_oop_subroutine();
  // TODO: verify method and klass metadata (compare against vptr?)
  void _verify_method_ptr(Register reg, const char * msg, const char * file, int line) {}
  void _verify_klass_ptr(Register reg, const char * msg, const char * file, int line){}
  
  #define verify_method_ptr(reg) _verify_method_ptr(reg, "broken method " #reg, __FILE__, __LINE__)
  #define verify_klass_ptr(reg) _verify_klass_ptr(reg, "broken klass " #reg, __FILE__, __LINE__)
 
  // only if +VerifyFPU
  void verify_FPU(int stack_depth, const char* s = "illegal FPU state");

  // prints msg, dumps registers and stops execution
  void stop(const char* msg);

  // prints msg and continues
  void warn(const char* msg);

  static void debug(char* msg/*, RegistersForDebugging* regs*/);
  static void debug32(int rdi, int rsi, int rbp, int rsp, int rbx, int rdx, int rcx, int rax, int eip, char* msg);
  static void debug64(char* msg, int64_t pc, int64_t regs[]);

  void print_reg(Register reg);
  void print_reg(FloatRegister reg);
  //void os_breakpoint();

  void untested()                                { stop("untested"); }

  void unimplemented(const char* what = "")      { char* b = new char[1024];  jio_snprintf(b, sizeof(b), "unimplemented: %s", what);  stop(b); }

  void should_not_reach_here()                   { stop("should not reach here"); }

  void print_CPU_state();

  // Stack overflow checking
  void bang_stack_with_offset(int offset) {
    // stack grows down, caller passes positive offset
    assert(offset > 0, "must bang with negative offset");
    if (offset <= 32768) {
      sw(A0, SP, -offset);
    } else {
#ifdef _LP64
      li(AT, offset);
      dsub(AT, SP, AT);
#else
      move(AT, offset);
      sub(AT, SP, AT);
#endif
      sw(A0, AT, 0);
    }
  }

  	// Writes to stack successive pages until offset reached to check for
  	// stack overflow + shadow pages.  Also, clobbers tmp
  	void bang_stack_size(Register size, Register tmp);
        virtual RegisterOrConstant delayed_value_impl(intptr_t* delayed_value_addr,
                                                       Register tmp,
                                                       int offset);

  	// Support for serializing memory accesses between threads
  	void serialize_memory(Register thread, Register tmp);

  	//void verify_tlab();
  	void verify_tlab(Register t1, Register t2);

  	// Biased locking support
  	// lock_reg and obj_reg must be loaded up with the appropriate values.
  	// swap_reg must be rax, and is killed.
  	// tmp_reg is optional. If it is supplied (i.e., != noreg) it will
  	// be killed; if not supplied, push/pop will be used internally to
  	// allocate a temporary (inefficient, avoid if possible).
  	// Optional slow case is for implementations (interpreter and C1) which branch to
  	// slow case directly. Leaves condition codes set for C2's Fast_Lock node.
  	// Returns offset of first potentially-faulting instruction for null
  	// check info (currently consumed only by C1). If
  	// swap_reg_contains_mark is true then returns -1 as it is assumed
  	// the calling code has already passed any potential faults.
  	int biased_locking_enter(Register lock_reg, Register obj_reg,
			  Register swap_reg, Register tmp_reg,
			  bool swap_reg_contains_mark,
			  Label& done, Label* slow_case = NULL,
			  BiasedLockingCounters* counters = NULL);
	void biased_locking_exit (Register obj_reg, Register temp_reg, Label& done);


  // Calls

	void call(address entry);
	void call(address entry, relocInfo::relocType rtype); 
	void call(address entry, RelocationHolder& rh); 
	// Emit the CompiledIC call idiom
	void ic_call(address entry);

	void jmp(address entry);
	void jmp(address entry, relocInfo::relocType rtype);

	// Argument ops
	/*inline void store_int_argument(Register s, Argument& a);
	inline void store_long_argument(Register s, Argument& a);
	inline void store_float_argument(FloatRegister s, Argument& a);
	inline void store_double_argument(FloatRegister s, Argument& a);
	inline void store_ptr_argument(Register s, Argument& a);*/
inline void store_int_argument(Register s, Argument &a) {
        if(a.is_Register()) {
                move(a.as_Register(), s);
        } else {
                sw(s, a.as_caller_address());
        }
}

inline void store_long_argument(Register s, Argument &a) {
        Argument a1 = a.successor();
        if(a.is_Register() && a1.is_Register()) {
                move(a.as_Register(), s);
                move(a.as_Register(), s);
        } else {
                sd(s, a.as_caller_address());
        }
}

inline void store_float_argument(FloatRegister s, Argument &a) {
        if(a.is_Register()) {
                mov_s(a.as_FloatRegister(), s);
        } else {
                swc1(s, a.as_caller_address());
        }
}
inline void store_double_argument(FloatRegister s, Argument &a) {
        if(a.is_Register()) {
                mov_d(a.as_FloatRegister(), s);
        } else {
                sdc1(s, a.as_caller_address());
        }
}

inline void store_ptr_argument(Register s, Argument &a) {
  if(a.is_Register()) {
    move(a.as_Register(), s);
  } else {
    st_ptr(s, a.as_caller_address());
  }
}

  // Load and store values by size and signed-ness
  void load_sized_value(Register dst, Address src, size_t size_in_bytes, bool is_signed, Register dst2 = noreg);
  void store_sized_value(Address dst, Register src, size_t size_in_bytes, Register src2 = noreg);

  // interface method calling
  void lookup_interface_method(Register recv_klass,
                               Register intf_klass,
                               RegisterOrConstant itable_index,
                               Register method_result,
                               Register scan_temp,
                               Label& no_such_interface);
  // virtual method calling
  void lookup_virtual_method(Register recv_klass,
                             RegisterOrConstant vtable_index,
                             Register method_result);

	// ld_ptr will perform lw for 32 bit VMs and ld for 64 bit VMs
	// st_ptr will perform sw for 32 bit VMs and sd for 64 bit VMs
	inline void ld_ptr(Register rt, Address a){
         #ifdef _LP64
         ld(rt, a.base(), a.disp());
         #else
         lw(rt, a.base(), a.disp());
         #endif
        }
	inline void ld_ptr(Register rt, Register base, int offset16){
         #ifdef _LP64
         ld(rt, base, offset16);
         #else
         lw(rt, base, offset16);
         #endif

        }
	inline void st_ptr(Register rt, Address a){
        #ifdef _LP64
         sd(rt, a.base(), a.disp());
        #else
         sw(rt, a.base(), a.disp());
        #endif
        }
	inline void st_ptr(Register rt, Register base, int offset16) {
        #ifdef _LP64
          sd(rt, base, offset16);
        #else
          sw(rt, base, offset16);
        #endif

        }

	void ld_ptr(Register rt, Register offset, Register base);
	void st_ptr(Register rt, Register offset, Register base);

	// ld_long will perform lw for 32 bit VMs and ld for 64 bit VMs
	// st_long will perform sw for 32 bit VMs and sd for 64 bit VMs
	inline void ld_long(Register rt, Register base, int offset16);
	inline void st_long(Register rt, Register base, int offset16);
	inline void ld_long(Register rt, Address a);
	inline void st_long(Register rt, Address a);


	void ld_long(Register rt, Register offset, Register base);
	void st_long(Register rt, Register offset, Register base);
	// Regular vs. d* versions
	inline void addu_long(Register rd, Register rs, Register rt) {
        #ifdef _LP64
         daddu(rd, rs, rt);
        #else
         addu(rd, rs, rt);
        #endif
        }
	inline void addu_long(Register rd, Register rs, long imm32_64) {
        #ifdef _LP64
         daddiu(rd, rs, imm32_64);
        #else
         addiu(rd, rs, imm32_64);
        #endif

        }

	// Floating
 public:
	// swap the two byte of the low 16-bit halfword
	// this directive will use AT, be sure the high 16-bit of reg is zero
	// by yjl 6/28/2005
	void hswap(Register reg);
  	void huswap(Register reg);

	// convert big endian integer to little endian integer
  // by yjl 6/29/2005
  	void swap(Register reg);
 
  // implement the x86 instruction semantic
  // if c_reg == *dest then *dest <= x_reg
	// else c_reg <= *dest
	// the AT indicate if xchg occurred, 1 for xchged, else  0
	// by yjl 6/28/2005
	void cmpxchg(Register x_reg, Address dest, Register c_reg);
#ifdef _LP64
	void cmpxchg32(Register x_reg, Address dest, Register c_reg);
#endif
	void cmpxchg8(Register x_regLo, Register x_regHi, Address dest, Register c_regLo, Register c_regHi);


	
	void round_to(Register reg, int modulus) {
		assert_different_registers(reg, AT);
		increment(reg, modulus - 1);
		move(AT, - modulus);
		andr(reg, reg, AT);
	}

	//pop & push, added by aoqi
#ifdef _LP64
	void extend_sign(Register rh, Register rl) { stop("extend_sign"); }
	void neg(Register reg) { dsubu(reg, R0, reg); }
	void push (Register reg)      { sd  (reg, SP, -8); daddi(SP, SP, -8); }
	void push (FloatRegister reg) { sdc1(reg, SP, -8); daddi(SP, SP, -8); }
	void pop  (Register reg)      { ld  (reg, SP, 0);  daddi(SP, SP, 8); }
	void pop  (FloatRegister reg) { ldc1(reg, SP, 0);  daddi(SP, SP, 8); }
	void pop  ()                  { daddi(SP, SP, 8); }
	void pop2 ()                  { daddi(SP, SP, 16); }
#else
	void extend_sign(Register rh, Register rl) { sra(rh, rl, 31); }
	void neg(Register reg) { subu(reg, R0, reg); }
	void push (Register reg)      { sw  (reg, SP, -4); addi(SP, SP, -4); }
	void push (FloatRegister reg) { swc1(reg, SP, -4); addi(SP, SP, -4); }
	void pop  (Register reg)      { lw  (reg, SP, 0);  addi(SP, SP, 4); }
	void pop  (FloatRegister reg) { lwc1(reg, SP, 0);  addi(SP, SP, 4); }
	void pop  ()                  { addi(SP, SP, 4); }
	void pop2 ()                  { addi(SP, SP, 8); }
#endif
	void push2(Register reg1, Register reg2);
	void pop2 (Register reg1, Register reg2);
	void dpush (Register reg)     { sd  (reg, SP, -8); daddi(SP, SP, -8); }
	void dpop  (Register reg)     { ld  (reg, SP, 0);  daddi(SP, SP, 8); }

	/* branches may exceed 16-bit offset */
	void b_far(address entry);
	void b_far(Label& L);
	
	void bne_far    (Register rs, Register rt, address entry);
	void bne_far    (Register rs, Register rt, Label& L);

	void beq_far    (Register rs, Register rt, address entry);
	void beq_far    (Register rs, Register rt, Label& L);

	//move an 32-bit immediate to Register
	void move(Register reg, int imm32)  { li32(reg, imm32); }
	void li	(Register rd, long imm);
	void li	(Register rd, address addr) { li(rd, (long)addr); }
	//replace move(Register reg, int imm)
	void li32(Register rd, int imm32); // sign-extends to 64 bits on mips64
#ifdef _LP64
	void dli(Register rd, long imm) { li(rd, imm); }
	void li64(Register rd, long imm);
	void li48(Register rd, long imm);
#endif

#ifdef _LP64
	void move(Register rd, Register rs)   { dadd(rd, rs, R0); }
	void move_u32(Register rd, Register rs)   { addu32(rd, rs, R0); }
#else
	void move(Register rd, Register rs)   { add(rd, rs, R0); }
#endif
	void dmove(Register rd, Register rs)  { dadd(rd, rs, R0); }

#ifdef _LP64
  	void shl(Register reg, int sa)        { dsll(reg, reg, sa); }
  	void shr(Register reg, int sa)        { dsrl(reg, reg, sa); }
  	void sar(Register reg, int sa)        { dsra(reg, reg, sa); }
#else
  	void shl(Register reg, int sa)        { sll(reg, reg, sa); }
  	void shr(Register reg, int sa)        { srl(reg, reg, sa); }
  	void sar(Register reg, int sa)        { sra(reg, reg, sa); }
#endif

#ifndef PRODUCT
  static void pd_print_patched_instruction(address branch) {
  jint stub_inst = *(jint*) branch;
  print_instruction(stub_inst);
  ::tty->print("%s", " (unresolved)");

  }
#endif

	// the follow two might use AT register, be sure you have no meanful data in AT before you call them
	// by yjl 6/23/2005
	void increment(Register reg, int imm);
	void decrement(Register reg, int imm);

	//FIXME 
  	void empty_FPU_stack(){/*need implemented*/};

//we need 2 fun to save and resotre general register
	void pushad();
	void popad();

  // Test sub_klass against super_klass, with fast and slow paths.

  // The fast path produces a tri-state answer: yes / no / maybe-slow.
  // One of the three labels can be NULL, meaning take the fall-through.
  // If super_check_offset is -1, the value is loaded up from super_klass.
  // No registers are killed, except temp_reg.
  void check_klass_subtype_fast_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp_reg,
                                     Label* L_success,
                                     Label* L_failure,
                                     Label* L_slow_path,
                RegisterOrConstant super_check_offset = RegisterOrConstant(-1));

  // The rest of the type check; must be wired to a corresponding fast path.
  // It does not repeat the fast path logic, so don't use it standalone.
  // The temp_reg and temp2_reg can be noreg, if no temps are available.
  // Updates the sub's secondary super cache as necessary.
  // If set_cond_codes, condition codes will be Z on success, NZ on failure.
  void check_klass_subtype_slow_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp_reg,
                                     Register temp2_reg,
                                     Label* L_success,
                                     Label* L_failure,
                                     bool set_cond_codes = false);

   // Simplified, combined version, good for typical uses.
   // Falls through on failure.
   void check_klass_subtype(Register sub_klass,
                            Register super_klass,
                            Register temp_reg,
                            Label& L_success);

  // method handles (JSR 292)
  Address argument_address(RegisterOrConstant arg_slot, int extra_slot_offset = 0);

  void get_vm_result  (Register oop_result, Register thread);
  void get_vm_result_2(Register metadata_result, Register thread);
#undef VIRTUAL
  void atomic_inc32(address counter_addr, int inc, Register tmp_reg1, Register tmp_reg2);

  void fast_lock(Register obj, Register box, Register tmp, Register scr);
  void fast_unlock(Register obj, Register box, Register tmp);
};

/**
 * class SkipIfEqual:
 *
 * Instantiating this class will result in assembly code being output that will
 * jump around any code emitted between the creation of the instance and it's
 * automatic destruction at the end of a scope block, depending on the value of
 * the flag passed to the constructor, which will be checked at run-time.
 */
class SkipIfEqual {
 private:
  MacroAssembler* _masm;
  Label _label;

 public:
   SkipIfEqual(MacroAssembler*, const bool* flag_addr, bool value);
   ~SkipIfEqual();
};

#ifdef ASSERT
inline bool AbstractAssembler::pd_check_instruction_mark() { return true; }
#endif

#endif // CPU_MIPS_VM_ASSEMBLER_MIPS_HPP
