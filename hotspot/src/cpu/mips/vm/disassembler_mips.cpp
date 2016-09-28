/*
 * Copyright (c) 1997, 2010, Oracle and/or its affiliates. All rights reserved.
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

#ifdef USE_PRAGMA_IDENT_SRC
#pragma ident "@(#)disassembler_mips.cpp	1.35 03/12/23 16:36:14 JVM"
#endif
//by yjl 6/21/2005
//FIXME: ugly code here, it always loads a dll/so to do actually work, and dont work for product
//change it in the future
//1/2, 07 , jerome
# include "precompiled.hpp"
# include "depChecker_mips.hpp"
# include "runtime/fprofiler.hpp"

//CHANGE_ME BY YJL
#ifndef PRODUCT

class mips32_env : public DisassemblerEnv {
 private:
  nmethod*      code;
  outputStream* output;
 public:
  mips32_env(nmethod* rcode, outputStream* routput) {
    code   = rcode;
    output = routput;
  }
  void print_label(intptr_t value);
  void print_raw(char* str) { output->print_raw(str); }
  void print(char* format, ...);
  char* string_for_offset(intptr_t value);
  char* string_for_constant(unsigned char* pc, intptr_t value, int is_decimal);
};


void mips32_env::print_label(intptr_t value) {
 if (!Universe::is_fully_initialized()) {
	 output->print(INTPTR_FORMAT, value);
	 return;
 }
  address adr = (address) value;
  if (StubRoutines::contains(adr)) {
    StubCodeDesc* desc = StubCodeDesc::desc_for(adr);
    const char * desc_name = "unknown stub";
    if (desc != NULL) {
      desc_name = desc->name();
    }
    output->print("Stub::%s", desc_name);
    if (WizardMode) output->print(" " INTPTR_FORMAT, value);
  } else {
    output->print(INTPTR_FORMAT, value); 
  }
}

void mips32_env::print(char* format, ...) {
  va_list ap;
  va_start(ap, format);
  output->vprint(format, ap);
  va_end(ap);
}

char* mips32_env::string_for_offset(intptr_t value) {
  stringStream st;
 if (!Universe::is_fully_initialized()) {
	 st.print("%d", value);
	 return st.as_string();
 }
  BarrierSet* bs = Universe::heap()->barrier_set();
  BarrierSet::Name bsn = bs->kind();
    
	if (bs->kind() == BarrierSet::CardTableModRef && (jbyte*) value == ((CardTableModRefBS*)(bs))->byte_map_base) {
    st.print("word_map_base");
  } else {
    st.print("%d", value);
  }
  return st.as_string();
}

char* mips32_env::string_for_constant(unsigned char* pc, intptr_t value, int is_decimal) {
  stringStream st;
  oop obj = NULL;
#ifndef CORE
  if (code && (obj = code->embeddedOop_at(pc))!=NULL) {
    obj->print_value_on(&st);
  } else 
#endif
  {
    if (is_decimal == 1) {
      st.print("%d", value);
    } else {
      st.print("0x%lx", value);
    }
  }
  return st.as_string();
}

#define PRINT_NOP() \
	env->print("nop");

#define PRINT_ORRI(OP) \
	env->print("%s %s, %s, 0x%x", OP, as_Register(Assembler::rt(insn))->name(), \
			as_Register(Assembler::rs(insn))->name(), \
			(short)Assembler::low16(insn) )

#define PRINT_ORRL(OP) \
	env->print("%s %s, %s, ", OP, as_Register(Assembler::rs(insn))->name(), \
			as_Register(Assembler::rt(insn))->name()); \
	env->print_label( (intptr_t)start + 4 + ((short)Assembler::low16(insn)<<2) ) 

#define PRINT_J(OP) \
	env->print((char*)OP); \
	env->print_label( ( ( (intptr_t)start + 4 ) & 0xc0000000 ) | ( Assembler::low26(insn) << 2 ) ); \
	env->print("");

#define PRINT_ORSL(OP) \
	env->print("%s %s, ", OP, as_Register(Assembler::rs(insn))->name()); \
	env->print_label( (intptr_t)start + 4 + (short)Assembler::low16(insn) ); \
	env->print("");

#define PRINT_OROB(OP) \
	env->print("%s %s, 0x%x(%s)", OP, as_Register(Assembler::rt(insn))->name(), \
			(short)Assembler::low16(insn), \
			as_Register(Assembler::rs(insn))->name() )

#define PRINT_OFOB(OP) \
	env->print("%s %s, 0x%x(%s)", OP, as_FloatRegister(Assembler::rt(insn))->name(), \
			(short)Assembler::low16(insn), \
			as_Register(Assembler::rs(insn))->name() )


#define PRINT_ORRS(OP) \
	env->print("%s %s, %s, %d", OP, as_Register(Assembler::rd(insn))->name(), \
			as_Register(Assembler::rt(insn))->name(), \
			Assembler::sa(insn) )

#define PRINT_ORRR(OP) \
	env->print("%s %s, %s, %s", OP, as_Register(Assembler::rd(insn))->name(), \
			as_Register(Assembler::rs(insn))->name(), \
			as_Register(Assembler::rt(insn))->name() )

#define PRINT_ORRRI_GSLDC2(OP) \
	env->print("%s %s, %s, %s, 0x%x", OP,as_Register(Assembler::rt(insn))->name(), \
			as_Register(Assembler::rs(insn))->name(), \
                        as_Register(Assembler::rd(insn))->name(), \
                        ((short)Assembler::low(insn, 11) >> 3) )

#define PRINT_ORRR_2(OP) \
	env->print("%s %s, %s, %s", OP, as_Register(Assembler::rd(insn))->name(), \
			as_Register(Assembler::rt(insn))->name(), \
			as_Register(Assembler::rs(insn))->name() )

#define PRINT_ORS(OP) \
	env->print("%s %s", OP, as_Register(Assembler::rs(insn))->name())

#define PRINT_ORD(OP) \
	env->print("%s %s", OP, as_Register(Assembler::rd(insn))->name())

#define PRINT_ORR(OP) \
	env->print("%s %s, %s", OP, as_Register(Assembler::rs(insn))->name(), \
			as_Register(Assembler::rt(insn))->name())

#define PRINT_ORR_2(OP) \
	env->print("%s %s, %s", OP, as_Register(Assembler::rt(insn))->name(), \
			as_Register(Assembler::rd(insn))->name())

#define PRINT_FLOAT(OP) \
	env->print("%s.%s %s, %s, %s", OP, fmt, as_FloatRegister(Assembler::sa(insn))->name(), \
			as_FloatRegister(Assembler::rd(insn))->name(),  \
			as_FloatRegister(Assembler::rt(insn))->name() )

#define PRINT_CVT(OP) \
	env->print("%s.%s %s, %s", OP, fmt, as_FloatRegister(Assembler::sa(insn))->name(), \
			 as_FloatRegister(Assembler::rd(insn))->name() )

static const char* fmt_str(int fmt) {
	switch(fmt) {
	case Assembler::single_fmt:
		return "s";
	case Assembler::double_fmt:
		return "d";
	case Assembler::word_fmt:
		return "w";
	case Assembler::long_fmt:
		return "l";
	}

	return "";
}

address Disassembler::decode_instruction(address start, DisassemblerEnv* env) {
	int insn = *(int*)start;
	int opcode = Assembler::opcode(insn);
	int special;
	const char *fmt;
        int flag = 0;

	if (insn == 0)
	{
		PRINT_NOP();
		return start+4;
	}

	switch(opcode) {
	case Assembler::special_op:
		special = Assembler::special(insn);
		switch(special) {
		case Assembler::sll_op:
		case Assembler::srl_op:
		case Assembler::sra_op:
		case Assembler::dsll_op:
		case Assembler::dsrl_op:
		case Assembler::dsra_op:
		case Assembler::dsll32_op:
		case Assembler::dsrl32_op:
		case Assembler::dsra32_op:
			PRINT_ORRS(Assembler::special_name[special]);
			break;
			
		case Assembler::movci_op:
                        flag = insn & (1 << 16);
                        if (flag) {
                            env->print("movt %s, %s", as_Register(Assembler::rd(insn))->name(), as_Register(Assembler::rs(insn))->name());
                        } else {
                            env->print("movf %s, %s", as_Register(Assembler::rd(insn))->name(), as_Register(Assembler::rs(insn))->name());
                        }
			break;

		case Assembler::sllv_op:
		case Assembler::srlv_op:
		case Assembler::srav_op:
		case Assembler::dsllv_op:
		case Assembler::dsrlv_op:
		case Assembler::dsrav_op:
			PRINT_ORRR_2(Assembler::special_name[special]);
			break;

		case Assembler::jr_op:
		case Assembler::jalr_op:
		case Assembler::mthi_op:
		case Assembler::mtlo_op:
			PRINT_ORS(Assembler::special_name[special]);
			break;
			
		case Assembler::syscall_op:
		case Assembler::break_op:
			env->print("%s 0x%x\n", Assembler::special_name[special], bitfield(insn, 6, 20)>>10);
			break;
					
		case Assembler::sync_op:
			env->print("sync\n");
			break;

		case Assembler::mfhi_op:
		case Assembler::mflo_op:
			PRINT_ORD(Assembler::special_name[special]);
			break;

		case Assembler::mult_op:
		case Assembler::multu_op:
		case Assembler::div_op:
		case Assembler::divu_op:
		case Assembler::dmult_op:
		case Assembler::dmultu_op:
		case Assembler::ddiv_op:
		case Assembler::ddivu_op:
			PRINT_ORR(Assembler::special_name[special]);
			break;

		case Assembler::add_op:
		case Assembler::addu_op:
		case Assembler::sub_op:
		case Assembler::subu_op:
		case Assembler::and_op:
		case Assembler::or_op:
		case Assembler::xor_op:
		case Assembler::nor_op:
		case Assembler::slt_op:
		case Assembler::sltu_op:
		case Assembler::movz_op:
		case Assembler::movn_op:
		case Assembler::dadd_op:
		case Assembler::daddu_op:
		case Assembler::dsub_op:
		case Assembler::dsubu_op:
			PRINT_ORRR(Assembler::special_name[special]);
			break;

		case Assembler::tge_op:
		case Assembler::tgeu_op:
		case Assembler::tlt_op:
		case Assembler::tltu_op:
		case Assembler::teq_op:
		case Assembler::tne_op:
			env->print("%s 0x%x, %s, %s\n", Assembler::special_name[special], bitfield(insn, 6, 10), 
					as_Register(Assembler::rs(insn))->name(),
					as_Register(Assembler::rt(insn))->name() );
			break;

		default:
			//Unimplemented();
			env->print("0x%x\n", insn);
		}
		break;
		
	case Assembler::special2_op:
		special = Assembler::special(insn);
		switch(special) {
		case Assembler::mul_op:
		case Assembler::gsdiv_op:
		case Assembler::gsddiv_op:
		case Assembler::gsmod_op:
		case Assembler::gsdmod_op:
		case Assembler::gsdmult_op:
			PRINT_ORRR(Assembler::special2_name[special]);
			break;
		case Assembler::madd_op:
		case Assembler::msub_op:
			PRINT_ORR(Assembler::special2_name[special]);
			break;
		}
		break;

	case Assembler::regimm_op:
		special	= Assembler::rt(insn);
		
		switch(special) {
		case Assembler::bltz_op:
		case Assembler::bgez_op:
		case Assembler::bltzl_op:
		case Assembler::bgezl_op:
		case Assembler::bltzal_op:
		case Assembler::bgezal_op:
		case Assembler::bltzall_op:
		case Assembler::bgezall_op:
			env->print("[%lx]%s %s, ", *(int *)start, Assembler::regimm_name[special], as_Register(Assembler::rs(insn))->name());
			env->print_label( (intptr_t)start + 4 + 4 * (short)Assembler::low16(insn) );
			env->print("\n");
			break;

		case Assembler::tgei_op:
		case Assembler::tgeiu_op:
		case Assembler::tlti_op:
		case Assembler::tltiu_op:
		case Assembler::teqi_op:
		case Assembler::tnei_op:
			env->print("%s %s, %d\n", Assembler::regimm_name[special], 
					as_Register(Assembler::rs(insn))->name(),
					(short)Assembler::low16(insn));
			break;

		default:
			//Unimplemented();
			env->print("0x%x\n", insn);
		}
		break;

	case Assembler::j_op:
	case Assembler::jal_op:
		PRINT_J(Assembler::ops_name[opcode]);
		break;
		
	case Assembler::beq_op:
	case Assembler::bne_op:
	case Assembler::blez_op:
	case Assembler::bgtz_op:
		PRINT_ORRL(Assembler::ops_name[opcode]);
		break;
		
	case Assembler::addi_op:
	case Assembler::addiu_op:
	case Assembler::slti_op:
	case Assembler::sltiu_op:
	case Assembler::ori_op:
	case Assembler::andi_op:
	case Assembler::xori_op:
	case Assembler::daddi_op:
	case Assembler::daddiu_op:
		PRINT_ORRI(Assembler::ops_name[opcode]);
		break;
		
	case Assembler::lui_op:
		env->print("lui %s, 0x%x", as_Register(Assembler::rt(insn))->name(), (short)Assembler::low16(insn) ); \
		break;

	case Assembler::cop1_op:
		special = Assembler::rs(insn);
		switch(special) {
		case Assembler::mf_op:
			PRINT_ORR_2("mfc1");
			break;
		case Assembler::mt_op:
			PRINT_ORR_2("mtc1");
			break;
		case Assembler::cf_op:
			PRINT_ORR_2("cfc1");
			break;
		case Assembler::ct_op:
			PRINT_ORR_2("ctc1");
			break;
		case Assembler::dmf_op:
			PRINT_ORR_2("dmfc1");
			break;
		case Assembler::dmt_op:
			PRINT_ORR_2("dmtc1");
			break;
		
		case Assembler::bc_op:
			special = Assembler::rt(insn);
			switch(special) {
			case Assembler::bcf_op:
				env->print("bc1f ");
				env->print_label( (intptr_t)start + 4 + (short)Assembler::low16(insn) );
				env->print("\n");
				break;
			case Assembler::bcfl_op:
				env->print("bc1fl ");
				env->print_label( (intptr_t)start + 4 + (short)Assembler::low16(insn) );
				env->print("\n");
				break;
			case Assembler::bct_op:
				env->print("bc1t ");
				env->print_label( (intptr_t)start + 4 + (short)Assembler::low16(insn) );
				env->print("\n");
				break;
			case Assembler::bctl_op:
				env->print("bc1tl ");
				env->print_label( (intptr_t)start + 4 + (short)Assembler::low16(insn) );
				env->print("\n");
				break;
			default:
				//Unimplemented();
			env->print("0x%x\n", insn);
			}
			break;
		case Assembler::single_fmt:
		case Assembler::double_fmt:
		case Assembler::word_fmt:
		case Assembler::long_fmt:
			fmt = fmt_str(special);	
			special = Assembler::special(insn);
			switch(special) {
			case Assembler::fadd_op:
			case Assembler::fsub_op:
			case Assembler::fmul_op:
			case Assembler::fdiv_op:
			case Assembler::fsqrt_op:
			case Assembler::fabs_op:
			case Assembler::fmov_op:
			case Assembler::fneg_op:
			case Assembler::froundl_op:
			case Assembler::ftruncl_op:
			case Assembler::fceill_op:
			case Assembler::ffloorl_op:
			case Assembler::froundw_op:
			case Assembler::ftruncw_op:
			case Assembler::fceilw_op:
			case Assembler::ffloorw_op:
				PRINT_FLOAT(Assembler::float_name[special]);
				break;

			case Assembler::fcvts_op:
				PRINT_CVT("cvt.s");
				break;
			case Assembler::fcvtd_op:
				PRINT_CVT("cvt.d");
				break;
			case Assembler::fcvtw_op:
				PRINT_CVT("cvt.w");
				break;
			case Assembler::fcvtl_op:
				PRINT_CVT("cvt.l");
				break;
			default:
				//tty->print_cr("0x%x(%x)", insn, opcode);
				//Unimplemented();
			env->print("0x%x\n", insn);
			}
		}
		break;
		
	case Assembler::beql_op:
	case Assembler::bnel_op:
	case Assembler::blezl_op:
	case Assembler::bgtzl_op:
		PRINT_ORRL(Assembler::ops_name[opcode]);
		break;

	case Assembler::ldl_op:
	case Assembler::ldr_op:
	case Assembler::lb_op:
	case Assembler::lh_op:
	case Assembler::lwl_op:
	case Assembler::lw_op:
	case Assembler::lbu_op:
	case Assembler::lhu_op:
	case Assembler::lwr_op:
	case Assembler::lwu_op:
	case Assembler::sb_op:
	case Assembler::sh_op:
	case Assembler::swl_op:
	case Assembler::sw_op:
	case Assembler::sdl_op:
	case Assembler::sdr_op:
	case Assembler::swr_op:
	case Assembler::ll_op:
	case Assembler::lld_op:
	case Assembler::ld_op:
	case Assembler::sc_op:
	case Assembler::scd_op:
	case Assembler::sd_op:
		PRINT_OROB(Assembler::ops_name[opcode]);
		break;
	case Assembler::sdc1_op:
	case Assembler::ldc1_op:
	case Assembler::lwc1_op:
	case Assembler::swc1_op:
		PRINT_OFOB(Assembler::ops_name[opcode]);
		break;

	case Assembler::gs_ldc2_op:
		special = Assembler::special(insn) & 0x7;
                PRINT_ORRRI_GSLDC2(Assembler::gs_ldc2_name[special]);
		break;

	case Assembler::gs_sdc2_op:
		special = Assembler::special(insn) & 0x7;
                PRINT_ORRRI_GSLDC2(Assembler::gs_sdc2_name[special]);
		break;

	default:
		//tty->print_cr("0x%x(%x)", insn, opcode);
		//Unimplemented();
			env->print("0x%x\n", insn);
	}

	return start+4;
}

/*
void Disassembler::decode(address start, address end, outputStream* st, CodeComments c) {
  if (!load_library())  return;
  decode_env env(CodeCache::find_blob_unsafe(start), st, c);
  env.decode_instructions(start, end);
}*/

void Disassembler::decode(CodeBlob* cb, outputStream* st) {
#ifndef CORE
  st = st ? st : tty;
  st->print_cr("Decoding CodeBlob " INTPTR_FORMAT, cb);
  decode(cb->content_begin(), cb->content_end(), st);
#endif
}


void Disassembler::decode(u_char* begin, u_char* end, outputStream* st) {
  st = st ? st : tty;

  const int show_bytes = false; // for disassembler debugging

  mips32_env env(NULL, st);
  unsigned char*  p = (unsigned char*) begin;
  CodeBlob* cb = CodeCache::find_blob_unsafe(begin);
  while (p < (unsigned char*) end) {
  if (cb != NULL) {
	  cb->print_block_comment(st, (unsigned char*)(p - cb->content_begin()));
  }

	  
    unsigned char* p0 = p;
    st->print("   "INTPTR_FORMAT ": ", p);
    p = decode_instruction(p, &env);
    if (show_bytes) {
      st->print("\t\t\t");
      while (p0 < p) st->print("%x ", *p0++);
    }
    st->cr();
  }
}


void Disassembler::decode(nmethod* nm, outputStream* st) {
#ifndef CORE
  st = st ? st : tty;

  st->print_cr("Decoding compiled method " INTPTR_FORMAT ":", nm);
  st->print("Code:");
  st->cr();
  
  mips32_env env(nm, st);
#ifdef COMPILER1
  unsigned char* p = nm->code_begin();
#else
  unsigned char* p = nm->content_begin();
#endif
  unsigned char* end = nm->content_end();
  while (p < end) {
    if (p == nm->entry_point())             st->print_cr("[Entry Point]");
    if (p == nm->verified_entry_point())    st->print_cr("[Verified Entry Point]");
    if (p == nm->exception_begin())         st->print_cr("[Exception Handler]");
    if (p == nm->stub_begin())              st->print_cr("[Stub Code]");
    if (p == nm->consts_begin())            st->print_cr("[Constants]");
    nm->print_block_comment(st, (unsigned char*)(p - nm->content_begin()));
    unsigned char* p0 = p;
    st->print("  " INTPTR_FORMAT ": ", p);
    p = decode_instruction(p, &env);
    nm->print_code_comment_on(st, 40, p0, p);
    st->cr();
    // Output pc bucket ticks if we have any
    address bucket_pc = FlatProfiler::bucket_start_for(p);
    if (bucket_pc != NULL && bucket_pc > p0 && bucket_pc <= p) {
      int bucket_count = FlatProfiler::bucket_count_for(bucket_pc);
      tty->print_cr("[%d]", bucket_count);
    } 
  }
#endif
}

#endif // PRODUCT

