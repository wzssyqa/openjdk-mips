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

#ifndef CPU_MIPS_VM_ASSEMBLER_MIPS_INLINE_HPP
#define CPU_MIPS_VM_ASSEMBLER_MIPS_INLINE_HPP

 #include "asm/assembler.inline.hpp"
 #include "asm/codeBuffer.hpp"
 #include "code/codeCache.hpp"

/*
inline void MacroAssembler::pd_patch_instruction(address branch, address target) {
  jint& stub_inst = *(jint*) branch;
  stub_inst = patched_branch(target - branch, stub_inst, 0);
}
*/

#ifndef PRODUCT
/*
inline void MacroAssembler::pd_print_patched_instruction(address branch) {
  jint stub_inst = *(jint*) branch;
  print_instruction(stub_inst);
  ::tty->print("%s", " (unresolved)");
}
*/
#endif // PRODUCT

//inline bool Address::is_simm13(int offset) { return Assembler::is_simm13(disp() + offset); }


inline void Assembler::check_delay() {
# ifdef CHECK_DELAY
//  guarantee( delay_state != at_delay_slot, "must say delayed() when filling delay slot");
  delay_state = no_delay;
# endif
}

inline void Assembler::emit_long(int x) {
  check_delay();
  AbstractAssembler::emit_int32(x);
}

inline void Assembler::emit_data(int x, relocInfo::relocType rtype) {
  relocate(rtype);
  emit_long(x);
}

inline void Assembler::emit_data(int x, RelocationHolder const& rspec) {
  relocate(rspec);
  emit_long(x);
}
/*
inline void MacroAssembler::store_int_argument(Register s, Argument &a) {
	if(a.is_Register()) {
		move(a.as_Register(), s);
	} else {
		sw(s, a.as_caller_address());
	}
}

inline void MacroAssembler::store_long_argument(Register s, Argument &a) {
	Argument a1 = a.successor();
	if(a.is_Register() && a1.is_Register()) {
		move(a.as_Register(), s);
		move(a.as_Register(), s);
	} else {
		sd(s, a.as_caller_address());
	}
}

inline void MacroAssembler::store_float_argument(FloatRegister s, Argument &a) {
	if(a.is_Register()) {
		mov_s(a.as_FloatRegister(), s);
	} else {
		swc1(s, a.as_caller_address());
	}
}

inline void MacroAssembler::store_double_argument(FloatRegister s, Argument &a) {
	if(a.is_Register()) {
		mov_d(a.as_FloatRegister(), s);
	} else {
		sdc1(s, a.as_caller_address());
	}
}

inline void MacroAssembler::store_ptr_argument(Register s, Argument &a) {
  if(a.is_Register()) {
    move(a.as_Register(), s);
  } else {
    st_ptr(s, a.as_caller_address());
  }
}
inline void MacroAssembler::ld_ptr(Register rt, Register base, int offset16) {
#ifdef _LP64
  ld(rt, base, offset16);
#else
  lw(rt, base, offset16);
#endif
}
inline void MacroAssembler::ld_ptr(Register rt, Address a) {
#ifdef _LP64
  ld(rt, a.base(), a.disp());
#else
  lw(rt, a.base(), a.disp());
#endif
}

inline void MacroAssembler::st_ptr(Register rt, Address a) {
#ifdef _LP64
  sd(rt, a.base(), a.disp());
#else
  sw(rt, a.base(), a.disp());
#endif
}

inline void MacroAssembler::st_ptr(Register rt, Register base, int offset16) {
#ifdef _LP64
  sd(rt, base, offset16);
#else
  sw(rt, base, offset16);
#endif
}

inline void MacroAssembler::ld_long(Register rt, Register base, int offset16) {
#ifdef _LP64
  ld(rt, base, offset16);
#else
  lw(rt, base, offset16);
#endif
}

inline void MacroAssembler::st_long(Register rt, Register base, int offset16) {
#ifdef _LP64
  sd(rt, base, offset16);
#else
  sw(rt, base, offset16);
#endif
}

inline void MacroAssembler::ld_long(Register rt, Address a) {
#ifdef _LP64
  ld(rt, a.base(), a.disp());
#else
  lw(rt, a.base(), a.disp());
#endif
}

inline void MacroAssembler::st_long(Register rt, Address a) {
#ifdef _LP64
  sd(rt, a.base(), a.disp());
#else
  sw(rt, a.base(), a.disp());
#endif
}

inline void MacroAssembler::addu_long(Register rd, Register rs, Register rt) {
#ifdef _LP64
  daddu(rd, rs, rt);
#else
  addu(rd, rs, rt);
#endif
}

inline void MacroAssembler::addu_long(Register rd, Register rs, long imm32_64) {
#ifdef _LP64
  daddiu(rd, rs, imm32_64);
#else
  addiu(rd, rs, imm32_64);
#endif
} */ 

#endif // CPU_MIPS_VM_ASSEMBLER_MIPS_INLINE_HPP

