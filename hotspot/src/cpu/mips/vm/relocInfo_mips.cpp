/*
 * Copyright (c) 1998, 2013, Oracle and/or its affiliates. All rights reserved.
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
#include "code/relocInfo.hpp"
#include "nativeInst_mips.hpp"
#include "oops/oop.inline.hpp"
#include "runtime/safepoint.hpp"


void Relocation::pd_set_data_value(address x, intptr_t o, bool verify_only) {
#ifdef _LP64
  x += o;
  typedef Assembler::WhichOperand WhichOperand;
  WhichOperand which = (WhichOperand) format(); // that is, disp32 or imm, call32, narrow oop
  assert(which == Assembler::disp32_operand ||
         which == Assembler::narrow_oop_operand ||
         which == Assembler::imm_operand, "format unpacks ok");
  if (which == Assembler::imm_operand) {
    if (verify_only) {
      assert(nativeMovConstReg_at(addr())->data() == (long)x, "instructions must match");
    } else {
      nativeMovConstReg_at(addr())->set_data((intptr_t)(x));
    }
  } else if (which == Assembler::narrow_oop_operand) {
    // both compressed oops and compressed classes look the same
    if (Universe::heap()->is_in_reserved((oop)x)) {
      if (verify_only) {
        assert(nativeMovConstReg_at(addr())->data() == (long)oopDesc::encode_heap_oop((oop)x), "instructions must match");
      } else {
        nativeMovConstReg_at(addr())->set_data((intptr_t)(oopDesc::encode_heap_oop((oop)x)));
      }
    } else {
      if (verify_only) {
        assert(nativeMovConstReg_at(addr())->data() == (long)Klass::encode_klass((Klass*)x), "instructions must match");
      } else {
        nativeMovConstReg_at(addr())->set_data((intptr_t)(Klass::encode_klass((Klass*)x)));
      }
    }
  } else {
    // Note:  Use runtime_call_type relocations for call32_operand.
    assert(0, "call32_operand not supported in MIPS64");
  }
#else
  if (verify_only) {
    assert(*pd_address_in_code() == (x + o), "instructions must match");
  } else {
    *pd_address_in_code() = x + o;
  }
#endif // MIPS64
}


//NOTICE HERE, this relocate is not need for MIPS, since MIPS USE abosolutly target,
//Maybe We should FORGET CALL RELOCATION
address Relocation::pd_call_destination(address orig_addr) {
  intptr_t adj = 0;
  NativeInstruction* ni = nativeInstruction_at(addr());
  if (ni->is_call()) {
    return nativeCall_at(addr())->destination() + adj;
  } else if (ni->is_jump()) {
    //return nativeJump_at(addr())->jump_destination() + adj;
		return nativeGeneralJump_at(addr())->jump_destination() + adj;
  } else if (ni->is_cond_jump()) {
		return nativeCondJump_at(addr())->jump_destination() +adj;
  } else {
    ShouldNotReachHere();
    return NULL;
  }
}


void Relocation::pd_set_call_destination(address x) {
  NativeInstruction* ni = nativeInstruction_at(addr());
  if (ni->is_call()) {
    nativeCall_at(addr())->set_destination(x);
  } else if (ni->is_jump()) 
    //NativeJump* nj = nativeJump_at(addr());
    nativeGeneralJump_at(addr())->set_jump_destination(x);
  else if (ni->is_cond_jump()) 
		nativeCondJump_at(addr())->set_jump_destination(x);
  else
    { ShouldNotReachHere(); }

    // Unresolved jumps are recognized by a destination of -1
    // However 64bit can't actually produce such an address
    // and encodes a jump to self but jump_destination will
    // return a -1 as the signal. We must not relocate this
    // jmp or the ic code will not see it as unresolved.
/*
    if (nj->jump_destination() == (address) -1) {
      x = addr(); // jump to self
    }
    nj->set_jump_destination(x);
  } else if (ni->is_cond_jump()) {
    // %%%% kludge this, for now, until we get a jump_destination method
    address old_dest = nativeGeneralJump_at(addr())->jump_destination();
    address disp = Assembler::locate_operand(addr(), Assembler::call32_operand);
    *(jint*)disp += (x - old_dest);
  } else if (ni->is_mov_literal64()) {
    ((NativeMovConstReg*)ni)->set_data((intptr_t)x);
  } else {
    ShouldNotReachHere();
  }
*/
}


address* Relocation::pd_address_in_code() {
	//ShouldNotReachHere();
	return (address*)addr();
}


address Relocation::pd_get_address_from_code() {
  tty->print_cr("%s: %d", __func__, __LINE__); //aoqi_test
	NativeMovConstReg* ni = nativeMovConstReg_at(addr());
	return (address)ni->data();
}


/*
int Relocation::pd_breakpoint_size() {
  // minimum breakpoint size, in short words
  return NativeIllegalInstruction::instruction_size / sizeof(short);
}

void Relocation::pd_swap_in_breakpoint(address x, short* instrs, int instrlen) {
  Untested("pd_swap_in_breakpoint");
  if (instrs != NULL) {
    assert(instrlen * sizeof(short) == NativeIllegalInstruction::instruction_size, "enough instrlen in reloc. data");
    for (int i = 0; i < instrlen; i++) {
      instrs[i] = ((short*)x)[i];
    }
  }
  NativeIllegalInstruction::insert(x);
}

void Relocation::pd_swap_out_breakpoint(address x, short* instrs, int instrlen) {
  Untested("pd_swap_out_breakpoint");
  assert(NativeIllegalInstruction::instruction_size == sizeof(short), "right address unit for update");
  NativeInstruction* ni = nativeInstruction_at(x);
  *(short*)ni->addr_at(0) = instrs[0];
}
*/

void poll_Relocation::fix_relocation_after_move(const CodeBuffer* src, CodeBuffer* dest) {
//	Unimplemented();
}

void poll_return_Relocation::fix_relocation_after_move(const CodeBuffer* src, CodeBuffer* dest) {
//	Unimplemented();
}

void internal_pc_Relocation::fix_relocation_after_move(const CodeBuffer* src, CodeBuffer* dest) {
	address target =0;
	NativeMovConstReg* ni = nativeMovConstReg_at(addr());
	target = new_addr_for((address)ni->data(), src, dest); 
	ni->set_data((intptr_t)target);  
} 

void metadata_Relocation::pd_fix_value(address x) {
}

