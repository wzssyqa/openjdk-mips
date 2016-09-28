/*
 * Copyright (c) 1997, 2012, Oracle and/or its affiliates. All rights reserved.
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
#include "asm/macroAssembler.inline.hpp"
#include "code/icBuffer.hpp"
#include "gc_interface/collectedHeap.inline.hpp"
#include "interpreter/bytecodes.hpp"
#include "memory/resourceArea.hpp"
#include "nativeInst_mips.hpp"
#include "oops/oop.inline.hpp"
#include "oops/oop.inline2.hpp"

int InlineCacheBuffer::ic_stub_code_size() {
  return NativeMovConstReg::instruction_size +
         NativeGeneralJump::instruction_size +
         1;
  // so that code_end can be set in CodeBuffer
  // 64bit 15 = 6 + 8 bytes + 1 byte
  // 32bit 7 = 2 + 4 bytes + 1 byte
}


// we use T1 as cached oop(klass) now. this is the target of virtual call,
// when reach here, the receiver in T0
// refer to shareRuntime_mips.cpp,gen_i2c2i_adapters 
void InlineCacheBuffer::assemble_ic_buffer_code(address code_begin, void* cached_value, address entry_point) {
  ResourceMark rm;
  CodeBuffer      code(code_begin, ic_stub_code_size());
  MacroAssembler* masm            = new MacroAssembler(&code);
  // note: even though the code contains an embedded oop, we do not need reloc info
  // because
  // (1) the oop is old (i.e., doesn't matter for scavenges)
  // (2) these ICStubs are removed *before* a GC happens, so the roots disappear
//  assert(cached_oop == NULL || cached_oop->is_perm(), "must be perm oop");
#define __ masm->
#ifndef _LP64
  __ lui(T1, Assembler::split_high((int)cached_value));
  __ addiu(T1, T1, Assembler::split_low((int)cached_value));

  __ lui(T9, Assembler::split_high((int)entry_point));
  __ addiu(T9, T9, Assembler::split_low((int)entry_point));
#else
  __ li48(T1, (long)cached_value);

  __ li48(T9, (long)entry_point);
#endif
  __ jr(T9);
  __ delayed()->nop();
  __ flush();
#undef __ 
}


address InlineCacheBuffer::ic_buffer_entry_point(address code_begin) {
  NativeMovConstReg* move = nativeMovConstReg_at(code_begin);   // creation also verifies the object
  //NativeJump*        jump = nativeJump_at(move->next_instruction_address());
  NativeGeneralJump*        jump = nativeGeneralJump_at(move->next_instruction_address());
  return jump->jump_destination();
}

void* InlineCacheBuffer::ic_buffer_cached_value(address code_begin) {
  // creation also verifies the object
  NativeMovConstReg* move = nativeMovConstReg_at(code_begin);
  // Verifies the jump
  //NativeJump*        jump = nativeJump_at(move->next_instruction_address());
  NativeGeneralJump*        jump = nativeGeneralJump_at(move->next_instruction_address());
  void* o= (void*)move->data();  
  return o;
}
