/*
 * Copyright (c) 1999, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "runtime/os.hpp"
#include "runtime/threadLocalStorage.hpp"

void MacroAssembler::int3() {
#ifndef _LP64
  int imm = (intptr_t)CAST_FROM_FN_PTR(address, os::breakpoint);
  if (is_simm16(imm)) {
    addiu(T9, R0, imm);
  } else { 
    lui(T9, split_high(imm));
    if (split_low(imm))
      addiu(T9, T9, split_low(imm)); 
  }
#else
  li(T9, CAST_FROM_FN_PTR(address, os::breakpoint));
#endif

  jalr();	
  delayed()->nop();
}

void MacroAssembler::get_thread(Register thread) {
/* 2012/3/19 Jin Guojie:
  In MIPS64, we don't use full 64-bit address space.
  Only a small range is actually used.

  Example:
  $  cat /proc/13352/maps
  120000000-120010000 r-xp 00000000 08:01 41077                            /mnt/openjdk6-mips-full/build/linux-mips64/j2sdk-image/bin/java
  12001c000-120020000 rw-p 0000c000 08:01 41077                            /mnt/openjdk6-mips-full/build/linux-mips64/j2sdk-image/bin/java
  120020000-1208dc000 rwxp 00000000 00:00 0                                [heap]
  555d574000-555d598000 r-xp 00000000 08:01 2073768                        /lib/ld-2.12.so
  555d598000-555d59c000 rw-p 00000000 00:00 0 
  ......
  558b1f8000-558b23c000 rwxp 00000000 00:00 0 
  558b23c000-558b248000 ---p 00000000 00:00 0 
  558b248000-558b28c000 rwxp 00000000 00:00 0 
  ffff914000-ffff94c000 rwxp 00000000 00:00 0                              [stack]
  ffffffc000-10000000000 r-xp 00000000 00:00 0                             [vdso]

  All stacks are positioned at 0x55________.
  Therefore, we can utilize the same algorithm used in 32-bit.
 */
#if 0 //def _LP64
  // call pthread_getspecific
  // void * pthread_getspecific(pthread_key_t key);

  pushad();
  li(A0, ThreadLocalStorage::thread_index());
  call(CAST_FROM_FN_PTR(address, pthread_getspecific), relocInfo::runtime_call_type);
  delayed()->nop();
  int off;//depending on the sd sequence in pushad();

  /*
   * Jin: in [assembler_mips.cpp] pushad(), F12 is inserted between A7 and T0.
   * Therefore, the offsets before A7 need to be adjusted by 8 bytes.
   *
   * NOTE: I have tried removing the push action of F12 from pushad(), but failed.
   * Maybe other modules in Hotspot depend on this special layout.
   */
  if (thread->encoding() >= AT->encoding() && thread->encoding() <= A7->encoding())
  {
    off = 23 - thread->encoding();
    sd(V0, SP, off * wordSize);  //sd V0 to stack, thus after popad(), thread would not be pop.
  }
  else if (thread->encoding() >= T0->encoding() && thread->encoding() <= T3->encoding())
  {
    off = 20 - thread->encoding();
    sd(V0, SP, off * wordSize);  //sd V0 to stack, thus after popad(), thread would not be pop.
  }
  else if(thread->encoding() == T8->encoding() || thread->encoding() == T9->encoding())
    {
      off = 28 - thread->encoding() ;
      sd(V0, SP, off * wordSize);  //sd V0 to stack, thus after popad(), thread would not be pop.
    }
    else
      move(thread, V0);	//thread does not push in stack.
  popad();
#elif defined(_LP64)
  /* int index = ((uintptr_t)p >> PAGE_SHIFT) & ((1UL << (SP_BITLENGTH - PAGE_SHIFT)) - 1);
   * Thread* thread = _sp_map[index]; 
   */
  Register tmp;

  if (thread == AT)
    tmp = T9;
  else
    tmp = AT;

  move(thread, SP);
  shr(thread, PAGE_SHIFT);

  push(tmp);
  li(tmp, ((1UL << (SP_BITLENGTH - PAGE_SHIFT)) - 1));
  andr(thread, thread, tmp);
  shl(thread, Address::times_ptr); // sizeof(Thread *)
  li48(tmp, (long)ThreadLocalStorage::sp_map_addr());
  add(tmp, tmp, thread);
  ld_ptr(thread, tmp, 0);
  pop(tmp);
#else // MIPS O32
  move(thread, SP);
  shr(thread, PAGE_SHIFT);
  shl(thread, 2);
  lui(AT, ThreadLocalStorage::sp_map_high());
  add(AT, AT, thread);
  lw(thread, AT, ThreadLocalStorage::sp_map_low());
#endif
}
