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

#ifndef CPU_MIPS_VM_DISASSEMBLER_MIPS_HPP
#define CPU_MIPS_VM_DISASSEMBLER_MIPS_HPP

#ifdef USE_PRAGMA_IDENT_HDR
#pragma ident "@(#)disassembler_mips.hpp	1.16 03/12/23 16:36:15 JVM"
#endif
//by yjl /6/21/2005

// The disassembler prints out mips32 code annotated
// with Java specific information.

class Disassembler {
 private:
  // decodes one instruction and return the start of the next instruction.
  static address decode_instruction(address start, DisassemblerEnv* env);
 public:
	//CHANGE_ME BY YJL
  static bool can_decode() {
    //return (_decode_instructions != NULL) || load_library();
    return true;
  }
  static void decode(CodeBlob *cb,               outputStream* st = NULL) PRODUCT_RETURN;
  static void decode(nmethod* nm,                outputStream* st = NULL) PRODUCT_RETURN;
  static void decode(u_char* begin, u_char* end, outputStream* st = NULL) PRODUCT_RETURN;
};

#endif // CPU_MIPS_VM_DISASSEMBLER_MIPS_HPP

