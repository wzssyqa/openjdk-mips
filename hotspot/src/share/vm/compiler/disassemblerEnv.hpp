/*
 * Copyright (c) 2008, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifdef USE_PRAGMA_IDENT_HDR
#pragma ident "@(#)disassemblerEnv.hpp	1.14 05/11/18 15:21:38 JVM"
#endif

// Call-back interface for external disassembler
class DisassemblerEnv {
 public:
  // printing
  virtual void print_label(intptr_t value)   = 0;
  virtual void print_raw(char* str)     = 0;
  virtual void print(char* format, ...) = 0;
  // helpers
  virtual char* string_for_offset(intptr_t value) = 0;
  virtual char* string_for_constant(unsigned char* pc, intptr_t value, int is_decimal) = 0;
};

