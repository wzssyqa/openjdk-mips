/*
 * Copyright (c) 1997, 2011, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_MIPS_VM_INTERPRETER_MIPS_HPP
#define CPU_MIPS_VM_INTERPRETER_MIPS_HPP

 public:

  // Sentinel placed in the code for interpreter returns so
  // that i2c adapters and osr code can recognize an interpreter
  // return address and convert the return to a specialized
  // block of code to handle compiedl return values and cleaning
  // the fpu stack.
  static const int return_sentinel;

  static Address::ScaleFactor stackElementScale() {
//    return TaggedStackInterpreter? Address::times_8 : Address::times_4;
    return NOT_LP64(Address::times_4)LP64_ONLY(Address::times_8);
  }

  // Offset from rsp (which points to the last stack element)
  static int expr_offset_in_bytes(int i) { return stackElementSize*i ; }
	// Size of interpreter code.  Increase if too small.  Interpreter will
  // fail with a guarantee ("not enough space for interpreter generation");
  // if too small.
  // Run with +PrintInterpreterSize to get the VM to print out the size.
  // Max size with JVMTI and TaggedStackInterpreter
  const static int InterpreterCodeSize = 168 * 1024;
#if 0
  // Support for Tagged Stacks

  // Stack index relative to tos (which points at value)
  static int expr_index_at(int i)     {
    return stackElementWords() * i;
  }

  static int expr_tag_index_at(int i) {
    assert(TaggedStackInterpreter, "should not call this");
    // tag is one word above java stack element
    return stackElementWords() * i + 1;
  }

  // Already negated by c++ interpreter
  static int local_index_at(int i)     {
    assert(i<=0, "local direction already negated");
    return stackElementWords() * i + (value_offset_in_bytes()/wordSize);
  }

  static int local_tag_index_at(int i) {
    assert(i<=0, "local direction already negated");
    assert(TaggedStackInterpreter, "should not call this");
    return stackElementWords() * i + (tag_offset_in_bytes()/wordSize);
  }
#endif
#endif // CPU_MIPS_VM_INTERPRETER_MIPS_HPP

