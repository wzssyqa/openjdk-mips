/*
 * Copyright (c) 2003, 2013, Oracle and/or its affiliates. All rights reserved.
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

// This file holds the platform specific parts of the StubRoutines
// definition. See stubRoutines.hpp for a description on how to
// extend it.

//static bool    returns_to_call_stub(address return_pc)   { return return_pc == _call_stub_return_address; }
  static bool    returns_to_call_stub(address return_pc){ 	
	  return return_pc == _call_stub_return_address||return_pc == gs2::get_call_stub_compiled_return(); 
  }

enum platform_dependent_constants
{
  //code_size1 =  19000, // simply increase if too small (assembler will
                      // crash if too small)
  //code_size2 = 22000  // simply increase if too small (assembler will
                      // crash if too small)
  code_size1 = 20000,		// simply increase if too small (assembler will crash if too small)
  code_size2 = 40000		// simply increase if too small (assembler will crash if too small)
};
//aoqi:FIXME class name gs2?
class gs2 {
	friend class StubGenerator;
 	friend class VMStructs;
private:
	// If we call compiled code directly from the call stub we will
  // need to adjust the return back to the call stub to a specialized
  // piece of code that can handle compiled results and cleaning the fpu
  // stack. The variable holds that location.
	static address _call_stub_compiled_return;  
	static address _get_previous_fp_entry;

public:
  // Call back points for traps in compiled code
	static address get_previous_fp_entry()     { return _get_previous_fp_entry; }
  static address get_call_stub_compiled_return()    { return _call_stub_compiled_return; }
  static void set_call_stub_compiled_return(address ret){ _call_stub_compiled_return = ret; }

};
