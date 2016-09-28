/*
 * Copyright (c) 2003, 2014, Oracle and/or its affiliates. All rights reserved.
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
#include "interpreter/bytecodeHistogram.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterGenerator.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "interpreter/templateTable.hpp"
#include "oops/arrayOop.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "oops/oop.inline.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/arguments.hpp"
#include "runtime/deoptimization.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/synchronizer.hpp"
#include "runtime/timer.hpp"
#include "runtime/vframeArray.hpp"
#include "utilities/debug.hpp"
#ifdef COMPILER1
#include "c1/c1_Runtime1.hpp"
#endif

#define __ _masm->


address AbstractInterpreterGenerator::generate_slow_signature_handler() {
  address entry = __ pc();

  // Rmethod: method
  // LVP: pointer to locals
  // T3: first stack arg - wordSize
  __ move(T3, SP);
  __ pushad();
  __ call_VM(noreg,
      CAST_FROM_FN_PTR(address,
	    InterpreterRuntime::slow_signature_handler),
	    Rmethod, LVP, T3);
  __ move(S0, V0);
  __ popad();
  __ move(V0, S0);

  // V0: result handler

  // Stack layout:
  //        ...

  // Do FP first so we can use c_rarg3 as temp
  __ ld(T3, Address(SP, -1 * wordSize)); // float/double identifiers

  // A0 is for env. 
  // If the mothed is not static, A1 will be corrected in generate_native_entry.
  for ( int i= 1; i < Argument::n_register_parameters; i++ ) {
    Register reg = as_Register(i + A0->encoding());
    FloatRegister floatreg = as_FloatRegister(i + F12->encoding());
    Label isfloatordouble, isdouble, next;

    __ andi(AT, T3, 1 << (i*2));      // Float or Double?
    __ bne(AT, R0, isfloatordouble);
    __ delayed()->nop();

    // Do Int register here
    __ ld(reg, Address(SP, -(Argument::n_register_parameters + 1 -i) * wordSize));
    __ b (next);
    __ delayed()->nop();

    __ bind(isfloatordouble);
    __ andi(AT,T3, 1 << ((i*2)+1));     // Double?
    __ bne(AT, R0, isdouble);
    __ delayed()->nop();

    // Do Float Here
    __ lwc1(floatreg, Address(SP, -(Argument::n_float_register_parameters + 1 -i) * wordSize));
    __ b(next);
    __ delayed()->nop();

    // Do Double here
    __ bind(isdouble);
    __ ldc1(floatreg, Address(SP, -(Argument::n_float_register_parameters + 1 -i) * wordSize));

    __ bind(next);
  }

  __ jr(RA);
  __ delayed()->nop();
  return entry;
}


//
// Various method entries
//

address InterpreterGenerator::generate_math_entry(AbstractInterpreter::MethodKind kind) {

  // Rmethod: methodOop
  // V0: scratrch
  // esi: send 's sp, should we use Rsender @jerome
  if (!InlineIntrinsics) return NULL; // Generate a vanilla entry
  address entry_point = __ pc();

  // These don't need a safepoint check because they aren't virtually
  // callable. We won't enter these intrinsics from compiled code.
  // If in the future we added an intrinsic which was virtually callable
  // we'd have to worry about how to safepoint so that this code is used.


  // mathematical functions inlined by compiler
  // (interpreter must provide identical implementation
  // in order to avoid monotonicity bugs when switching
  // from interpreter to compiler in the middle of some
  // computation)
  //
  // stack: [ lo(arg) ] <-- sp
  //        [ hi(arg) ]
  /*
     if (Universe::is_jdk12x_version()) {
  // Note: For JDK 1.2 StrictMath doesn't exist and Math.sin/cos/sqrt are
  //       native methods. Interpreter::method_kind(...) does a check for
  //       native methods first before checking for intrinsic methods and
  //       thus will never select this entry point. Make sure it is not
  //       called accidentally since the SharedRuntime entry points will
  //       not work for JDK 1.2.
  __ should_not_reach_here();
  } else 
   */ 
  {
    // Note: For JDK 1.3 StrictMath exists and Math.sin/cos/sqrt are
    //       java methods.  Interpreter::method_kind(...) will select
    //       this entry point for the corresponding methods in JDK 1.3.
    //FIXME, @jerome
    /*if (TaggedStackInterpreter) {
      __ lw(AT, SP,3*wordSize); 
      __ push(AT);//push hi note ,SP -=wordSize 
      __ lw(AT, SP,2*wordSize); 
      __ push(AT);//push lo 
      __ lwc1(F12, SP, 2 * wordSize);
      __ lwc1(F13, SP, 3 * wordSize);
      __ sw(RA, SP, (1) * wordSize);
      __ sw(FP, SP, (0) * wordSize);
      __ addi(SP, SP, 2 * wordSize);
      __ move(FP, SP);

    }else {*/
      __ ldc1(F12, SP, 0 * wordSize);
      __ ldc1(F13, SP, 1 * wordSize);
      __ sd(RA, SP, (-1) * wordSize);
      __ sd(FP, SP, (-2) * wordSize);
      __ move(FP, SP);
      __ daddi(SP, SP, (-2) * wordSize);

//    }		
    // [ fp     ] <-- sp
    // [ ra     ]
    // [ lo     ] <-- fp
    // [ hi     ]
    /*		
		switch (kind) {
		case Interpreter::java_lang_math_sin :
		__ sincos(true, true);
		break;
		case Interpreter::java_lang_math_cos :
		__ sincos(false, true);
		break;
		case Interpreter::java_lang_math_sqrt: 
		__ sqrt_d(F0, F12);
		break;
		default                              : 
		ShouldNotReachHere();
		}
     */
    //FIXME, need consider this    
    switch (kind) {
      case Interpreter::java_lang_math_sin :
	__ trigfunc('s');
	break;
      case Interpreter::java_lang_math_cos :
	__ trigfunc('c');
	break;
      case Interpreter::java_lang_math_tan :
	__ trigfunc('t');
	break;
      case Interpreter::java_lang_math_sqrt: 
	//	__ fsqrt();
	__ sqrt_d(F0, F12);
	break;
      case Interpreter::java_lang_math_abs:
	//	__ fabs();
	__ abs_d(F0, F12);	
	break;
      case Interpreter::java_lang_math_log:
	//	__ flog();
	// Store to stack to convert 80bit precision back to 64bits
	//	__ push_fTOS();
	//	__ pop_fTOS();
	break;
      case Interpreter::java_lang_math_log10:
	//	__ flog10();
	// Store to stack to convert 80bit precision back to 64bits
	//	__ push_fTOS();
	//	__ pop_fTOS();
	break;
      case Interpreter::java_lang_math_pow:
          //__ fld_d(Address(rsp, 3*wordSize)); // second argument (one
                                             // empty stack slot)
          //__ pow_with_fallback(0);
          break;
      case Interpreter::java_lang_math_exp:
           //__ exp_with_fallback(0);
           break;

      default                              : 
	ShouldNotReachHere();
    }

    // must maintain return value in F0:F1
    __ ld(RA, FP, (-1) * wordSize);
    //FIXME	
    __ move(SP, Rsender);
    // __ move(SP, T0);	
    __ ld(FP, FP, (-2) * wordSize);
    __ jr(RA);
    __ delayed()->nop();
  }
  return entry_point;    
}


// Abstract method entry
// Attempt to execute abstract method. Throw exception
address InterpreterGenerator::generate_abstract_entry(void) {

	// Rmethod: methodOop
	// V0: receiver (unused)
	// esi: previous interpreter state (C++ interpreter) must preserve
	// Rsender : sender 's sp
	address entry_point = __ pc();

	// abstract method entry
	// throw exception
	// adjust stack to what a normal return would do

	//__ movl(esp, esi);
	__ move(SP,Rsender); //FIXME, why jvm6 add this @jerome
	__ call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::throw_AbstractMethodError));
	// the call_VM checks for exception, so we should never return here.
	__ should_not_reach_here();

	return entry_point;
}


// Empty method, generate a very fast return.

address InterpreterGenerator::generate_empty_entry(void) {

	// Rmethod: methodOop
	// V0: receiver (unused)
	// esi: previous interpreter state (C++ interpreter) must preserve
	//Rsender: sender 's sp , must set sp to this value on return , on mips ,now use T0,as it right?
	if (!UseFastEmptyMethods) return NULL;

	address entry_point = __ pc();

	Label slow_path;
	__ li(RT0, SafepointSynchronize::address_of_state());   
	__ lw(AT, RT0, 0);
	__ move(RT0, (SafepointSynchronize::_not_synchronized));   
	__ bne(AT, RT0,slow_path); 
	__ delayed()->nop(); 
	__ move(SP, Rsender);
	__ jr(RA);
	__ delayed()->nop();
	__ bind(slow_path);
	(void) generate_normal_entry(false);

	return entry_point;

}

void Deoptimization::unwind_callee_save_values(frame* f, vframeArray* vframe_array) {

  // This code is sort of the equivalent of C2IAdapter::setup_stack_frame back in
  // the days we had adapter frames. When we deoptimize a situation where a
  // compiled caller calls a compiled caller will have registers it expects
  // to survive the call to the callee. If we deoptimize the callee the only
  // way we can restore these registers is to have the oldest interpreter
  // frame that we create restore these values. That is what this routine
  // will accomplish.

  // At the moment we have modified c2 to not have any callee save registers
  // so this problem does not exist and this routine is just a place holder.

  assert(f->is_interpreted_frame(), "must be interpreted");
}
