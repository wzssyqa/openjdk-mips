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

#include "precompiled.hpp"
#include "asm/macroAssembler.hpp"
#include "asm/macroAssembler.inline.hpp"
#include "interpreter/interpreter.hpp"
#include "nativeInst_mips.hpp"
#include "oops/instanceOop.hpp"
#include "oops/method.hpp"
#include "oops/objArrayKlass.hpp"
#include "oops/oop.inline.hpp"
#include "prims/methodHandles.hpp"
#include "runtime/frame.inline.hpp"
#include "runtime/handles.inline.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "runtime/stubRoutines.hpp"
#include "runtime/thread.inline.hpp"
#include "utilities/top.hpp"
#ifdef COMPILER2
#include "opto/runtime.hpp"
#endif


// Declaration and definition of StubGenerator (no .hpp file).
// For a more detailed description of the stub routine structure
// see the comment in stubRoutines.hpp

#define __ _masm->
//#define TIMES_OOP (UseCompressedOops ? Address::times_4 : Address::times_8)
//#define a__ ((Assembler*)_masm)->

//#ifdef PRODUCT
//#define BLOCK_COMMENT(str) /* nothing */
//#else
//#define BLOCK_COMMENT(str) __ block_comment(str)
//#endif

//#define BIND(label) bind(label); BLOCK_COMMENT(#label ":")
const int MXCSR_MASK = 0xFFC0;  // Mask out any pending exceptions

// Stub Code definitions

static address handle_unsafe_access() {
  JavaThread* thread = JavaThread::current();
  address pc = thread->saved_exception_pc();
  // pc is the instruction which we must emulate
  // doing a no-op is fine:  return garbage from the load
  // therefore, compute npc
  //address npc = Assembler::locate_next_instruction(pc);
	address npc = (address)((unsigned long)pc + sizeof(unsigned long));

  // request an async exception
  thread->set_pending_unsafe_access_error();

  // return address of next instruction to execute
  return npc;
}

class StubGenerator: public StubCodeGenerator {
 private:

  // ABI mips n64
  // This fig is not MIPS ABI. It is call Java from C ABI.
  // Call stubs are used to call Java from C
  //
  //    [ return_from_Java     ]
  //    [ argument word n-1    ] <--- sp
  //      ...
  //    [ argument word 0      ]
  //      ...
  //-10 [ S6     	       ]
  // -9 [ S5		       ] 
  // -8 [ S4		       ]
  // -7 [ S3                   ]
  // -6 [ S0  		       ]
  // -5 [ TSR(S2)	       ]
  // -4 [ LVP(S7)              ]
  // -3 [ BCP(S1)              ]
  // -2 [ saved fp             ] <--- fp_after_call
  // -1 [ return address       ] 
  //  0 [ ptr. to call wrapper ] <--- a0 (old sp -->)fp
  //  1 [ result               ] <--- a1
  //  2 [ result_type          ] <--- a2
  //  3 [ method               ] <--- a3
  //  4 [ entry_point          ] <--- a4
  //  5 [ parameters           ] <--- a5
  //  6 [ parameter_size       ] <--- a6
  //  7 [ thread               ] <--- a7

  //
  // _LP64: n64 does not save paras in sp.
  //
  //    [ return_from_Java     ]
  //    [ argument word n-1    ] <--- sp
  //      ...
  //    [ argument word 0      ]
  //      ...
  //-14 [ thread               ]
  //-13 [ result_type          ] <--- a2
  //-12 [ result               ] <--- a1
  //-11 [ ptr. to call wrapper ] <--- a0
  //-10 [ S6     	       ]
  // -9 [ S5		       ] 
  // -8 [ S4		       ]
  // -7 [ S3                   ]
  // -6 [ S0  		       ]
  // -5 [ TSR(S2)	       ]
  // -4 [ LVP(S7)              ]
  // -3 [ BCP(S1)              ]
  // -2 [ saved fp             ] <--- fp_after_call
  // -1 [ return address       ] 
  //  0 [        	       ] <--- old sp
  /*
   * 2014/01/16 Fu: Find a right place in the call_stub for GP.
   * GP will point to the starting point of Interpreter::dispatch_table(itos). 
   * It should be saved/restored before/after Java calls. 
   *
   */
   enum call_stub_layout {
     RA_off		  = -1,
     FP_off		  = -2,
     BCP_off		  = -3,
     LVP_off		  = -4,
     TSR_off		  = -5,
     S1_off		  = -6,
     S3_off		  = -7,
     S4_off		  = -8,
     S5_off		  = -9,
     S6_off		  = -10,
     result_off		  = -11,
     result_type_off	  = -12,
     thread_off		  = -13,
     total_off		  = thread_off - 3,
     GP_off               = -16,
   };

  address generate_call_stub(address& return_address) {

    StubCodeMark mark(this, "StubRoutines", "call_stub");
    address start = __ pc();

    // same as in generate_catch_exception()!

    // stub code
    // save ra and fp
    __ sd(RA, SP, RA_off * wordSize);
    __ sd(FP, SP, FP_off * wordSize);
    __ sd(BCP, SP, BCP_off * wordSize);
    __ sd(LVP, SP, LVP_off * wordSize);
    __ sd(GP, SP, GP_off * wordSize);
    __ sd(TSR, SP, TSR_off * wordSize);
    __ sd(S1, SP, S1_off * wordSize);
    __ sd(S3, SP, S3_off * wordSize);
    __ sd(S4, SP, S4_off * wordSize);
    __ sd(S5, SP, S5_off * wordSize);
    __ sd(S6, SP, S6_off * wordSize);


    __ li48(GP, (long)Interpreter::dispatch_table(itos));
    
    // I think 14 is the max gap between argument and callee saved register
    __ daddi(FP, SP, (-2) * wordSize);
    __ daddi(SP, SP, total_off * wordSize);
//FIXME, aoqi. find a suitable place to save A1 & A2.
    /*
    __ sd(A0, FP, frame::entry_frame_call_wrapper_offset * wordSize);
    __ sd(A1, FP, 3 * wordSize);
    __ sd(A2, FP, 4 * wordSize);
    __ sd(A3, FP, 5 * wordSize);
    __ sd(A4, FP, 6 * wordSize);
    __ sd(A5, FP, 7 * wordSize);
    __ sd(A6, FP, 8 * wordSize);
    __ sd(A7, FP, 9 * wordSize);
    */
    __ sd(A0, FP, frame::entry_frame_call_wrapper_offset * wordSize);
    __ sd(A1, FP, result_off * wordSize);
    __ sd(A2, FP, result_type_off * wordSize);
    __ sd(A7, FP, thread_off * wordSize);

#ifdef OPT_THREAD
    //__ get_thread(TREG);
    __ move(TREG, A7);

    //__ ld(TREG, FP, thread_off * wordSize);
#endif
    //add for compressedoops
    __ reinit_heapbase();

#ifdef ASSERT
    // make sure we have no pending exceptions
    { 
      Label L;
    	__ ld(AT, A7, in_bytes(Thread::pending_exception_offset()));
    	__ beq(AT, R0, L); 
    	__ delayed()->nop();
    	/* FIXME: I do not know how to realize stop in mips arch, do it in the future */
    	__ stop("StubRoutines::call_stub: entered with pending exception");
    	__ bind(L);
    }
#endif

    // pass parameters if any
    // A5: parameter
    // A6: parameter_size
    // T0: parameter_size_tmp(--)
    // T2: offset(++)
    // T3: tmp
    Label parameters_done;
    // judge if the parameter_size equals 0
    __ beq(A6, R0, parameters_done);
    __ delayed()->nop();
    __ dsll(AT, A6, Interpreter::logStackElementSize);
    __ dsub(SP, SP, AT); 
    __ move(AT, -StackAlignmentInBytes); 
    __ andr(SP, SP , AT); 
    // Copy Java parameters in reverse order (receiver last)
    // Note that the argument order is inverted in the process
    // source is edx[ecx: N-1..0]
    // dest   is esp[ebx: 0..N-1]
    Label loop;
    __ move(T0, A6);
    __ move(T2, R0);
    __ bind(loop);
    
    // get parameter
    __ dsll(T3, T0, LogBytesPerWord);   
    __ dadd(T3, T3, A5);	    
    __ ld(AT, T3,  -wordSize);
    __ dsll(T3, T2, LogBytesPerWord); 
    __ dadd(T3, T3, SP); 
    __ sd(AT, T3, Interpreter::expr_offset_in_bytes(0));
    __ daddi(T2, T2, 1); 
    __ daddi(T0, T0, -1); 
    __ bne(T0, R0, loop);
    __ delayed()->nop();
    // advance to next parameter
    
    // call Java function
    __ bind(parameters_done);
    
    // receiver in V0, methodOop in Rmethod
    
    __ move(Rmethod, A3);
    __ move(Rsender, SP);             //set sender sp
    __ jalr(A4);
    __ delayed()->nop();
    return_address = __ pc();
    
    Label common_return;
    __ bind(common_return);
    
    // store result depending on type
    // (everything that is not T_LONG, T_FLOAT or T_DOUBLE is treated as T_INT)
    __ ld(T0, FP, result_off * wordSize); 	// result --> T0
    Label is_long, is_float, is_double, exit;
    __ ld(T2, FP, result_type_off * wordSize);	// result_type --> T2
    __ daddi(T3, T2, (-1) * T_LONG);
    __ beq(T3, R0, is_long);
    __ delayed()->daddi(T3, T2, (-1) * T_FLOAT);
    __ beq(T3, R0, is_float);
    __ delayed()->daddi(T3, T2, (-1) * T_DOUBLE);
    __ beq(T3, R0, is_double);
    __ delayed()->nop();
    
    // handle T_INT case
    __ sd(V0, T0, 0 * wordSize);
    __ bind(exit);
    
    // restore 
    __ daddi(SP, FP, 2 * wordSize );
    __ ld(RA, SP, RA_off * wordSize);
    __ ld(FP, SP, FP_off * wordSize);
    __ ld(BCP, SP, BCP_off * wordSize);
    __ ld(LVP, SP, LVP_off * wordSize);
    __ ld(GP, SP, GP_off * wordSize);
    __ ld(TSR, SP, TSR_off * wordSize);

    __ ld(S1, SP, S1_off * wordSize);
    __ ld(S3, SP, S3_off * wordSize);
    __ ld(S4, SP, S4_off * wordSize);
    __ ld(S5, SP, S5_off * wordSize);
    __ ld(S6, SP, S6_off * wordSize);

    // return
    __ jr(RA);
    __ delayed()->nop();
    
    // handle return types different from T_INT
    __ bind(is_long);
    __ sd(V0, T0, 0 * wordSize);
    //__ sd(V1, T0, 1 * wordSize);
    //__ sd(R0, T0, 1 * wordSize);
    __ b(exit);
    __ delayed()->nop();
    
    __ bind(is_float);
    __ swc1(F0, T0, 0 * wordSize);
    __ b(exit);
    __ delayed()->nop();
    
    __ bind(is_double);
    __ sdc1(F0, T0, 0 * wordSize);
    //__ sdc1(F1, T0, 1 * wordSize);
    //__ sd(R0, T0, 1 * wordSize);
    __ b(exit);
    __ delayed()->nop();
    //FIXME, 1.6 mips version add operation of fpu here
    StubRoutines::gs2::set_call_stub_compiled_return(__ pc());
    __ b(common_return);
    __ delayed()->nop(); 
    return start;
  }

  // Return point for a Java call if there's an exception thrown in
  // Java code.  The exception is caught and transformed into a
  // pending exception stored in JavaThread that can be tested from
  // within the VM.
  //
  // Note: Usually the parameters are removed by the callee. In case
  // of an exception crossing an activation frame boundary, that is
  // not the case if the callee is compiled code => need to setup the
  // rsp.
  //
  // rax: exception oop

  address generate_catch_exception() {
    StubCodeMark mark(this, "StubRoutines", "catch_exception");
    address start = __ pc();

    Register thread = TREG;

    // get thread directly
#ifndef OPT_THREAD
    __ ld(thread, FP, thread_off * wordSize);
#endif

#ifdef ASSERT
    // verify that threads correspond
    { Label L;
      __ get_thread(T8);
      __ beq(T8, thread, L);
      __ delayed()->nop();
      __ stop("StubRoutines::catch_exception: threads must correspond");
      __ bind(L);
    }
#endif
    // set pending exception
    __ verify_oop(V0);
    __ sd(V0, thread, in_bytes(Thread::pending_exception_offset()));
    __ li(AT, (long)__FILE__);
    __ sd(AT, thread, in_bytes(Thread::exception_file_offset   ()));
    __ li(AT, (long)__LINE__);
    __ sd(AT, thread, in_bytes(Thread::exception_line_offset   ()));

    // complete return to VM
    assert(StubRoutines::_call_stub_return_address != NULL, "_call_stub_return_address must have been generated before");
    __ jmp(StubRoutines::_call_stub_return_address, relocInfo::none);
    __ delayed()->nop();

    return start;
  }

  // Continuation point for runtime calls returning with a pending
  // exception.  The pending exception check happened in the runtime
  // or native call stub.  The pending exception in Thread is
  // converted into a Java-level exception.
  //
  // Contract with Java-level exception handlers:
  // rax: exception
  // rdx: throwing pc
  //
  // NOTE: At entry of this stub, exception-pc must be on stack !!

  address generate_forward_exception() {
    StubCodeMark mark(this, "StubRoutines", "forward exception");
    //Register thread = TREG;
    Register thread = TREG;
    address start = __ pc();

    // Upon entry, the sp points to the return address returning into Java
    // (interpreted or compiled) code; i.e., the return address becomes the
    // throwing pc.
    //
    // Arguments pushed before the runtime call are still on the stack but
    // the exception handler will reset the stack pointer -> ignore them.
    // A potential result in registers can be ignored as well.

#ifdef ASSERT
    // make sure this code is only executed if there is a pending exception
#ifndef OPT_THREAD
    __ get_thread(thread);
#endif
    { Label L;
      __ ld(AT, thread, in_bytes(Thread::pending_exception_offset()));
      __ bne(AT, R0, L);
      __ delayed()->nop();
      __ stop("StubRoutines::forward exception: no pending exception (1)");
      __ bind(L);
    }
#endif

    // compute exception handler into T9
    __ ld(A1, SP, 0);
    __ call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::exception_handler_for_return_address), thread, A1);
    __ move(T9, V0);
    __ pop(V1);

#ifndef OPT_THREAD
    __ get_thread(thread);
#endif
    __ ld(V0, thread, in_bytes(Thread::pending_exception_offset()));
    __ sd(R0, thread, in_bytes(Thread::pending_exception_offset()));

#ifdef ASSERT
    // make sure exception is set
    { Label L;
      __ bne(V0, R0, L);
      __ delayed()->nop();
      __ stop("StubRoutines::forward exception: no pending exception (2)");
      __ bind(L);
    }
#endif

    // continue at exception handler (return address removed)
    // V0: exception
    // T9: exception handler
    // V1: throwing pc
    __ verify_oop(V0);
    __ jr(T9);
    __ delayed()->nop();

    return start;
  }

  // Support for intptr_t get_previous_fp()
  //
  // This routine is used to find the previous frame pointer for the
  // caller (current_frame_guess). This is used as part of debugging
  // ps() is seemingly lost trying to find frames.
  // This code assumes that caller current_frame_guess) has a frame.
  address generate_get_previous_fp() {
    StubCodeMark mark(this, "StubRoutines", "get_previous_fp");
    const Address old_fp       (FP,  0);
    const Address older_fp       (V0,  0);
    address start = __ pc();
    __ enter();    
    __ lw(V0, old_fp); // callers fp
    __ lw(V0, older_fp); // the frame for ps()
    __ leave();
    __ jr(RA);
    __ delayed()->nop();
    return start;
  }
  // The following routine generates a subroutine to throw an
  // asynchronous UnknownError when an unsafe access gets a fault that
  // could not be reasonably prevented by the programmer.  (Example:
  // SIGBUS/OBJERR.)
  address generate_handler_for_unsafe_access() {
		StubCodeMark mark(this, "StubRoutines", "handler_for_unsafe_access");
		address start = __ pc();
		__ pushad();                      // push registers
		//  Address next_pc(esp, RegisterImpl::number_of_registers * BytesPerWord);
		__ call(CAST_FROM_FN_PTR(address, handle_unsafe_access), relocInfo::runtime_call_type);
		__ delayed()->nop(); 
		__ sw(V0, SP, RegisterImpl::number_of_registers * BytesPerWord); 
		__ popad();
		__ jr(RA);
		__ delayed()->nop();  
		return start;
  }

  // Non-destructive plausibility checks for oops
  //
  // Arguments:
  //    all args on stack!
  //
  // Stack after saving c_rarg3:
  //    [tos + 0]: saved c_rarg3
  //    [tos + 1]: saved c_rarg2
  //    [tos + 2]: saved r12 (several TemplateTable methods use it)
  //    [tos + 3]: saved flags
  //    [tos + 4]: return address
  //  * [tos + 5]: error message (char*)
  //  * [tos + 6]: object to verify (oop)
  //  * [tos + 7]: saved rax - saved by caller and bashed
  //  * = popped on exit
  address generate_verify_oop() {
	  StubCodeMark mark(this, "StubRoutines", "verify_oop");
	  address start = __ pc();
	  __ reinit_heapbase();
	  __ verify_oop_subroutine(); 
    address end = __ pc();
	  return start;
  }

  //
  //  Generate overlap test for array copy stubs
  //
  //  Input:
  //     A0    -  array1
  //     A1    -  array2
  //     A2    -  element count
  //
  //  Note: this code can only use %eax, %ecx, and %edx
  //

 // use T9 as temp 
  void array_overlap_test(address no_overlap_target, int log2_elem_size) {
    int elem_size = 1 << log2_elem_size;
    Address::ScaleFactor sf = Address::times_1;

    switch (log2_elem_size) {
      case 0: sf = Address::times_1; break;
      case 1: sf = Address::times_2; break;
      case 2: sf = Address::times_4; break;
      case 3: sf = Address::times_8; break;
    }

    __ dsll(AT, A2, sf);
    __ dadd(AT, AT, A0); 
    __ lea(T9, Address(AT, -elem_size)); 
    __ dsub(AT, A1, A0); 
    __ blez(AT, no_overlap_target); 
    __ delayed()->nop(); 
    __ dsub(AT, A1, T9); 
    __ bgtz(AT, no_overlap_target); 
    __ delayed()->nop(); 

    // 2016/05/10 aoqi: If A0 = 0xf... and A1 = 0x0..., than goto no_overlap_target 
    Label L;
    __ bgez(A0, L);
    __ delayed()->nop(); 
    __ bgtz(A1, no_overlap_target);
    __ delayed()->nop(); 
    __ bind(L);

  }

  //
  //  Generate store check for array
  //
  //  Input:
  //     %edi    -  starting address
  //     %ecx    -  element count
  //
  //  The 2 input registers are overwritten
  //
 
  //
  //  Generate store check for array
  //
  //  Input:
  //     T0    -  starting address(edi)
  //     T1    -  element count  (ecx)
  //
  //  The 2 input registers are overwritten
  //
 
#define TIMES_OOP (UseCompressedOops ? Address::times_4 : Address::times_8)

	void array_store_check() {
		BarrierSet* bs = Universe::heap()->barrier_set();
		assert(bs->kind() == BarrierSet::CardTableModRef, "Wrong barrier set kind");
		CardTableModRefBS* ct = (CardTableModRefBS*)bs;
		assert(sizeof(*ct->byte_map_base) == sizeof(jbyte), "adjust this code");
		Label l_0;

		__ dsll(AT, T1, TIMES_OOP);
		__ dadd(AT, T0, AT); 
		__ daddiu(T1, AT, - BytesPerHeapOop);

		__ shr(T0, CardTableModRefBS::card_shift); 
		__ shr(T1, CardTableModRefBS::card_shift);

		__ dsub(T1, T1, T0);   // end --> cards count
		__ bind(l_0);

		__ li48(AT, (long)ct->byte_map_base); 
		__ dadd(AT, AT, T0); 
		__ dadd(AT, AT, T1); 
		__ sb(R0, AT, 0);
		//__ daddi(T1, T1, -4);  
		__ daddi(T1, T1, - 1);
		__ bgez(T1, l_0);
		__ delayed()->nop(); 
	}

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-, 2-, or 1-byte boundaries,
  // we let the hardware handle it.  The one to eight bytes within words,
  // dwords or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  // Side Effects:
  //   disjoint_byte_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_byte_copy().
  //
	address generate_disjoint_byte_copy(bool aligned, const char *name) {
	  StubCodeMark mark(this, "StubRoutines", name);
	  __ align(CodeEntryAlignment);
	  address start = __ pc();
	  Label l_0, l_1, l_2, l_3, l_4, l_5, l_6;

	  __ push(T3);
	  __ push(T0);
	  __ push(T1);
	  __ push(T8);
	  __ move(T3, A0); 
	  __ move(T0, A1);
	  __ move(T1, A2);  
	  __ move(T8, T1);             // original count in T1
	  __ daddi(AT, T1, -3); 
	  __ blez(AT, l_4);  
	  __ delayed()->nop();	
	  if (!aligned) {
          //TODO: copy 8 bytes at one time
	    // 2016/5/8 Jin: only when src and dest has the same alignment can we do lw/sw */
	    __ andi(AT, T3, 3); 
	    __ andi(T9, T0, 3); 
	    __ bne(AT, T9, l_5); 
	    __ delayed()->nop();	
	  
	    // align source address at dword address boundary
	    __ move(T1, 4); 
	    __ sub(T1, T1, T3); 
	    __ andi(T1, T1, 3); 
	    __ beq(T1, R0, l_1); 
	    __ delayed()->nop();	
	    __ sub(T8,T8,T1); 
	    __ bind(l_0);
	    __ lb(AT, T3, 0); 
	    __ sb(AT, T0, 0); 
	    __ addi(T3, T3, 1); 
	    __ addi(T0, T0, 1); 
	    __ addi(T1 ,T1, -1);  
	    __ bne(T1, R0, l_0); 
	    __ delayed()->nop(); 
	    __ bind(l_1);
	    __ move(T1, T8); 
	  }
	  __ shr(T1, 2); 
	  __ beq(T1, R0, l_4);     // no dwords to move
	  __ delayed()->nop(); 
	  // copy aligned dwords
	  __ bind(l_2);
	  __ align(16);
	  __ bind(l_3);
	  __ lw(AT, T3, 0);   
	  __ sw(AT, T0, 0 ); 
	  __ addi(T3, T3, 4); 
	  __ addi(T0, T0, 4); 
	  __ addi(T1, T1, -1); 
	  __ bne(T1, R0, l_3); 
	  __ delayed()->nop(); 
	  __ bind(l_4);
	  __ move(T1, T8); 
	  __ andi(T1, T1, 3); 
	  __ beq(T1, R0, l_6);  
	  __ delayed()->nop(); 
	  // copy suffix
	  __ bind(l_5);
	  __ lb(AT, T3, 0); 
	  __ sb(AT, T0, 0); 
	  __ addi(T3, T3, 1);  
	  __ addi(T0, T0, 1);  
	  __ addi(T1, T1, -1); 
	  __ bne(T1, R0, l_5 ); 
	  __ delayed()->nop(); 
	  __ bind(l_6);
	  __ pop(T8); 
	  __ pop(T1); 
	  __ pop(T0); 
	  __ pop(T3); 
	  __ jr(RA); 
	  __ delayed()->nop(); 
	  return start;
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   A0   - source array address
  //   A1   - destination array address
  //   A2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-, 2-, or 1-byte boundaries,
  // we let the hardware handle it.  The one to eight bytes within words,
  // dwords or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  address generate_conjoint_byte_copy(bool aligned, const char *name) {
    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();

    Label l_copy_4_bytes_loop, l_copy_suffix, l_copy_suffix_loop, l_exit;
    Label l_copy_byte, l_from_unaligned, l_unaligned, l_4_bytes_aligned;

    address nooverlap_target = aligned ?
	    StubRoutines::arrayof_jbyte_disjoint_arraycopy() :
	    StubRoutines::jbyte_disjoint_arraycopy();

    array_overlap_test(nooverlap_target, 0);

    const Register from      = A0;   // source array address
    const Register to        = A1;   // destination array address
    const Register count     = A2;   // elements count
    const Register end_from  = T3;   // source array end address
    const Register end_to    = T0;   // destination array end address
    const Register end_count = T1;   // destination array end address

    __ push(end_from);	
    __ push(end_to);	
    __ push(end_count);	
    __ push(T8);	

    // copy from high to low
    __ move(end_count, count);  
    __ dadd(end_from, from, end_count);  
    __ dadd(end_to, to, end_count);  

    // 2016/05/08 aoqi: If end_from and end_to has differante alignment, unaligned copy is performed.
    __ andi(AT, end_from, 3); 
    __ andi(T8, end_to, 3); 
    __ bne(AT, T8, l_copy_byte); 
    __ delayed()->nop();	

    // First deal with the unaligned data at the top.
    __ bind(l_unaligned);
    __ beq(end_count, R0, l_exit); 
    __ delayed()->nop(); 

    __ andi(AT, end_from, 3);    
    __ bne(AT, R0, l_from_unaligned); 
    __ delayed()->nop(); 

    __ andi(AT, end_to, 3);    
    __ beq(AT, R0, l_4_bytes_aligned); 
    __ delayed()->nop(); 

    __ bind(l_from_unaligned);
    __ lb(AT, end_from, -1);   
    __ sb(AT, end_to, -1); 
    __ daddi(end_from, end_from, -1); 
    __ daddi(end_to, end_to, -1); 
    __ daddi(end_count, end_count, -1); 
    __ b(l_unaligned); 
    __ delayed()->nop(); 

    // now end_to, end_from point to 4-byte aligned high-ends
    //     end_count contains byte count that is not copied.
    // copy 4 bytes at a time
    __ bind(l_4_bytes_aligned);

    __ move(T8, end_count); 
    __ daddi(AT, end_count, -3); 
    __ blez(AT, l_copy_suffix); 
    __ delayed()->nop();	

    //__ andi(T8, T8, 3); 
    __ lea(end_from, Address(end_from, -4));
    __ lea(end_to, Address(end_to, -4));

    __ dsrl(end_count, end_count, 2); 
    __ align(16);
    __ bind(l_copy_4_bytes_loop); //l_copy_4_bytes
    __ lw(AT, end_from, 0);   
    __ sw(AT, end_to, 0); 
    __ addi(end_from, end_from, -4);    
    __ addi(end_to, end_to, -4);    
    __ addi(end_count, end_count, -1);  
    __ bne(end_count, R0, l_copy_4_bytes_loop); 
    __ delayed()->nop(); 

    __ b(l_copy_suffix);  
    __ delayed()->nop(); 
    // copy dwords aligned or not with repeat move
    // l_copy_suffix
    // copy suffix (0-3 bytes)
    __ bind(l_copy_suffix); 
    __ andi(T8, T8, 3); 
    __ beq(T8, R0, l_exit); 
    __ delayed()->nop(); 
    __ addi(end_from, end_from, 3); 
    __ addi(end_to, end_to, 3); 
    __ bind(l_copy_suffix_loop);
    __ lb(AT, end_from, 0);  
    __ sb(AT, end_to, 0); 
    __ addi(end_from, end_from, -1);  
    __ addi(end_to, end_to, -1);  
    __ addi(T8, T8, -1); 
    __ bne(T8, R0, l_copy_suffix_loop); 
    __ delayed()->nop(); 

    __ bind(l_copy_byte);
    __ beq(end_count, R0, l_exit); 
    __ delayed()->nop(); 
    __ lb(AT, end_from, -1);   
    __ sb(AT, end_to, -1); 
    __ daddi(end_from, end_from, -1); 
    __ daddi(end_to, end_to, -1); 
    __ daddi(end_count, end_count, -1); 
    __ b(l_copy_byte); 
    __ delayed()->nop(); 

    __ bind(l_exit);
    __ pop(T8);	
    __ pop(end_count);	
    __ pop(end_to);	
    __ pop(end_from);	
    __ jr(RA); 
    __ delayed()->nop(); 
    return start;
  }

  // Generate stub for disjoint short copy.  If "aligned" is true, the
  // "from" and "to" addresses are assumed to be heapword aligned.
  //
  // Arguments for generated stub:
  //      from:  A0
  //      to:    A1
  //  elm.count: A2 treated as signed
  //  one element: 2 bytes
  //
  // Strategy for aligned==true:
  //
  //  If length <= 9:
  //     1. copy 1 elements at a time (l_5)
  //
  //  If length > 9:
  //     1. copy 4 elements at a time until less than 4 elements are left (l_7)
  //     2. copy 2 elements at a time until less than 2 elements are left (l_6)
  //     3. copy last element if one was left in step 2. (l_1)
  //
  //
  // Strategy for aligned==false:
  //
  //  If length <= 9: same as aligned==true case
  //
  //  If length > 9:
  //     1. continue with step 7. if the alignment of from and to mod 4
  //        is different.
  //     2. align from and to to 4 bytes by copying 1 element if necessary
  //     3. at l_2 from and to are 4 byte aligned; continue with
  //        6. if they cannot be aligned to 8 bytes because they have
  //        got different alignment mod 8.
  //     4. at this point we know that both, from and to, have the same
  //        alignment mod 8, now copy one element if necessary to get
  //        8 byte alignment of from and to.
  //     5. copy 4 elements at a time until less than 4 elements are
  //        left; depending on step 3. all load/stores are aligned.
  //     6. copy 2 elements at a time until less than 2 elements are
  //        left. (l_6)
  //     7. copy 1 element at a time. (l_5)
  //     8. copy last element if one was left in step 6. (l_1)
  //
  //  TODO:
  //
  //  1. use loongson 128-bit load/store
  //  2. use loop unrolling optimization when len is big enough, for example if len > 0x2000:
  //    __ bind(l_x);
  //    __ ld(AT, tmp1, 0);
  //    __ ld(tmp, tmp1, 8);
  //    __ sd(AT, tmp2, 0);
  //    __ sd(tmp, tmp2, 8);
  //    __ ld(AT, tmp1, 16);
  //    __ ld(tmp, tmp1, 24);
  //    __ sd(AT, tmp2, 16);
  //    __ sd(tmp, tmp2, 24);
  //    __ daddi(tmp1, tmp1, 32);
  //    __ daddi(tmp2, tmp2, 32);
  //    __ daddi(tmp3, tmp3, -16);
  //    __ daddi(AT, tmp3, -16);
  //    __ bgez(AT, l_x);
  //    __ delayed()->nop();
  //
  address generate_disjoint_short_copy(bool aligned, const char * name) {
    StubCodeMark mark(this, "StubRoutines", name);
    __ align(CodeEntryAlignment);

    Register tmp1 = T0;
    Register tmp2 = T1;
    Register tmp3 = T3;

    address start = __ pc();

    __ push(tmp1);
    __ push(tmp2);
    __ push(tmp3);
    __ move(tmp1, A0);
    __ move(tmp2, A1);
    __ move(tmp3, A2);

    Label l_1, l_2, l_3, l_4, l_5, l_6, l_7, l_8;
    Label l_debug;
    // don't try anything fancy if arrays don't have many elements
    __ daddi(AT, tmp3, -9);
    __ blez(AT, l_1);
    __ delayed()->nop();

    if (!aligned) {
      __ xorr(AT, A0, A1);
      __ andi(AT, AT, 1);
      __ bne(AT, R0, l_debug); // if arrays don't have the same alignment mod 2, can this happen?
      __ delayed()->nop();

      __ xorr(AT, A0, A1);
      __ andi(AT, AT, 3);
      __ bne(AT, R0, l_1); // if arrays don't have the same alignment mod 4, do 1 element copy
      __ delayed()->nop();

      // At this point it is guaranteed that both, from and to have the same alignment mod 4.

      // Copy 1 element if necessary to align to 4 bytes.
      __ andi(AT, A0, 3);
      __ beq(AT, R0, l_2);
      __ delayed()->nop();

      __ lhu(AT, tmp1, 0);
      __ daddi(tmp1, tmp1, 2);
      __ sh(AT, tmp2, 0);
      __ daddi(tmp2, tmp2, 2);
      __ daddi(tmp3, tmp3, -1);
      __ bind(l_2);

      // At this point the positions of both, from and to, are at least 4 byte aligned.

      // Copy 4 elements at a time.
      // Align to 8 bytes, but only if both, from and to, have same alignment mod 8.
      __ xorr(AT, tmp1, tmp2);
      __ andi(AT, AT, 7);
      __ bne(AT, R0, l_6); // not same alignment mod 8 -> copy 2, either from or to will be unaligned
      __ delayed()->nop();

      // Copy a 2-element word if necessary to align to 8 bytes.
      __ andi(AT, tmp1, 7);
      __ beq(AT, R0, l_7);
      __ delayed()->nop();

      __ lw(AT, tmp1, 0);
      __ daddi(tmp3, tmp3, -2);
      __ sw(AT, tmp2, 0);
      { // FasterArrayCopy
        __ daddi(tmp1, tmp1, 4);
        __ daddi(tmp2, tmp2, 4);
      }
    }

    __ bind(l_7);

    // Copy 4 elements at a time; either the loads or the stores can
    // be unaligned if aligned == false.

    { // FasterArrayCopy
      __ daddi(AT, tmp3, -15);
      __ blez(AT, l_6); // copy 2 at a time if less than 16 elements remain
      __ delayed()->nop();

      __ bind(l_8);
      // For Loongson, there is 128-bit memory access. TODO
      __ ld(AT, tmp1, 0);
      __ sd(AT, tmp2, 0);
      __ daddi(tmp1, tmp1, 8);
      __ daddi(tmp2, tmp2, 8);
      __ daddi(tmp3, tmp3, -4);
      __ daddi(AT, tmp3, -4);
      __ bgez(AT, l_8);
      __ delayed()->nop();
    }
    __ bind(l_6);

    // copy 2 element at a time
    { // FasterArrayCopy
      __ daddi(AT, tmp3, -1);
      __ blez(AT, l_1);
      __ delayed()->nop();

      __ bind(l_3);
      __ lw(AT, tmp1, 0);
      __ sw(AT, tmp2, 0);
      __ daddi(tmp1, tmp1, 4);
      __ daddi(tmp2, tmp2, 4);
      __ daddi(tmp3, tmp3, -2);
      __ daddi(AT, tmp3, -2);
      __ bgez(AT, l_3);
      __ delayed()->nop();

    }

    // do single element copy (8 bit), can this happen?
    __ bind(l_1);
    __ beq(R0, tmp3, l_4);
    __ delayed()->nop();

    { // FasterArrayCopy

      __ bind(l_5);
      __ lhu(AT, tmp1, 0);
      __ daddi(tmp3, tmp3, -1);
      __ sh(AT, tmp2, 0);
      __ daddi(tmp1, tmp1, 2);
      __ daddi(tmp2, tmp2, 2);
      __ daddi(AT, tmp3, -1);
      __ bgez(AT, l_5);
      __ delayed()->nop();
    }
    __ bind(l_4);
    __ pop(tmp3);
    __ pop(tmp2);
    __ pop(tmp1);

    __ jr(RA);
    __ delayed()->nop();

    __ bind(l_debug);
    __ stop("generate_disjoint_short_copy should not reach here");
    return start;
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4- or 2-byte boundaries, we
  // let the hardware handle it.  The two or four words within dwords
  // or qwords that span cache line boundaries will still be loaded
  // and stored atomically.
  //
  address generate_conjoint_short_copy(bool aligned, const char *name) {
		Label l_1, l_2, l_3, l_4, l_5;
		StubCodeMark mark(this, "StubRoutines", name);
		__ align(CodeEntryAlignment);
		address start = __ pc();
		address nooverlap_target = aligned ?
						StubRoutines::arrayof_jshort_disjoint_arraycopy() :
						StubRoutines::jshort_disjoint_arraycopy();

		array_overlap_test(nooverlap_target, 1);

		__ push(T3);	
		__ push(T0);	
		__ push(T1);	
		__ push(T8);	

		/*
			 __ pushl(esi);
			 __ movl(ecx, Address(esp, 4+12));      // count
			 __ pushl(edi);
			 __ movl(esi, Address(esp, 8+ 4));      // from
			 __ movl(edi, Address(esp, 8+ 8));      // to
		 */ 
		__ move(T1, A2);  
		__ move(T3, A0); 
		__ move(T0, A1);


		// copy dwords from high to low
		// __ leal(esi, Address(esi, ecx, Address::times_2, -4)); // from + count*2 - 4
		__ sll(AT, T1, Address::times_2); 
		__ add(AT, T3, AT); 
		__ lea(T3, Address( AT, -4)); 
		//__ std();
		//__ leal(edi, Address(edi, ecx, Address::times_2, -4)); // to + count*2 - 4
		__ sll(AT,T1 , Address::times_2); 
		__ add(AT, T0, AT); 
		__ lea(T0, Address( AT, -4)); 
		//  __ movl(eax, ecx);
		__ move(T8, T1); 
		__ bind(l_1);
		//   __ sarl(ecx, 1);              // dword count
		__ sra(T1,T1, 1); 
		//__ jcc(Assembler::equal, l_4);                   // no dwords to move
		__ beq(T1, R0, l_4);  
		__ delayed()->nop(); 
		/*    __ cmpl(ecx, 32);
					__ jcc(Assembler::above, l_3);                   // > 32 dwords
		// copy dwords with loop
		__ subl(edi, esi);
		 */     __ align(16);
		__ bind(l_2);
		//__ movl(edx, Address(esi));
		__ lw(AT, T3, 0);   
		//__ movl(Address(edi, esi, Address::times_1), edx);
		__ sw(AT, T0, 0); 
		//__ subl(esi, 4);
		__ addi(T3, T3, -4); 
		__ addi(T0, T0, -4); 
		//__ decl(ecx);
		__ addi(T1, T1, -1); 
		//  __ jcc(Assembler::notEqual, l_2);
		__ bne(T1, R0, l_2); 
		__ delayed()->nop(); 
		//  __ addl(edi, esi);
		// __ jmp(l_4);
		__ b(l_4);
		__ delayed()->nop();
		// copy dwords with repeat move
		__ bind(l_3);
		//   __ rep_movl();
		__ bind(l_4);
		//  __ andl(eax, 1);              // suffix count
		__ andi(T8, T8, 1);              // suffix count
		//__ jcc(Assembler::equal, l_5);                   // no suffix
		__ beq(T8, R0, l_5 );  
		__ delayed()->nop(); 
		// copy suffix
		//   __ movw(edx, Address(esi, 2));
		__ lh(AT, T3, 2); 
		//  __ movw(Address(edi, 2), edx);
		__ sh(AT, T0, 2); 
		__ bind(l_5);
		//    __ cld();
		//    __ popl(edi);
		//    __ popl(esi);
		//   __ ret(0);
		__ pop(T8);	
		__ pop(T1);	
		__ pop(T0);	
		__ pop(T3);	
		__ jr(RA); 
		__ delayed()->nop();   
		return start;
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  // Side Effects:
  //   disjoint_int_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_int_oop_copy().
  //
  address generate_disjoint_int_oop_copy(bool aligned, bool is_oop, const char *name) {
		Label l_2, l_3, l_4, l_stchk;
		StubCodeMark mark(this, "StubRoutines", name);
		__ align(CodeEntryAlignment);
		address start = __ pc();
		/*
			 __ pushl(esi);
			 __ movl(ecx, Address(esp, 4+12));      // count
			 __ pushl(edi);
			 __ movl(esi, Address(esp, 8+ 4));      // from
			 __ movl(edi, Address(esp, 8+ 8));      // to
		 */
		__ push(T3);	
		__ push(T0);	
		__ push(T1);	
		__ push(T8);	
		__ move(T1, A2);  
		__ move(T3, A0); 
		__ move(T0, A1);

		// __ cmpl(ecx, 32);
		// __ jcc(Assembler::belowEqual, l_2);                   // <= 32 dwords
		// __ rep_movl();
		__ b(l_2); 	
		__ delayed()->nop();	
		if (is_oop) {
		//  __ jmp(l_stchk);
			__ b(l_stchk); 
			__ delayed()->nop(); 
		}
		//    __ popl(edi);
		//   __ popl(esi);
		//  __ ret(0);
		__ pop(T8);	
		__ pop(T1);	
		__ pop(T0);	
		__ pop(T3);	
		__ jr(RA); 
		__ delayed()->nop(); 

		__ bind(l_2);
		//  __ subl(edi, esi);
		//  __ testl(ecx, ecx);
		// __ jcc(Assembler::zero, l_4);
		__ beq(T1, R0, l_4);  
		__ delayed()->nop(); 
		__ align(16);
		__ bind(l_3);
		//__ movl(edx, Address(esi));
		__ lw(AT, T3, 0);   
		// __ movl(Address(edi, esi, Address::times_1), edx);
		__ sw(AT, T0, 0); 
		// __ addl(esi, 4);
		__ addi(T3, T3, 4);
		__ addi(T0, T0, 4);
		//   __ decl(ecx);
		__ addi(T1, T1, -1); 
		//    __ jcc(Assembler::notEqual, l_3);
		__ bne(T1, R0, l_3); 
		__ delayed()->nop(); 
		if (is_oop) {
			__ bind(l_stchk);
			//      __ movl(edi, Address(esp, 8+ 8));
			//     __ movl(ecx, Address(esp, 8+ 12));
			__ move(T0, A1); 
			__ move(T1, A2); 
			array_store_check();
		}
		__ bind(l_4);
		//    __ popl(edi);
		//   __ popl(esi);
		//  __ ret(0);
		__ pop(T8);
		__ pop(T1);
		__ pop(T0);
		__ pop(T3);
		__ jr(RA); 
		__ delayed()->nop(); 
		return start;
	}

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  address generate_conjoint_int_oop_copy(bool aligned, bool is_oop, const char *name) {
		Label l_2, l_3, l_4, l_stchk;
		StubCodeMark mark(this, "StubRoutines", name);
		__ align(CodeEntryAlignment);
		address start = __ pc();
		address nooverlap_target;

		if (is_oop) {
			nooverlap_target = aligned ?
							StubRoutines::arrayof_oop_disjoint_arraycopy() :
							StubRoutines::oop_disjoint_arraycopy();
		}else {
			nooverlap_target = aligned ?
							StubRoutines::arrayof_jint_disjoint_arraycopy() :
							StubRoutines::jint_disjoint_arraycopy();
		}

		array_overlap_test(nooverlap_target, 2);

		__ push(T3);
		__ push(T0);
		__ push(T1);
		__ push(T8);

		/*
			 __ pushl(esi);
			 __ movl(ecx, Address(esp, 4+12));      // count
			 __ pushl(edi);
			 __ movl(esi, Address(esp, 8+ 4));      // from
			 __ movl(edi, Address(esp, 8+ 8));      // to
		 */ 
		__ move(T1, A2);  
		__ move(T3, A0); 
		__ move(T0, A1);

		//__ leal(esi, Address(esi, ecx, Address::times_4, -4)); // from + count*4 - 4
		__ sll(AT, T1, Address::times_4); 
		__ add(AT, T3, AT); 
		__ lea(T3 , Address(AT, -4)); 
		//__ std();
		//__ leal(edi, Address(edi, ecx, Address::times_4, -4)); // to + count*4 - 4
		__ sll(AT, T1, Address::times_4); 
		__ add(AT, T0, AT); 
		__ lea(T0 , Address(AT, -4)); 

		//    __ cmpl(ecx, 32);
		//   __ jcc(Assembler::above, l_3);                   // > 32 dwords
		//  __ testl(ecx, ecx);
		//__ jcc(Assembler::zero, l_4);
		__ beq(T1, R0, l_4); 
		__ delayed()->nop();  
		// __ subl(edi, esi);
		__ align(16);
		__ bind(l_2);
		// __ movl(edx, Address(esi));
		__ lw(AT, T3, 0);   
		// __ movl(Address(esi, edi, Address::times_1), edx);
		__ sw(AT, T0, 0); 
		// __ subl(esi, 4);
		__ addi(T3, T3, -4); 
		__ addi(T0, T0, -4); 
		//   __ decl(ecx);
		__ addi(T1, T1, -1); 
		//__ jcc(Assembler::notEqual, l_2);
		__ bne(T1, R0, l_2);  
		__ delayed()->nop(); 
		if (is_oop) {
			// __ jmp(l_stchk);
			__ b( l_stchk); 
			__ delayed()->nop(); 
		}
		__ bind(l_4);
		//      __ cld();
		//     __ popl(edi);
		//    __ popl(esi);
		//   __ ret(0);
		__ pop(T8); 
		__ pop(T1); 
		__ pop(T0); 
		__ pop(T3); 
		__ jr(RA); 
		__ delayed()->nop(); 
		__ bind(l_3);
		//   __ rep_movl();
		if (is_oop) {
			__ bind(l_stchk);
			//  __ movl(edi, Address(esp, 8+ 8));
			__ move(T0, A1);  
			// __ movl(ecx, Address(esp, 8+ 12));
			__ move(T1, A2);  
			array_store_check();
		}
		//    __ cld();
		//   __ popl(edi);
		//   __ popl(esi);
		//  __ ret(0);
		__ pop(T8);	
		__ pop(T1);	
		__ pop(T0);	
		__ pop(T3);	
		__ jr(RA);	
		__ delayed()->nop(); 
		return start;
  }

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  // Side Effects:
  //   disjoint_int_copy_entry is set to the no-overlap entry point
  //   used by generate_conjoint_int_oop_copy().
  //
  address generate_disjoint_long_oop_copy(bool aligned, bool is_oop, const char *name) {
		Label l_2, l_3, l_4, l_stchk;
		StubCodeMark mark(this, "StubRoutines", name);
		__ align(CodeEntryAlignment);
		address start = __ pc();
		__ push(T3);	
		__ push(T0);	
		__ push(T1);	
		__ push(T8);	
		__ move(T1, A2);  
		__ move(T3, A0); 
		__ move(T0, A1);

		// __ cmpl(ecx, 32);
		// __ jcc(Assembler::belowEqual, l_2);                   // <= 32 dwords
		// __ rep_movl();
		__ b(l_2); 	
		__ delayed()->nop();	
		if (is_oop) {
		//  __ jmp(l_stchk);
			__ b(l_stchk); 
			__ delayed()->nop(); 
		}
		//    __ popl(edi);
		//   __ popl(esi);
		//  __ ret(0);
		__ pop(T8);	
		__ pop(T1);	
		__ pop(T0);	
		__ pop(T3);	
		__ jr(RA); 
		__ delayed()->nop(); 

		__ bind(l_2);
		//  __ subl(edi, esi);
		//  __ testl(ecx, ecx);
		// __ jcc(Assembler::zero, l_4);
		__ beq(T1, R0, l_4);  
		__ delayed()->nop(); 
		__ align(16);
		__ bind(l_3);
		//__ movl(edx, Address(esi));
		__ ld(AT, T3, 0);   
		// __ movl(Address(edi, esi, Address::times_1), edx);
		__ sd(AT, T0, 0); 
		// __ addl(esi, 4);
		__ addi(T3, T3, 8);
		__ addi(T0, T0, 8);
		//   __ decl(ecx);
		__ addi(T1, T1, -1); 
		//    __ jcc(Assembler::notEqual, l_3);
		__ bne(T1, R0, l_3); 
		__ delayed()->nop(); 
		if (is_oop) {
			__ bind(l_stchk);
			//      __ movl(edi, Address(esp, 8+ 8));
			//     __ movl(ecx, Address(esp, 8+ 12));
			__ move(T0, A1); 
			__ move(T1, A2); 
			array_store_check();
		}
		__ bind(l_4);
		//    __ popl(edi);
		//   __ popl(esi);
		//  __ ret(0);
		__ pop(T8);
		__ pop(T1);
		__ pop(T0);
		__ pop(T3);
		__ jr(RA); 
		__ delayed()->nop(); 
		return start;
	}

  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord == 8-byte boundary
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  // If 'from' and/or 'to' are aligned on 4-byte boundaries, we let
  // the hardware handle it.  The two dwords within qwords that span
  // cache line boundaries will still be loaded and stored atomicly.
  //
  address generate_conjoint_long_oop_copy(bool aligned, bool is_oop, const char *name) {
		Label l_2, l_3, l_4, l_stchk;
		StubCodeMark mark(this, "StubRoutines", name);
		__ align(CodeEntryAlignment);
		address start = __ pc();
		address nooverlap_target;

		if (is_oop) {
			nooverlap_target = aligned ?
							StubRoutines::arrayof_oop_disjoint_arraycopy() :
							StubRoutines::oop_disjoint_arraycopy();
		}else {
			nooverlap_target = aligned ?
							StubRoutines::arrayof_jlong_disjoint_arraycopy() :
							StubRoutines::jlong_disjoint_arraycopy();
		}

		array_overlap_test(nooverlap_target, 3);

		__ push(T3);
		__ push(T0);
		__ push(T1);
		__ push(T8);

		__ move(T1, A2);  
		__ move(T3, A0); 
		__ move(T0, A1);

		//__ leal(esi, Address(esi, ecx, Address::times_4, -4)); // from + count*4 - 4
		__ sll(AT, T1, Address::times_8); 
		__ add(AT, T3, AT); 
		__ lea(T3 , Address(AT, -8)); 
		//__ std();
		//__ leal(edi, Address(edi, ecx, Address::times_4, -4)); // to + count*4 - 4
		__ sll(AT, T1, Address::times_8); 
		__ add(AT, T0, AT); 
		__ lea(T0 , Address(AT, -8)); 

		//    __ cmpl(ecx, 32);
		//   __ jcc(Assembler::above, l_3);                   // > 32 dwords
		//  __ testl(ecx, ecx);
		//__ jcc(Assembler::zero, l_4);
		__ beq(T1, R0, l_4); 
		__ delayed()->nop();  
		// __ subl(edi, esi);
		__ align(16);
		__ bind(l_2);
		// __ movl(edx, Address(esi));
		__ ld(AT, T3, 0);   
		// __ movl(Address(esi, edi, Address::times_1), edx);
		__ sd(AT, T0, 0); 
		// __ subl(esi, 4);
		__ addi(T3, T3, -8); 
		__ addi(T0, T0, -8); 
		//   __ decl(ecx);
		__ addi(T1, T1, -1); 
		//__ jcc(Assembler::notEqual, l_2);
		__ bne(T1, R0, l_2);  
		__ delayed()->nop(); 
		if (is_oop) {
			// __ jmp(l_stchk);
			__ b( l_stchk); 
			__ delayed()->nop(); 
		}
		__ bind(l_4);
		//      __ cld();
		//     __ popl(edi);
		//    __ popl(esi);
		//   __ ret(0);
		__ pop(T8); 
		__ pop(T1); 
		__ pop(T0); 
		__ pop(T3); 
		__ jr(RA); 
		__ delayed()->nop(); 
		__ bind(l_3);
		//   __ rep_movl();
		if (is_oop) {
			__ bind(l_stchk);
			//  __ movl(edi, Address(esp, 8+ 8));
			__ move(T0, A1);  
			// __ movl(ecx, Address(esp, 8+ 12));
			__ move(T1, A2);  
			array_store_check();
		}
		//    __ cld();
		//   __ popl(edi);
		//   __ popl(esi);
		//  __ ret(0);
		__ pop(T8);	
		__ pop(T1);	
		__ pop(T0);	
		__ pop(T3);	
		__ jr(RA);	
		__ delayed()->nop(); 
		return start;
  }
#if 0
  // Arguments:
  //   aligned - true => Input and output aligned on a HeapWord boundary == 8 bytes
  //             ignored
  //   is_oop  - true => oop array, so generate store check code
  //   name    - stub name string
  //
  // Inputs:
  //   c_rarg0   - source array address
  //   c_rarg1   - destination array address
  //   c_rarg2   - element count, treated as ssize_t, can be zero
  //
  address generate_conjoint_long_oop_copy(bool aligned, bool is_oop, const char *name) {
    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();

    Label L_copy_32_bytes, L_copy_8_bytes, L_exit;
    const Register from        = rdi;  // source array address
    const Register to          = rsi;  // destination array address
    const Register qword_count = rdx;  // elements count
    const Register saved_count = rcx;

    __ enter(); // required for proper stackwalking of RuntimeStub frame
    assert_clean_int(c_rarg2, rax);    // Make sure 'count' is clean int.

    address disjoint_copy_entry = NULL;
    if (is_oop) {
      assert(!UseCompressedOops, "shouldn't be called for compressed oops");
      disjoint_copy_entry = disjoint_oop_copy_entry;
      oop_copy_entry  = __ pc();
      array_overlap_test(disjoint_oop_copy_entry, Address::times_8);
    } else {
      disjoint_copy_entry = disjoint_long_copy_entry;
      long_copy_entry = __ pc();
      array_overlap_test(disjoint_long_copy_entry, Address::times_8);
    }
    BLOCK_COMMENT("Entry:");
    // caller can pass a 64-bit byte count here (from Unsafe.copyMemory)

    array_overlap_test(disjoint_copy_entry, Address::times_8);
    setup_arg_regs(); // from => rdi, to => rsi, count => rdx
                      // r9 and r10 may be used to save non-volatile registers

    // 'from', 'to' and 'qword_count' are now valid

    if (is_oop) {
      // Save to and count for store barrier
      __ movptr(saved_count, qword_count);
      // No registers are destroyed by this call
      gen_write_ref_array_pre_barrier(to, saved_count);
    }

    __ jmp(L_copy_32_bytes);

    // Copy trailing qwords
  __ BIND(L_copy_8_bytes);
    __ movq(rax, Address(from, qword_count, Address::times_8, -8));
    __ movq(Address(to, qword_count, Address::times_8, -8), rax);
    __ decrement(qword_count);
    __ jcc(Assembler::notZero, L_copy_8_bytes);

    if (is_oop) {
      __ jmp(L_exit);
    } else {
      inc_counter_np(SharedRuntime::_jlong_array_copy_ctr);
      restore_arg_regs();
      __ xorptr(rax, rax); // return 0
      __ leave(); // required for proper stackwalking of RuntimeStub frame
      __ ret(0);
    }

    // Copy in 32-bytes chunks
    copy_32_bytes_backward(from, to, qword_count, rax, L_copy_32_bytes, L_copy_8_bytes);

    if (is_oop) {
    __ BIND(L_exit);
      __ lea(rcx, Address(to, saved_count, Address::times_8, -8));
      gen_write_ref_array_post_barrier(to, rcx, rax);
      inc_counter_np(SharedRuntime::_oop_array_copy_ctr);
    } else {
      inc_counter_np(SharedRuntime::_jlong_array_copy_ctr);
    }
    restore_arg_regs();
    __ xorptr(rax, rax); // return 0
    __ leave(); // required for proper stackwalking of RuntimeStub frame
    __ ret(0);

    return start;
  }


  // Helper for generating a dynamic type check.
  // Smashes no registers.
  void generate_type_check(Register sub_klass,
                           Register super_check_offset,
                           Register super_klass,
                           Label& L_success) {
    assert_different_registers(sub_klass, super_check_offset, super_klass);

    BLOCK_COMMENT("type_check:");

    Label L_miss;

    // a couple of useful fields in sub_klass:
    int ss_offset = (klassOopDesc::header_size() * HeapWordSize +
                     Klass::secondary_supers_offset_in_bytes());
    int sc_offset = (klassOopDesc::header_size() * HeapWordSize +
                     Klass::secondary_super_cache_offset_in_bytes());
    Address secondary_supers_addr(sub_klass, ss_offset);
    Address super_cache_addr(     sub_klass, sc_offset);

    // if the pointers are equal, we are done (e.g., String[] elements)
    __ cmpptr(super_klass, sub_klass);
    __ jcc(Assembler::equal, L_success);

    // check the supertype display:
    Address super_check_addr(sub_klass, super_check_offset, Address::times_1, 0);
    __ cmpptr(super_klass, super_check_addr); // test the super type
    __ jcc(Assembler::equal, L_success);

    // if it was a primary super, we can just fail immediately
    __ cmpl(super_check_offset, sc_offset);
    __ jcc(Assembler::notEqual, L_miss);

    // Now do a linear scan of the secondary super-klass chain.
    // The repne_scan instruction uses fixed registers, which we must spill.
    // (We need a couple more temps in any case.)
    // This code is rarely used, so simplicity is a virtue here.
    inc_counter_np(SharedRuntime::_partial_subtype_ctr);
    {
      __ push(rax);
      __ push(rcx);
      __ push(rdi);
      assert_different_registers(sub_klass, super_klass, rax, rcx, rdi);

      __ movptr(rdi, secondary_supers_addr);
      // Load the array length.
      __ movl(rcx, Address(rdi, arrayOopDesc::length_offset_in_bytes()));
      // Skip to start of data.
      __ addptr(rdi, arrayOopDesc::base_offset_in_bytes(T_OBJECT));
      // Scan rcx words at [rdi] for occurance of rax
      // Set NZ/Z based on last compare
      __ movptr(rax, super_klass);
      if (UseCompressedOops) {
        // Compare against compressed form.  Don't need to uncompress because
        // looks like orig rax is restored in popq below.
        __ encode_heap_oop(rax);
        __ repne_scanl();
      } else {
        __ repne_scan();
      }

      // Unspill the temp. registers:
      __ pop(rdi);
      __ pop(rcx);
      __ pop(rax);

      __ jcc(Assembler::notEqual, L_miss);
    }

    // Success.  Cache the super we found and proceed in triumph.
    __ movptr(super_cache_addr, super_klass); // note: rax is dead
    __ jmp(L_success);

    // Fall through on failure!
    __ BIND(L_miss);
  }

  //
  //  Generate checkcasting array copy stub
  //
  //  Input:
  //    c_rarg0   - source array address
  //    c_rarg1   - destination array address
  //    c_rarg2   - element count, treated as ssize_t, can be zero
  //    c_rarg3   - size_t ckoff (super_check_offset)
  // not Win64
  //    c_rarg4   - oop ckval (super_klass)
  // Win64
  //    rsp+40    - oop ckval (super_klass)
  //
  //  Output:
  //    rax ==  0  -  success
  //    rax == -1^K - failure, where K is partial transfer count
  //
  address generate_checkcast_copy(const char *name) {

    Label L_load_element, L_store_element, L_do_card_marks, L_done;

    // Input registers (after setup_arg_regs)
    const Register from        = rdi;   // source array address
    const Register to          = rsi;   // destination array address
    const Register length      = rdx;   // elements count
    const Register ckoff       = rcx;   // super_check_offset
    const Register ckval       = r8;    // super_klass

    // Registers used as temps (r13, r14 are save-on-entry)
    const Register end_from    = from;  // source array end address
    const Register end_to      = r13;   // destination array end address
    const Register count       = rdx;   // -(count_remaining)
    const Register r14_length  = r14;   // saved copy of length
    // End pointers are inclusive, and if length is not zero they point
    // to the last unit copied:  end_to[0] := end_from[0]

    const Register rax_oop    = rax;    // actual oop copied
    const Register r11_klass  = r11;    // oop._klass

    //---------------------------------------------------------------
    // Assembler stub will be used for this call to arraycopy
    // if the two arrays are subtypes of Object[] but the
    // destination array type is not equal to or a supertype
    // of the source type.  Each element must be separately
    // checked.

    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();

    __ enter(); // required for proper stackwalking of RuntimeStub frame

    checkcast_copy_entry  = __ pc();
    BLOCK_COMMENT("Entry:");

#ifdef ASSERT
    // caller guarantees that the arrays really are different
    // otherwise, we would have to make conjoint checks
    { Label L;
      array_overlap_test(L, TIMES_OOP);
      __ stop("checkcast_copy within a single array");
      __ bind(L);
    }
#endif //ASSERT

    // allocate spill slots for r13, r14
    enum {
      saved_r13_offset,
      saved_r14_offset,
      saved_rbp_offset,
      saved_rip_offset,
      saved_rarg0_offset
    };
    __ subptr(rsp, saved_rbp_offset * wordSize);
    __ movptr(Address(rsp, saved_r13_offset * wordSize), r13);
    __ movptr(Address(rsp, saved_r14_offset * wordSize), r14);
    setup_arg_regs(4); // from => rdi, to => rsi, length => rdx
                       // ckoff => rcx, ckval => r8
                       // r9 and r10 may be used to save non-volatile registers
#ifdef _WIN64
    // last argument (#4) is on stack on Win64
    const int ckval_offset = saved_rarg0_offset + 4;
    __ movptr(ckval, Address(rsp, ckval_offset * wordSize));
#endif

    // check that int operands are properly extended to size_t
    assert_clean_int(length, rax);
    assert_clean_int(ckoff, rax);

#ifdef ASSERT
    BLOCK_COMMENT("assert consistent ckoff/ckval");
    // The ckoff and ckval must be mutually consistent,
    // even though caller generates both.
    { Label L;
      int sco_offset = (klassOopDesc::header_size() * HeapWordSize +
                        Klass::super_check_offset_offset_in_bytes());
      __ cmpl(ckoff, Address(ckval, sco_offset));
      __ jcc(Assembler::equal, L);
      __ stop("super_check_offset inconsistent");
      __ bind(L);
    }
#endif //ASSERT

    // Loop-invariant addresses.  They are exclusive end pointers.
    Address end_from_addr(from, length, TIMES_OOP, 0);
    Address   end_to_addr(to,   length, TIMES_OOP, 0);
    // Loop-variant addresses.  They assume post-incremented count < 0.
    Address from_element_addr(end_from, count, TIMES_OOP, 0);
    Address   to_element_addr(end_to,   count, TIMES_OOP, 0);

    gen_write_ref_array_pre_barrier(to, count);

    // Copy from low to high addresses, indexed from the end of each array.
    __ lea(end_from, end_from_addr);
    __ lea(end_to,   end_to_addr);
    __ movptr(r14_length, length);        // save a copy of the length
    assert(length == count, "");          // else fix next line:
    __ negptr(count);                     // negate and test the length
    __ jcc(Assembler::notZero, L_load_element);

    // Empty array:  Nothing to do.
    __ xorptr(rax, rax);                  // return 0 on (trivial) success
    __ jmp(L_done);

    // ======== begin loop ========
    // (Loop is rotated; its entry is L_load_element.)
    // Loop control:
    //   for (count = -count; count != 0; count++)
    // Base pointers src, dst are biased by 8*(count-1),to last element.
    __ align(16);

    __ BIND(L_store_element);
    __ store_heap_oop(rax_oop, to_element_addr);  // store the oop
    __ sync();
    __ increment(count);               // increment the count toward zero
    __ jcc(Assembler::zero, L_do_card_marks);

    // ======== loop entry is here ========
    __ BIND(L_load_element);
    __ load_heap_oop(rax_oop, from_element_addr); // load the oop
    __ testptr(rax_oop, rax_oop);
    __ jcc(Assembler::zero, L_store_element);

    __ load_klass(r11_klass, rax_oop);// query the object klass
    generate_type_check(r11_klass, ckoff, ckval, L_store_element);
    // ======== end loop ========

    // It was a real error; we must depend on the caller to finish the job.
    // Register rdx = -1 * number of *remaining* oops, r14 = *total* oops.
    // Emit GC store barriers for the oops we have copied (r14 + rdx),
    // and report their number to the caller.
    assert_different_registers(rax, r14_length, count, to, end_to, rcx);
    __ lea(end_to, to_element_addr);
    gen_write_ref_array_post_barrier(to, end_to, rscratch1);
    __ movptr(rax, r14_length);           // original oops
    __ addptr(rax, count);                // K = (original - remaining) oops
    __ notptr(rax);                       // report (-1^K) to caller
    __ jmp(L_done);

    // Come here on success only.
    __ BIND(L_do_card_marks);
    __ addptr(end_to, -wordSize);         // make an inclusive end pointer
    gen_write_ref_array_post_barrier(to, end_to, rscratch1);
    __ xorptr(rax, rax);                  // return 0 on success

    // Common exit point (success or failure).
    __ BIND(L_done);
    __ movptr(r13, Address(rsp, saved_r13_offset * wordSize));
    __ movptr(r14, Address(rsp, saved_r14_offset * wordSize));
    inc_counter_np(SharedRuntime::_checkcast_array_copy_ctr);
    restore_arg_regs();
    __ leave(); // required for proper stackwalking of RuntimeStub frame
    __ ret(0);

    return start;
  }

  //
  //  Generate 'unsafe' array copy stub
  //  Though just as safe as the other stubs, it takes an unscaled
  //  size_t argument instead of an element count.
  //
  //  Input:
  //    c_rarg0   - source array address
  //    c_rarg1   - destination array address
  //    c_rarg2   - byte count, treated as ssize_t, can be zero
  //
  // Examines the alignment of the operands and dispatches
  // to a long, int, short, or byte copy loop.
  //
  address generate_unsafe_copy(const char *name) {

    Label L_long_aligned, L_int_aligned, L_short_aligned;

    // Input registers (before setup_arg_regs)
    const Register from        = c_rarg0;  // source array address
    const Register to          = c_rarg1;  // destination array address
    const Register size        = c_rarg2;  // byte count (size_t)

    // Register used as a temp
    const Register bits        = rax;      // test copy of low bits

    __ align(CodeEntryAlignment);
    StubCodeMark mark(this, "StubRoutines", name);
    address start = __ pc();

    __ enter(); // required for proper stackwalking of RuntimeStub frame

    // bump this on entry, not on exit:
    inc_counter_np(SharedRuntime::_unsafe_array_copy_ctr);

    __ mov(bits, from);
    __ orptr(bits, to);
    __ orptr(bits, size);

    __ testb(bits, BytesPerLong-1);
    __ jccb(Assembler::zero, L_long_aligned);

    __ testb(bits, BytesPerInt-1);
    __ jccb(Assembler::zero, L_int_aligned);

    __ testb(bits, BytesPerShort-1);
    __ jump_cc(Assembler::notZero, RuntimeAddress(byte_copy_entry));

    __ BIND(L_short_aligned);
    __ shrptr(size, LogBytesPerShort); // size => short_count
    __ jump(RuntimeAddress(short_copy_entry));

    __ BIND(L_int_aligned);
    __ shrptr(size, LogBytesPerInt); // size => int_count
    __ jump(RuntimeAddress(int_copy_entry));

    __ BIND(L_long_aligned);
    __ shrptr(size, LogBytesPerLong); // size => qword_count
    __ jump(RuntimeAddress(long_copy_entry));

    return start;
  }

  // Perform range checks on the proposed arraycopy.
  // Kills temp, but nothing else.
  // Also, clean the sign bits of src_pos and dst_pos.
  void arraycopy_range_checks(Register src,     // source array oop (c_rarg0)
                              Register src_pos, // source position (c_rarg1)
                              Register dst,     // destination array oo (c_rarg2)
                              Register dst_pos, // destination position (c_rarg3)
                              Register length,
                              Register temp,
                              Label& L_failed) {
    BLOCK_COMMENT("arraycopy_range_checks:");

    //  if (src_pos + length > arrayOop(src)->length())  FAIL;
    __ movl(temp, length);
    __ addl(temp, src_pos);             // src_pos + length
    __ cmpl(temp, Address(src, arrayOopDesc::length_offset_in_bytes()));
    __ jcc(Assembler::above, L_failed);

    //  if (dst_pos + length > arrayOop(dst)->length())  FAIL;
    __ movl(temp, length);
    __ addl(temp, dst_pos);             // dst_pos + length
    __ cmpl(temp, Address(dst, arrayOopDesc::length_offset_in_bytes()));
    __ jcc(Assembler::above, L_failed);

    // Have to clean up high 32-bits of 'src_pos' and 'dst_pos'.
    // Move with sign extension can be used since they are positive.
    __ movslq(src_pos, src_pos);
    __ movslq(dst_pos, dst_pos);

    BLOCK_COMMENT("arraycopy_range_checks done");
  }

  //
  //  Generate generic array copy stubs
  //
  //  Input:
  //    c_rarg0    -  src oop
  //    c_rarg1    -  src_pos (32-bits)
  //    c_rarg2    -  dst oop
  //    c_rarg3    -  dst_pos (32-bits)
  // not Win64
  //    c_rarg4    -  element count (32-bits)
  // Win64
  //    rsp+40     -  element count (32-bits)
  //
  //  Output:
  //    rax ==  0  -  success
  //    rax == -1^K - failure, where K is partial transfer count
  //
  address generate_generic_copy(const char *name) {

    Label L_failed, L_failed_0, L_objArray;
    Label L_copy_bytes, L_copy_shorts, L_copy_ints, L_copy_longs;

    // Input registers
    const Register src        = c_rarg0;  // source array oop
    const Register src_pos    = c_rarg1;  // source position
    const Register dst        = c_rarg2;  // destination array oop
    const Register dst_pos    = c_rarg3;  // destination position
    // elements count is on stack on Win64
#ifdef _WIN64
#define C_RARG4 Address(rsp, 6 * wordSize)
#else
#define C_RARG4 c_rarg4
#endif

    { int modulus = CodeEntryAlignment;
      int target  = modulus - 5; // 5 = sizeof jmp(L_failed)
      int advance = target - (__ offset() % modulus);
      if (advance < 0)  advance += modulus;
      if (advance > 0)  __ nop(advance);
    }
    StubCodeMark mark(this, "StubRoutines", name);

    // Short-hop target to L_failed.  Makes for denser prologue code.
    __ BIND(L_failed_0);
    __ jmp(L_failed);
    assert(__ offset() % CodeEntryAlignment == 0, "no further alignment needed");

    __ align(CodeEntryAlignment);
    address start = __ pc();

    __ enter(); // required for proper stackwalking of RuntimeStub frame

    // bump this on entry, not on exit:
    inc_counter_np(SharedRuntime::_generic_array_copy_ctr);

    //-----------------------------------------------------------------------
    // Assembler stub will be used for this call to arraycopy
    // if the following conditions are met:
    //
    // (1) src and dst must not be null.
    // (2) src_pos must not be negative.
    // (3) dst_pos must not be negative.
    // (4) length  must not be negative.
    // (5) src klass and dst klass should be the same and not NULL.
    // (6) src and dst should be arrays.
    // (7) src_pos + length must not exceed length of src.
    // (8) dst_pos + length must not exceed length of dst.
    //

    //  if (src == NULL) return -1;
    __ testptr(src, src);         // src oop
    size_t j1off = __ offset();
    __ jccb(Assembler::zero, L_failed_0);

    //  if (src_pos < 0) return -1;
    __ testl(src_pos, src_pos); // src_pos (32-bits)
    __ jccb(Assembler::negative, L_failed_0);

    //  if (dst == NULL) return -1;
    __ testptr(dst, dst);         // dst oop
    __ jccb(Assembler::zero, L_failed_0);

    //  if (dst_pos < 0) return -1;
    __ testl(dst_pos, dst_pos); // dst_pos (32-bits)
    size_t j4off = __ offset();
    __ jccb(Assembler::negative, L_failed_0);

    // The first four tests are very dense code,
    // but not quite dense enough to put four
    // jumps in a 16-byte instruction fetch buffer.
    // That's good, because some branch predicters
    // do not like jumps so close together.
    // Make sure of this.
    guarantee(((j1off ^ j4off) & ~15) != 0, "I$ line of 1st & 4th jumps");

    // registers used as temp
    const Register r11_length    = r11; // elements count to copy
    const Register r10_src_klass = r10; // array klass
    const Register r9_dst_klass  = r9;  // dest array klass

    //  if (length < 0) return -1;
    __ movl(r11_length, C_RARG4);       // length (elements count, 32-bits value)
    __ testl(r11_length, r11_length);
    __ jccb(Assembler::negative, L_failed_0);

    __ load_klass(r10_src_klass, src);
#ifdef ASSERT
    //  assert(src->klass() != NULL);
    BLOCK_COMMENT("assert klasses not null");
    { Label L1, L2;
      __ testptr(r10_src_klass, r10_src_klass);
      __ jcc(Assembler::notZero, L2);   // it is broken if klass is NULL
      __ bind(L1);
      __ stop("broken null klass");
      __ bind(L2);
      __ load_klass(r9_dst_klass, dst);
      __ cmpq(r9_dst_klass, 0);
      __ jcc(Assembler::equal, L1);     // this would be broken also
      BLOCK_COMMENT("assert done");
    }
#endif

    // Load layout helper (32-bits)
    //
    //  |array_tag|     | header_size | element_type |     |log2_element_size|
    // 32        30    24            16              8     2                 0
    //
    //   array_tag: typeArray = 0x3, objArray = 0x2, non-array = 0x0
    //

    int lh_offset = klassOopDesc::header_size() * HeapWordSize +
                    Klass::layout_helper_offset_in_bytes();

    const Register rax_lh = rax;  // layout helper

    __ movl(rax_lh, Address(r10_src_klass, lh_offset));

    // Handle objArrays completely differently...
    jint objArray_lh = Klass::array_layout_helper(T_OBJECT);
    __ cmpl(rax_lh, objArray_lh);
    __ jcc(Assembler::equal, L_objArray);

    //  if (src->klass() != dst->klass()) return -1;
    __ load_klass(r9_dst_klass, dst);
    __ cmpq(r10_src_klass, r9_dst_klass);
    __ jcc(Assembler::notEqual, L_failed);

    //  if (!src->is_Array()) return -1;
    __ cmpl(rax_lh, Klass::_lh_neutral_value);
    __ jcc(Assembler::greaterEqual, L_failed);

    // At this point, it is known to be a typeArray (array_tag 0x3).
#ifdef ASSERT
    { Label L;
      __ cmpl(rax_lh, (Klass::_lh_array_tag_type_value << Klass::_lh_array_tag_shift));
      __ jcc(Assembler::greaterEqual, L);
      __ stop("must be a primitive array");
      __ bind(L);
    }
#endif

    arraycopy_range_checks(src, src_pos, dst, dst_pos, r11_length,
                           r10, L_failed);

    // typeArrayKlass
    //
    // src_addr = (src + array_header_in_bytes()) + (src_pos << log2elemsize);
    // dst_addr = (dst + array_header_in_bytes()) + (dst_pos << log2elemsize);
    //

    const Register r10_offset = r10;    // array offset
    const Register rax_elsize = rax_lh; // element size

    __ movl(r10_offset, rax_lh);
    __ shrl(r10_offset, Klass::_lh_header_size_shift);
    __ andptr(r10_offset, Klass::_lh_header_size_mask);   // array_offset
    __ addptr(src, r10_offset);           // src array offset
    __ addptr(dst, r10_offset);           // dst array offset
    BLOCK_COMMENT("choose copy loop based on element size");
    __ andl(rax_lh, Klass::_lh_log2_element_size_mask); // rax_lh -> rax_elsize

    // next registers should be set before the jump to corresponding stub
    const Register from     = c_rarg0;  // source array address
    const Register to       = c_rarg1;  // destination array address
    const Register count    = c_rarg2;  // elements count

    // 'from', 'to', 'count' registers should be set in such order
    // since they are the same as 'src', 'src_pos', 'dst'.

  __ BIND(L_copy_bytes);
    __ cmpl(rax_elsize, 0);
    __ jccb(Assembler::notEqual, L_copy_shorts);
    __ lea(from, Address(src, src_pos, Address::times_1, 0));// src_addr
    __ lea(to,   Address(dst, dst_pos, Address::times_1, 0));// dst_addr
    __ movl2ptr(count, r11_length); // length
    __ jump(RuntimeAddress(byte_copy_entry));

  __ BIND(L_copy_shorts);
    __ cmpl(rax_elsize, LogBytesPerShort);
    __ jccb(Assembler::notEqual, L_copy_ints);
    __ lea(from, Address(src, src_pos, Address::times_2, 0));// src_addr
    __ lea(to,   Address(dst, dst_pos, Address::times_2, 0));// dst_addr
    __ movl2ptr(count, r11_length); // length
    __ jump(RuntimeAddress(short_copy_entry));

  __ BIND(L_copy_ints);
    __ cmpl(rax_elsize, LogBytesPerInt);
    __ jccb(Assembler::notEqual, L_copy_longs);
    __ lea(from, Address(src, src_pos, Address::times_4, 0));// src_addr
    __ lea(to,   Address(dst, dst_pos, Address::times_4, 0));// dst_addr
    __ movl2ptr(count, r11_length); // length
    __ jump(RuntimeAddress(int_copy_entry));

  __ BIND(L_copy_longs);
#ifdef ASSERT
    { Label L;
      __ cmpl(rax_elsize, LogBytesPerLong);
      __ jcc(Assembler::equal, L);
      __ stop("must be long copy, but elsize is wrong");
      __ bind(L);
    }
#endif
    __ lea(from, Address(src, src_pos, Address::times_8, 0));// src_addr
    __ lea(to,   Address(dst, dst_pos, Address::times_8, 0));// dst_addr
    __ movl2ptr(count, r11_length); // length
    __ jump(RuntimeAddress(long_copy_entry));

    // objArrayKlass
  __ BIND(L_objArray);
    // live at this point:  r10_src_klass, src[_pos], dst[_pos]

    Label L_plain_copy, L_checkcast_copy;
    //  test array classes for subtyping
    __ load_klass(r9_dst_klass, dst);
    __ cmpq(r10_src_klass, r9_dst_klass); // usual case is exact equality
    __ jcc(Assembler::notEqual, L_checkcast_copy);

    // Identically typed arrays can be copied without element-wise checks.
    arraycopy_range_checks(src, src_pos, dst, dst_pos, r11_length,
                           r10, L_failed);

    __ lea(from, Address(src, src_pos, TIMES_OOP,
                 arrayOopDesc::base_offset_in_bytes(T_OBJECT))); // src_addr
    __ lea(to,   Address(dst, dst_pos, TIMES_OOP,
                 arrayOopDesc::base_offset_in_bytes(T_OBJECT))); // dst_addr
    __ movl2ptr(count, r11_length); // length
  __ BIND(L_plain_copy);
    __ jump(RuntimeAddress(oop_copy_entry));

  __ BIND(L_checkcast_copy);
    // live at this point:  r10_src_klass, !r11_length
    {
      // assert(r11_length == C_RARG4); // will reload from here
      Register r11_dst_klass = r11;
      __ load_klass(r11_dst_klass, dst);

      // Before looking at dst.length, make sure dst is also an objArray.
      __ cmpl(Address(r11_dst_klass, lh_offset), objArray_lh);
      __ jcc(Assembler::notEqual, L_failed);

      // It is safe to examine both src.length and dst.length.
#ifndef _WIN64
      arraycopy_range_checks(src, src_pos, dst, dst_pos, C_RARG4,
                             rax, L_failed);
#else
      __ movl(r11_length, C_RARG4);     // reload
      arraycopy_range_checks(src, src_pos, dst, dst_pos, r11_length,
                             rax, L_failed);
      __ load_klass(r11_dst_klass, dst); // reload
#endif

      // Marshal the base address arguments now, freeing registers.
      __ lea(from, Address(src, src_pos, TIMES_OOP,
                   arrayOopDesc::base_offset_in_bytes(T_OBJECT)));
      __ lea(to,   Address(dst, dst_pos, TIMES_OOP,
                   arrayOopDesc::base_offset_in_bytes(T_OBJECT)));
      __ movl(count, C_RARG4);          // length (reloaded)
      Register sco_temp = c_rarg3;      // this register is free now
      assert_different_registers(from, to, count, sco_temp,
                                 r11_dst_klass, r10_src_klass);
      assert_clean_int(count, sco_temp);

      // Generate the type check.
      int sco_offset = (klassOopDesc::header_size() * HeapWordSize +
                        Klass::super_check_offset_offset_in_bytes());
      __ movl(sco_temp, Address(r11_dst_klass, sco_offset));
      assert_clean_int(sco_temp, rax);
      generate_type_check(r10_src_klass, sco_temp, r11_dst_klass, L_plain_copy);

      // Fetch destination element klass from the objArrayKlass header.
      int ek_offset = (klassOopDesc::header_size() * HeapWordSize +
                       objArrayKlass::element_klass_offset_in_bytes());
      __ movptr(r11_dst_klass, Address(r11_dst_klass, ek_offset));
      __ movl(sco_temp,      Address(r11_dst_klass, sco_offset));
      assert_clean_int(sco_temp, rax);

      // the checkcast_copy loop needs two extra arguments:
      assert(c_rarg3 == sco_temp, "#3 already in place");
      __ movptr(C_RARG4, r11_dst_klass);  // dst.klass.element_klass
      __ jump(RuntimeAddress(checkcast_copy_entry));
    }

  __ BIND(L_failed);
    __ xorptr(rax, rax);
    __ notptr(rax); // return -1
    __ leave();   // required for proper stackwalking of RuntimeStub frame
    __ ret(0);

    return start;
  }

#undef length_arg
#endif

//FIXME
  address generate_disjoint_long_copy(bool aligned, const char *name) {
	  Label l_1, l_2;
	  StubCodeMark mark(this, "StubRoutines", name);
	  __ align(CodeEntryAlignment);
	  address start = __ pc();

	  //      __ movl(ecx, Address(esp, 4+8));       // count
	  //     __ movl(eax, Address(esp, 4+0));       // from
	  //    __ movl(edx, Address(esp, 4+4));       // to
	  __ move(T1, A2);  
	  __ move(T3, A0); 
	  __ move(T0, A1);
	  __ push(T3); 
	  __ push(T0);
	  __ push(T1);
	  //__ subl(edx, eax);
	  //__ jmp(l_2);
	  __ b(l_2);  
	  __ delayed()->nop();   
	  __ align(16);
	  __ bind(l_1);
	  //   if (VM_Version::supports_mmx()) {
	  //     __ movq(mmx0, Address(eax));
	  //     __ movq(Address(eax, edx, Address::times_1), mmx0);
	  //   } else {
	  //   __ fild_d(Address(eax));
	  __ ld(AT, T3, 0);   
	  // __ fistp_d(Address(eax, edx, Address::times_1));
	  __ sd (AT, T0, 0); 
	  //   }
	  //   __ addl(eax, 8);
	  __ addi(T3, T3, 8); 
	  __ addi(T0, T0, 8); 
	  __ bind(l_2);
	  //    __ decl(ecx);
	  __ addi(T1, T1, -1); 
	  //    __ jcc(Assembler::greaterEqual, l_1);
	  __ bgez(T1, l_1);    
	  __ delayed()->nop(); 
	  //  if (VM_Version::supports_mmx()) {
	  //    __ emms();
	  //  }
	  //  __ ret(0);
	  __ pop(T1); 
	  __ pop(T0); 
	  __ pop(T3); 
	  __ jr(RA); 
	  __ delayed()->nop(); 
	  return start;
  }


  address generate_conjoint_long_copy(bool aligned, const char *name) {
	  Label l_1, l_2;
	  StubCodeMark mark(this, "StubRoutines", name);
	  __ align(CodeEntryAlignment);
	  address start = __ pc();
	  address nooverlap_target = aligned ?
		  StubRoutines::arrayof_jlong_disjoint_arraycopy() :
		  StubRoutines::jlong_disjoint_arraycopy();
	  array_overlap_test(nooverlap_target, 3);

	  __ push(T3); 
	  __ push(T0); 
	  __ push(T1); 

		/*      __ movl(ecx, Address(esp, 4+8));       // count
						__ movl(eax, Address(esp, 4+0));       // from
						__ movl(edx, Address(esp, 4+4));       // to
						__ jmp(l_2);

		 */
	  __ move(T1, A2);  
	  __ move(T3, A0); 
	  __ move(T0, A1);
	  __ sll(AT, T1, Address::times_8); 
	  __ add(AT, T3, AT); 
	  __ lea(T3 , Address(AT, -8)); 
	  __ sll(AT, T1, Address::times_8); 
	  __ add(AT, T0, AT); 
	  __ lea(T0 , Address(AT, -8)); 



	  __ b(l_2); 
	  __ delayed()->nop(); 
	  __ align(16);
		__ bind(l_1);
		/*      if (VM_Version::supports_mmx()) {
						__ movq(mmx0, Address(eax, ecx, Address::times_8));
						__ movq(Address(edx, ecx,Address::times_8), mmx0);
						} else {
						__ fild_d(Address(eax, ecx, Address::times_8));
						__ fistp_d(Address(edx, ecx,Address::times_8));
						}
		 */    
		__ ld(AT, T3, 0);   
		__ sd (AT, T0, 0); 
	  __ addi(T3, T3, -8); 
	  __ addi(T0, T0,-8); 
	  __ bind(l_2);
	  //	    __ decl(ecx);
	  __ addi(T1, T1, -1); 
	  //__ jcc(Assembler::greaterEqual, l_1);
	  __ bgez(T1, l_1); 
	  __ delayed()->nop(); 
	  //      if (VM_Version::supports_mmx()) {
	  //      __ emms();
	  //   }
	  //  __ ret(0);
	  __ pop(T1); 
	  __ pop(T0); 
	  __ pop(T3); 
	  __ jr(RA); 
	  __ delayed()->nop();  
	  return start;
  }

  void generate_arraycopy_stubs() {
    if (UseCompressedOops) {
      StubRoutines::_oop_disjoint_arraycopy    = generate_disjoint_int_oop_copy(false, true, "oop_disjoint_arraycopy");
      StubRoutines::_oop_arraycopy   	= generate_conjoint_int_oop_copy(false, true, "oop_arraycopy");
    } else {
      StubRoutines::_oop_disjoint_arraycopy    = generate_disjoint_long_oop_copy(false, true, "oop_disjoint_arraycopy");
      StubRoutines::_oop_arraycopy   	= generate_conjoint_long_oop_copy(false, true, "oop_arraycopy");
    }

    StubRoutines::_jbyte_disjoint_arraycopy  = generate_disjoint_byte_copy(false, "jbyte_disjoint_arraycopy");
    StubRoutines::_jshort_disjoint_arraycopy = generate_disjoint_short_copy(false, "jshort_disjoint_arraycopy");
    StubRoutines::_jint_disjoint_arraycopy   = generate_disjoint_int_oop_copy(false, false, "jint_disjoint_arraycopy");
    StubRoutines::_jlong_disjoint_arraycopy  = generate_disjoint_long_copy(false, "jlong_disjoint_arraycopy");
    StubRoutines::_arrayof_jbyte_disjoint_arraycopy  = generate_disjoint_byte_copy(true, "arrayof_jbyte_disjoint_arraycopy");

    //  if (VM_Version::supports_mmx())
    //if (false)
    // StubRoutines::_arrayof_jshort_disjoint_arraycopy = generate_disjoint_short_mmx_copy_aligned("arrayof_jshort_disjoint_arraycopy");
    // else
    StubRoutines::_arrayof_jshort_disjoint_arraycopy = generate_disjoint_short_copy(true, "arrayof_jshort_disjoint_arraycopy");
    StubRoutines::_arrayof_jint_disjoint_arraycopy   = generate_disjoint_int_oop_copy(true, false, "arrayof_jint_disjoint_arraycopy");
    //StubRoutines::_arrayof_oop_disjoint_arraycopy   = generate_disjoint_int_oop_copy(true, true, "arrayof_oop_disjoint_arraycopy");
    StubRoutines::_arrayof_jlong_disjoint_arraycopy  = generate_disjoint_long_copy(true, "arrayof_jlong_disjoint_arraycopy");

    StubRoutines::_jbyte_arraycopy  = generate_conjoint_byte_copy(false, "jbyte_arraycopy");
    StubRoutines::_jshort_arraycopy = generate_conjoint_short_copy(false, "jshort_arraycopy");
    StubRoutines::_jint_arraycopy   = generate_conjoint_int_oop_copy(false, false, "jint_arraycopy");
    StubRoutines::_jlong_arraycopy  = generate_conjoint_long_copy(false, "jlong_arraycopy");

    StubRoutines::_arrayof_jbyte_arraycopy  = generate_conjoint_byte_copy(true, "arrayof_jbyte_arraycopy");
    StubRoutines::_arrayof_jshort_arraycopy = generate_conjoint_short_copy(true, "arrayof_jshort_arraycopy");
    StubRoutines::_arrayof_jint_arraycopy   = generate_conjoint_int_oop_copy(true, false, "arrayof_jint_arraycopy");
    //StubRoutines::_arrayof_oop_arraycopy    = generate_conjoint_int_oop_copy(true, true, "arrayof_oop_arraycopy");
    StubRoutines::_arrayof_jlong_arraycopy  = generate_conjoint_long_copy(true, "arrayof_jlong_arraycopy");

    StubRoutines::_arrayof_oop_disjoint_arraycopy    = StubRoutines::_oop_disjoint_arraycopy;
    StubRoutines::_arrayof_oop_arraycopy             = StubRoutines::_oop_arraycopy;
  }

//Wang: add a function to implement SafeFetch32 and SafeFetchN
  void generate_safefetch(const char* name, int size, address* entry,
                          address* fault_pc, address* continuation_pc) {
    // safefetch signatures:
    //   int      SafeFetch32(int*      adr, int      errValue);
    //   intptr_t SafeFetchN (intptr_t* adr, intptr_t errValue);
    //
    // arguments:
    //   A0 = adr
    //   A1 = errValue
    //
    // result:
    //   PPC_RET  = *adr or errValue

    StubCodeMark mark(this, "StubRoutines", name);

    // Entry point, pc or function descriptor.
    *entry = __ pc();

    // Load *adr into A1, may fault.
    *fault_pc = __ pc();
    switch (size) {
      case 4:
        // int32_t
        __ lw(A1, A0, 0); 
        break;
      case 8:
        // int64_t
        __ ld(A1, A0, 0); 
        break;
      default:
        ShouldNotReachHere();
    }

    // return errValue or *adr
    *continuation_pc = __ pc();
    __ addu(V0,A1,R0);
    __ jr(RA);
    __ delayed()->nop();
  }


#undef __
#define __ masm->

  // Continuation point for throwing of implicit exceptions that are
  // not handled in the current activation. Fabricates an exception
  // oop and initiates normal exception dispatching in this
  // frame. Since we need to preserve callee-saved values (currently
  // only for C2, but done for C1 as well) we need a callee-saved oop
  // map and therefore have to make these stubs into RuntimeStubs
  // rather than BufferBlobs.  If the compiler needs all registers to
  // be preserved between the fault point and the exception handler
  // then it must assume responsibility for that in
  // AbstractCompiler::continuation_for_implicit_null_exception or
  // continuation_for_implicit_division_by_zero_exception. All other
  // implicit exceptions (e.g., NullPointerException or
  // AbstractMethodError on entry) are either at call sites or
  // otherwise assume that stack unwinding will be initiated, so
  // caller saved registers were assumed volatile in the compiler.
  address generate_throw_exception(const char* name,
                                   address runtime_entry,
                                   bool restore_saved_exception_pc) {
    // Information about frame layout at time of blocking runtime call.
    // Note that we only have to preserve callee-saved registers since
    // the compilers are responsible for supplying a continuation point
		// if they expect all registers to be preserved.
//#define aoqi_test
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif
		enum layout {
			thread_off,    // last_java_sp                
			S7_off,        // callee saved register      sp + 1
			S6_off,        // callee saved register      sp + 2
			S5_off,        // callee saved register      sp + 3
			S4_off,        // callee saved register      sp + 4
			S3_off,        // callee saved register      sp + 5
			S2_off,        // callee saved register      sp + 6
			S1_off,        // callee saved register      sp + 7
			S0_off,        // callee saved register      sp + 8
			FP_off,
			ret_address,
			framesize
		};

		int insts_size = 2048;
		int locs_size  = 32;

		//  CodeBuffer* code     = new CodeBuffer(insts_size, locs_size, 0, 0, 0, false, 
		//  NULL, NULL, NULL, false, NULL, name, false);
		CodeBuffer code (name , insts_size, locs_size);
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif
		OopMapSet* oop_maps  = new OopMapSet();
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif
		MacroAssembler* masm = new MacroAssembler(&code);
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif

		address start = __ pc();
    	//__ stop("generate_throw_exception");
		/*
			 __ move(AT, (int)&jerome1 );
			 __ sw(SP, AT, 0); 	
			 __ move(AT, (int)&jerome2 );
			 __ sw(FP, AT, 0); 	
			 __ move(AT, (int)&jerome3 );
			 __ sw(RA, AT, 0); 	
			 __ move(AT, (int)&jerome4 );
			 __ sw(R0, AT, 0); 	
			 __ move(AT, (int)&jerome5 );
			 __ sw(R0, AT, 0); 	
			 __ move(AT, (int)&jerome6 );
			 __ sw(R0, AT, 0); 	
			 __ move(AT, (int)&jerome7 );
			 __ sw(R0, AT, 0); 	
			 __ move(AT, (int)&jerome10 );
			 __ sw(R0, AT, 0); 	

			 __ pushad();

		//__ enter();
		__ call(CAST_FROM_FN_PTR(address, SharedRuntime::print_call_statistics), 
		relocInfo::runtime_call_type);
		__ delayed()->nop();

		//__ leave();
		__ popad();

		 */

		// This is an inlined and slightly modified version of call_VM
		// which has the ability to fetch the return PC out of
		// thread-local storage and also sets up last_Java_sp slightly
		// differently than the real call_VM
#ifndef OPT_THREAD	
		Register java_thread = TREG;
		__ get_thread(java_thread);
#else
		Register java_thread = TREG;
#endif
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif
		if (restore_saved_exception_pc) {
			__ ld(RA, java_thread, in_bytes(JavaThread::saved_exception_pc_offset())); // eax
		}

		__ enter(); // required for proper stackwalking of RuntimeStub frame

		__ addi(SP, SP, (-1) * (framesize-2) * wordSize); // prolog
		__ sd(S0, SP, S0_off * wordSize);
		__ sd(S1, SP, S1_off * wordSize);
		__ sd(S2, SP, S2_off * wordSize);
		__ sd(S3, SP, S3_off * wordSize);
		__ sd(S4, SP, S4_off * wordSize);
		__ sd(S5, SP, S5_off * wordSize);
		__ sd(S6, SP, S6_off * wordSize);
		__ sd(S7, SP, S7_off * wordSize);

		int frame_complete = __ pc() - start;
		// push java thread (becomes first argument of C function)
		__ sd(java_thread, SP, thread_off * wordSize);
		if (java_thread!=A0)
			__ move(A0, java_thread);

		// Set up last_Java_sp and last_Java_fp
		__ set_last_Java_frame(java_thread, SP, FP, NULL);
		__ relocate(relocInfo::internal_pc_type);
		{
			intptr_t save_pc = (intptr_t)__ pc() +  NativeMovConstReg::instruction_size + NativeCall::return_address_offset + 4;
			__ li48(AT, save_pc);
		}
		__ sd(AT, java_thread, in_bytes(JavaThread::last_Java_pc_offset())); 

		// Call runtime
		__ call(runtime_entry);
		__ delayed()->nop();
		// Generate oop map
		OopMap* map =  new OopMap(framesize, 0);        
		oop_maps->add_gc_map(__ offset(),  map);

		// restore the thread (cannot use the pushed argument since arguments
		// may be overwritten by C code generated by an optimizing compiler);
		// however can use the register value directly if it is callee saved.
#ifndef OPT_THREAD
		__ get_thread(java_thread);
#endif

		__ ld(SP, java_thread, in_bytes(JavaThread::last_Java_sp_offset()));
		//  __ reset_last_Java_frame(java_thread, true);
		__ reset_last_Java_frame(java_thread, true, true);

		// Restore callee save registers.  This must be done after resetting the Java frame
		__ ld(S0, SP, S0_off * wordSize);
		__ ld(S1, SP, S1_off * wordSize);
		__ ld(S2, SP, S2_off * wordSize);
		__ ld(S3, SP, S3_off * wordSize);
		__ ld(S4, SP, S4_off * wordSize);
		__ ld(S5, SP, S5_off * wordSize);
		__ ld(S6, SP, S6_off * wordSize);
		__ ld(S7, SP, S7_off * wordSize);

		// discard arguments
		__ addi(SP, SP, (framesize-2) * wordSize); // epilog
		//	__ leave(); // required for proper stackwalking of RuntimeStub frame
		__ addi(SP, FP, wordSize);
		__ ld(FP, SP, -1*wordSize);
		// check for pending exceptions
#ifdef ASSERT
		Label L;
		__ lw(AT, java_thread, in_bytes(Thread::pending_exception_offset()));
		__ bne(AT, R0, L);
		__ delayed()->nop();
		__ should_not_reach_here();
		__ bind(L);
#endif //ASSERT
		__ jmp(StubRoutines::forward_exception_entry(), relocInfo::runtime_call_type);
		__ delayed()->nop();
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif
		RuntimeStub* stub = RuntimeStub::new_runtime_stub(name, &code,frame_complete, 
										framesize, oop_maps, false);
#ifdef aoqi_test
tty->print_cr("%s:%d name:%s", __func__, __LINE__, name);
#endif
		return stub->entry_point();
  }

  // Initialization
  void generate_initial() {
/*
		// Generates all stubs and initializes the entry points

    // This platform-specific stub is needed by generate_call_stub()
    StubRoutines::mips::_mxcsr_std        = generate_fp_mask("mxcsr_std",        0x0000000000001F80);

    // entry points that exist in all platforms Note: This is code
    // that could be shared among different platforms - however the
    // benefit seems to be smaller than the disadvantage of having a
    // much more complicated generator structure. See also comment in
    // stubRoutines.hpp.

    StubRoutines::_forward_exception_entry = generate_forward_exception();

    StubRoutines::_call_stub_entry =
      generate_call_stub(StubRoutines::_call_stub_return_address);

    // is referenced by megamorphic call
    StubRoutines::_catch_exception_entry = generate_catch_exception();

    // atomic calls
    StubRoutines::_atomic_xchg_entry         = generate_atomic_xchg();
    StubRoutines::_atomic_xchg_ptr_entry     = generate_atomic_xchg_ptr();
    StubRoutines::_atomic_cmpxchg_entry      = generate_atomic_cmpxchg();
    StubRoutines::_atomic_cmpxchg_long_entry = generate_atomic_cmpxchg_long();
    StubRoutines::_atomic_add_entry          = generate_atomic_add();
    StubRoutines::_atomic_add_ptr_entry      = generate_atomic_add_ptr();
    StubRoutines::_fence_entry               = generate_orderaccess_fence();

    StubRoutines::_handler_for_unsafe_access_entry =
      generate_handler_for_unsafe_access();

    // platform dependent
    StubRoutines::mips::_get_previous_fp_entry = generate_get_previous_fp();

    StubRoutines::mips::_verify_mxcsr_entry    = generate_verify_mxcsr();
*/
		// Generates all stubs and initializes the entry points

		//-------------------------------------------------------------
		//-----------------------------------------------------------
		// entry points that exist in all platforms
		// Note: This is code that could be shared among different platforms - however the benefit seems to be smaller 
		// than the disadvantage of having a much more complicated generator structure. 
		// See also comment in stubRoutines.hpp.
		StubRoutines::_forward_exception_entry = generate_forward_exception();    
		StubRoutines::_call_stub_entry = generate_call_stub(StubRoutines::_call_stub_return_address);
		// is referenced by megamorphic call    
		StubRoutines::_catch_exception_entry = generate_catch_exception();    

		StubRoutines::_handler_for_unsafe_access_entry = generate_handler_for_unsafe_access();

		// platform dependent
		StubRoutines::gs2::_get_previous_fp_entry = generate_get_previous_fp();
	}

void generate_all() {
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
    // Generates all stubs and initializes the entry points

    // These entry points require SharedInfo::stack0 to be set up in
    // non-core builds and need to be relocatable, so they each
    // fabricate a RuntimeStub internally.
	/*
    StubRoutines::_throw_AbstractMethodError_entry =
      generate_throw_exception("AbstractMethodError throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_AbstractMethodError),
                               false);

    StubRoutines::_throw_IncompatibleClassChangeError_entry =
      generate_throw_exception("IncompatibleClassChangeError throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_IncompatibleClassChangeError),
                               false);

    StubRoutines::_throw_ArithmeticException_entry =
      generate_throw_exception("ArithmeticException throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_ArithmeticException),
                               true);

    StubRoutines::_throw_NullPointerException_entry =
      generate_throw_exception("NullPointerException throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_NullPointerException),
                               true);

    StubRoutines::_throw_NullPointerException_at_call_entry =
      generate_throw_exception("NullPointerException at call throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_NullPointerException_at_call),
                               false);

    StubRoutines::_throw_StackOverflowError_entry =
      generate_throw_exception("StackOverflowError throw_exception",
                               CAST_FROM_FN_PTR(address,
                                                SharedRuntime::
                                                throw_StackOverflowError),
                               false);

    // entry points that are platform specific
    StubRoutines::mips::_f2i_fixup = generate_f2i_fixup();
    StubRoutines::mips::_f2l_fixup = generate_f2l_fixup();
    StubRoutines::mips::_d2i_fixup = generate_d2i_fixup();
    StubRoutines::mips::_d2l_fixup = generate_d2l_fixup();

    StubRoutines::mips::_float_sign_mask  = generate_fp_mask("float_sign_mask",  0x7FFFFFFF7FFFFFFF);
    StubRoutines::mips::_float_sign_flip  = generate_fp_mask("float_sign_flip",  0x8000000080000000);
    StubRoutines::mips::_double_sign_mask = generate_fp_mask("double_sign_mask", 0x7FFFFFFFFFFFFFFF);
    StubRoutines::mips::_double_sign_flip = generate_fp_mask("double_sign_flip", 0x8000000000000000);

    // support for verify_oop (must happen after universe_init)
    StubRoutines::_verify_oop_subroutine_entry = generate_verify_oop();

    // arraycopy stubs used by compilers
    generate_arraycopy_stubs();
	*/
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
		StubRoutines::_throw_AbstractMethodError_entry         = generate_throw_exception("AbstractMethodError throw_exception",          CAST_FROM_FN_PTR(address, SharedRuntime::throw_AbstractMethodError),  false);
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
//		StubRoutines::_throw_ArithmeticException_entry         = generate_throw_exception("ArithmeticException throw_exception",          CAST_FROM_FN_PTR(address, SharedRuntime::throw_ArithmeticException),  true);
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
//		StubRoutines::_throw_NullPointerException_entry        = generate_throw_exception("NullPointerException throw_exception",         CAST_FROM_FN_PTR(address, SharedRuntime::throw_NullPointerException), true);
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
		StubRoutines::_throw_NullPointerException_at_call_entry= generate_throw_exception("NullPointerException at call throw_exception", CAST_FROM_FN_PTR(address, SharedRuntime::throw_NullPointerException_at_call), false);
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
		StubRoutines::_throw_StackOverflowError_entry          = generate_throw_exception("StackOverflowError throw_exception",           CAST_FROM_FN_PTR(address, SharedRuntime::throw_StackOverflowError),   false);
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif

		//------------------------------------------------------
		//------------------------------------------------------------------
		// entry points that are platform specific  

		// support for verify_oop (must happen after universe_init)
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
		StubRoutines::_verify_oop_subroutine_entry	   = generate_verify_oop();
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
#ifndef CORE
		// arraycopy stubs used by compilers
		generate_arraycopy_stubs();
#ifdef aoqi_test
tty->print_cr("%s:%d", __func__, __LINE__);
#endif
#endif

    // Safefetch stubs.
    generate_safefetch("SafeFetch32", sizeof(int),     &StubRoutines::_safefetch32_entry,
                                                       &StubRoutines::_safefetch32_fault_pc,
                                                       &StubRoutines::_safefetch32_continuation_pc);
    generate_safefetch("SafeFetchN", sizeof(intptr_t), &StubRoutines::_safefetchN_entry,
                                                       &StubRoutines::_safefetchN_fault_pc,
                                                       &StubRoutines::_safefetchN_continuation_pc);
	}

 public:
  StubGenerator(CodeBuffer* code, bool all) : StubCodeGenerator(code) {
    if (all) {
      generate_all();
    } else {
      generate_initial();
    }
  }
}; // end class declaration
/*
address StubGenerator::disjoint_byte_copy_entry  = NULL;
address StubGenerator::disjoint_short_copy_entry = NULL;
address StubGenerator::disjoint_int_copy_entry   = NULL;
address StubGenerator::disjoint_long_copy_entry  = NULL;
address StubGenerator::disjoint_oop_copy_entry   = NULL;

address StubGenerator::byte_copy_entry  = NULL;
address StubGenerator::short_copy_entry = NULL;
address StubGenerator::int_copy_entry   = NULL;
address StubGenerator::long_copy_entry  = NULL;
address StubGenerator::oop_copy_entry   = NULL;

address StubGenerator::checkcast_copy_entry = NULL;
*/
void StubGenerator_generate(CodeBuffer* code, bool all) {
  StubGenerator g(code, all);
}
