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
#include "interp_masm_mips_64.hpp"
#include "interpreter/interpreter.hpp"
#include "interpreter/interpreterRuntime.hpp"
#include "oops/arrayOop.hpp"
#include "oops/markOop.hpp"
#include "oops/methodData.hpp"
#include "oops/method.hpp"
#include "prims/jvmtiExport.hpp"
#include "prims/jvmtiRedefineClassesTrace.hpp"
#include "prims/jvmtiThreadState.hpp"
#include "runtime/basicLock.hpp"
#include "runtime/biasedLocking.hpp"
#include "runtime/sharedRuntime.hpp"
#include "runtime/thread.inline.hpp"


// Implementation of InterpreterMacroAssembler

#ifdef CC_INTERP
void InterpreterMacroAssembler::get_method(Register reg) {
}
#endif // CC_INTERP

void InterpreterMacroAssembler::get_2_byte_integer_at_bcp(Register reg, Register tmp, int offset) {
  /* 2016/5/6 Jin: the runtime address of BCP may be unaligned.
   *   Refer to the SPARC implementation. */
  lbu(reg, BCP, offset+1); 
  lbu(tmp, BCP, offset);
#ifdef _LP64
  dsll(reg, reg, 8);
  daddu(reg, tmp, reg);
#else
  sll(reg, reg, 8);
  addu(reg, tmp, reg);
#endif
}

void InterpreterMacroAssembler::get_4_byte_integer_at_bcp(Register reg, Register tmp, int offset) {
  assert(reg != tmp, "need separate temp register");
  if (offset & 3) { // Offset unaligned?
    lbu(reg, BCP, offset+3);
    lbu(tmp, BCP, offset+2);
#ifdef _LP64
    dsll(reg, reg, 8);
    daddu(reg, tmp, reg);
    lbu(tmp, BCP, offset+1);
    dsll(reg, reg, 8);
    daddu(reg, tmp, reg);
    lbu(tmp, BCP, offset);
    dsll(reg, reg, 8);
    daddu(reg, tmp, reg);
#else
    sll(reg, reg, 8);
    addu(reg, tmp, reg);
    lbu(tmp, BCP, offset+1);
    sll(reg, reg, 8);
    addu(reg, tmp, reg);
    lbu(tmp, BCP, offset);
    sll(reg, reg, 8);
    addu(reg, tmp, reg);
#endif
  } else {
    lwu(reg, BCP, offset);
  }
}

#ifndef CC_INTERP

void InterpreterMacroAssembler::call_VM_leaf_base(address entry_point,
                                                  int number_of_arguments) {
    // interpreter specific
    //
    // Note: No need to save/restore bcp & locals (r13 & r14) pointer
    //       since these are callee saved registers and no blocking/
    //       GC can happen in leaf calls.
    // Further Note: DO NOT save/restore bcp/locals. If a caller has
    // already saved them so that it can use esi/edi as temporaries
    // then a save/restore here will DESTROY the copy the caller
    // saved! There used to be a save_bcp() that only happened in
    // the ASSERT path (no restore_bcp). Which caused bizarre failures
    // when jvm built with ASSERTs.
    /*
  #ifdef ASSERT
  {
  Label L;
  cmpptr(Address(rbp, frame::interpreter_frame_last_sp_offset * wordSize), (int32_t)NULL_WORD);
  jcc(Assembler::equal, L);
  stop("InterpreterMacroAssembler::call_VM_leaf_base:"
  " last_sp != NULL");
  bind(L);
  }
  #endif
  // super call
  MacroAssembler::call_VM_leaf_base(entry_point, number_of_arguments);
  // interpreter specific
  // Used to ASSERT that r13/r14 were equal to frame's bcp/locals
  // but since they may not have been saved (and we don't want to
  // save thme here (see note above) the assert is invalid.
     */
  #ifdef ASSERT
  save_bcp();
  { Label L;
    //cmpl(Address(ebp, frame::interpreter_frame_last_sp_offset * wordSize), 
    //NULL_WORD);
    ld(AT,FP,frame::interpreter_frame_last_sp_offset * wordSize); 
    // jcc(Assembler::equal, L);
    beq(AT,R0,L);  
    delayed()->nop(); 
    stop("InterpreterMacroAssembler::call_VM_leaf_base: last_sp != NULL");
    bind(L);
  }
  #endif
  // super call
  MacroAssembler::call_VM_leaf_base(entry_point, number_of_arguments);
  // interpreter specific
  #ifdef ASSERT
  { Label L;
    ld(T3, FP, frame::interpreter_frame_bcx_offset * wordSize);
    Assembler::beq(BCP, T3, L);
    delayed()->nop();
    stop("InterpreterMacroAssembler::call_VM_leaf_base: esi not callee saved?");
    bind(L);
  }
  { Label L;
    ld(T3, FP, frame::interpreter_frame_locals_offset * wordSize);
    Assembler::beq(LVP, T3, L);
    delayed()->nop();
    stop("InterpreterMacroAssembler::call_VM_leaf_base: edi not callee saved?");
    bind(L);
  }
  #endif
  }

void InterpreterMacroAssembler::call_VM_base(Register oop_result,
                                             Register java_thread,
                                             Register last_java_sp,
                                             address  entry_point,
                                             int      number_of_arguments,
                                             bool     check_exceptions) {
#if 0
	// interpreter specific
  //
  // Note: Could avoid restoring locals ptr (callee saved) - however doesn't
  //       really make a difference for these runtime calls, since they are
  //       slow anyway. Btw., bcp must be saved/restored since it may change
  //       due to GC.
  // assert(java_thread == noreg , "not expecting a precomputed java thread");
  save_bcp();
#ifdef ASSERT
  {
    Label L;
    cmpptr(Address(rbp, frame::interpreter_frame_last_sp_offset * wordSize), (int32_t)NULL_WORD);
    jcc(Assembler::equal, L);
    stop("InterpreterMacroAssembler::call_VM_leaf_base:"
         " last_sp != NULL");
    bind(L);
  }
#endif /* ASSERT */
  // super call
  MacroAssembler::call_VM_base(oop_result, noreg, last_java_sp,
                               entry_point, number_of_arguments,
                               check_exceptions);
  // interpreter specific
  restore_bcp();
  restore_locals();
#endif
#ifdef ASSERT
	{ Label L;
		//  cmpl(Address(ebp, frame::interpreter_frame_last_sp_offset * wordSize),
		//  NULL_WORD);
		// jcc(Assembler::equal, L);
		ld(AT, FP, frame::interpreter_frame_last_sp_offset * wordSize); 
		beq(AT, R0, L); 
		delayed()->nop(); 
		stop("InterpreterMacroAssembler::call_VM_base: last_sp != NULL");
		bind(L);
	}
#endif /* ASSERT */
	// interpreter specific
	//
	// Note: Could avoid restoring locals ptr (callee saved) - however doesn't
	//       really make a difference for these runtime calls, since they are
	//       slow anyway. Btw., bcp must be saved/restored since it may change
	//       due to GC.
	assert(java_thread == noreg , "not expecting a precomputed java thread");
	save_bcp();
	// super call
	MacroAssembler::call_VM_base(oop_result, java_thread, last_java_sp, entry_point, number_of_arguments, check_exceptions);
	restore_bcp();
	restore_locals();
}


void InterpreterMacroAssembler::check_and_handle_popframe(Register java_thread) {
  if (JvmtiExport::can_pop_frame()) {
    Label L;
    // Initiate popframe handling only if it is not already being
    // processed.  If the flag has the popframe_processing bit set, it
    // means that this code is called *during* popframe handling - we
    // don't want to reenter.
    // This method is only called just after the call into the vm in
    // call_VM_base, so the arg registers are available.
    /*
		movl(c_rarg0, Address(r15_thread, JavaThread::popframe_condition_offset()));
    testl(c_rarg0, JavaThread::popframe_pending_bit);
    jcc(Assembler::zero, L);
    testl(c_rarg0, JavaThread::popframe_processing_bit);
    jcc(Assembler::notZero, L);
    // Call Interpreter::remove_activation_preserving_args_entry() to get the
    // address of the same-named entrypoint in the generated interpreter code.
    call_VM_leaf(CAST_FROM_FN_PTR(address, Interpreter::remove_activation_preserving_args_entry));
    jmp(rax);
    bind(L);
		*/
		Register pop_cond = java_thread;
		// Not clear if any other register is available...
		lw(pop_cond, java_thread, in_bytes(JavaThread::popframe_condition_offset()));
		andi(AT, pop_cond, JavaThread::popframe_pending_bit);
		beq(AT, R0, L);		
		delayed()->andi(AT, pop_cond, JavaThread::popframe_processing_bit);		
		bne(AT, R0, L);
		delayed()->nop();
		call( CAST_FROM_FN_PTR(address, Interpreter::remove_activation_preserving_args_entry), relocInfo::runtime_call_type);
		delayed()->nop();
		jr(V0);
		delayed()->nop();
		bind(L);
		get_thread(java_thread);
  }
}


void InterpreterMacroAssembler::load_earlyret_value(TosState state) {
	//T8, thread
	get_thread(T8);
	ld_ptr(T8, T8,in_bytes(JavaThread::jvmti_thread_state_offset())); 
	/* 
	   const Address tos_addr (ecx, JvmtiThreadState::earlyret_tos_offset());
	   const Address oop_addr (ecx, JvmtiThreadState::earlyret_oop_offset());
	   const Address val_addr (ecx, JvmtiThreadState::earlyret_value_offset());
	   const Address val_addr1(ecx, JvmtiThreadState::earlyret_value_offset()
	   + in_ByteSize(wordSize));
	   */ 
	const Address tos_addr (T8, in_bytes(JvmtiThreadState::earlyret_tos_offset()));
	const Address oop_addr (T8, in_bytes(JvmtiThreadState::earlyret_oop_offset()));
	const Address val_addr (T8, in_bytes(JvmtiThreadState::earlyret_value_offset()));
	//V0, oop_addr,V1,val_addr 
	switch (state) {
		case atos: 
			//movl(eax, oop_addr);
			ld_ptr(V0, oop_addr);
			// movl(oop_addr, NULL_WORD);
			st_ptr(R0, oop_addr);  
			//verify_oop(eax, state);       break;
			verify_oop(V0, state);               
			break;
		case ltos: 
			// movl(edx, val_addr1);               // fall through
			ld_ptr(V0, val_addr);               // fall through
			break;
		case btos:                                     // fall through
		case ctos:                                     // fall through
		case stos:                                     // fall through
		case itos: 
			//	movl(eax, val_addr);               
			lw(V0, val_addr);               
			break;
			//FIXME ,I hava no idear fld store to where @jerome 
		case ftos: 
			//fld_s(val_addr);                       
			lwc1(F0,T8, in_bytes(JvmtiThreadState::earlyret_value_offset()));	
			break;
		case dtos: 
			//fld_d(val_addr);                       
			ldc1(F0,T8, in_bytes(JvmtiThreadState::earlyret_value_offset()));	
			break;
		case vtos: /* nothing to do */                    break;
		default  : ShouldNotReachHere();
	}
	// Clean up tos value in the thread object
	// movl(tos_addr,  (int) ilgl);
	//addi(AT,R0,(int)ilgl); 
	move(AT, (int)ilgl); 
	sw(AT, tos_addr);
	// movl(val_addr,  NULL_WORD);
	sw(R0,T8, in_bytes(JvmtiThreadState::earlyret_value_offset())); 
}


void InterpreterMacroAssembler::check_and_handle_earlyret(Register java_thread) {
  if (JvmtiExport::can_force_early_return()) {
    Label L;
		Register tmp = T9;

    //movptr(c_rarg0, Address(r15_thread, JavaThread::jvmti_thread_state_offset()));
		ld_ptr(AT,java_thread, in_bytes(JavaThread::jvmti_thread_state_offset())); 
    //testptr(c_rarg0, c_rarg0);
    //jcc(Assembler::zero, L); // if (thread->jvmti_thread_state() == NULL) exit;
		beq(AT,R0,L);
		delayed()->nop(); 

    // Initiate earlyret handling only if it is not already being processed.
    // If the flag has the earlyret_processing bit set, it means that this code
    // is called *during* earlyret handling - we don't want to reenter.
    //movl(c_rarg0, Address(c_rarg0, JvmtiThreadState::earlyret_state_offset()));
		lw(AT, AT, in_bytes(JvmtiThreadState::earlyret_state_offset()));
    //cmpl(c_rarg0, JvmtiThreadState::earlyret_pending);
    //jcc(Assembler::notEqual, L);
		move(tmp, JvmtiThreadState::earlyret_pending); 
		bne(tmp, AT, L); 
		delayed()->nop(); 
		get_thread(java_thread);

    // Call Interpreter::remove_activation_early_entry() to get the address of the
    // same-named entrypoint in the generated interpreter code.
    //movptr(c_rarg0, Address(r15_thread, JavaThread::jvmti_thread_state_offset()));
		ld_ptr(tmp,java_thread, in_bytes(JavaThread::jvmti_thread_state_offset())); 
    //movl(c_rarg0, Address(c_rarg0, JvmtiThreadState::earlyret_tos_offset()));
		lw(AT,tmp, in_bytes(JvmtiThreadState::earlyret_tos_offset()));
		move(A0, AT); 
		//push(AT); 
		call(CAST_FROM_FN_PTR(address, Interpreter::remove_activation_early_entry),  
				relocInfo::runtime_call_type);
    //call_VM_leaf(CAST_FROM_FN_PTR(address, Interpreter::remove_activation_early_entry), c_rarg0);
    //jmp(rax);
    //bind(L);
		jr(V0); 
		delayed()->nop(); 
		bind(L);
		get_thread(java_thread);
  }
}


void InterpreterMacroAssembler::get_unsigned_2_byte_index_at_bcp(
                                                                 Register reg,
                                                                 int bcp_offset) {
  assert(bcp_offset >= 0, "bcp is still pointing to start of bytecode");
  get_2_byte_integer_at_bcp(reg, AT, bcp_offset);
  hswap(reg);
}


void InterpreterMacroAssembler::get_cache_and_index_at_bcp(Register cache,
                                                           Register index,
                                                           int bcp_offset,
                                                           size_t index_size) {
  assert_different_registers(cache, index);
  get_cache_index_at_bcp(index, bcp_offset, index_size);
  ld(cache, FP, frame::interpreter_frame_cache_offset * wordSize);
  assert(sizeof(ConstantPoolCacheEntry) == 4 * wordSize, "adjust code below");
  assert(exact_log2(in_words(ConstantPoolCacheEntry::size())) == 2, "else change next line");
  shl(index, 2);
}

void InterpreterMacroAssembler::get_cache_and_index_and_bytecode_at_bcp(Register cache,
                                                                         Register index,
                                                                         Register bytecode,
                                                                         int byte_no,
                                                                         int bcp_offset,
                                                                        size_t index_size) {
   get_cache_and_index_at_bcp(cache, index, bcp_offset, index_size);
   // We use a 32-bit load here since the layout of 64-bit words on
   // little-endian machines allow us that.
   dsll(AT, index, Address::times_ptr);
   dadd(AT, cache, AT);
   lw(bytecode, AT, in_bytes(ConstantPoolCache::base_offset() + ConstantPoolCacheEntry::indices_offset()));

   const int shift_count = (1 + byte_no) * BitsPerByte;
   assert((byte_no == TemplateTable::f1_byte && shift_count == ConstantPoolCacheEntry::bytecode_1_shift) ||
          (byte_no == TemplateTable::f2_byte && shift_count == ConstantPoolCacheEntry::bytecode_2_shift),
          "correct shift count");
   dsrl(bytecode, bytecode, shift_count);
   assert(ConstantPoolCacheEntry::bytecode_1_mask == ConstantPoolCacheEntry::bytecode_2_mask, "common mask");
   move(AT, ConstantPoolCacheEntry::bytecode_1_mask);
   andr(bytecode, bytecode, AT);
 }

void InterpreterMacroAssembler::get_cache_entry_pointer_at_bcp(Register cache,
								Register tmp,
								int bcp_offset, size_t index_size) {
	assert(bcp_offset > 0, "bcp is still pointing to start of bytecode");
	assert(cache != tmp, "must use different register");

	get_cache_index_at_bcp(tmp, bcp_offset, index_size);
	assert(sizeof(ConstantPoolCacheEntry) == 4 * wordSize, "adjust code below");
	// convert from field index to ConstantPoolCacheEntry index
	// and from word offset to byte offset
	dsll(tmp, tmp, 2+LogBytesPerWord);
	ld(cache, FP, frame::interpreter_frame_cache_offset * wordSize);
	// skip past the header
	daddi(cache, cache, in_bytes(ConstantPoolCache::base_offset()));
	dadd(cache, cache, AT);
}

void InterpreterMacroAssembler::get_cache_index_at_bcp(Register index,
                                                       int bcp_offset,
                                                       size_t index_size) {
  assert(bcp_offset > 0, "bcp is still pointing to start of bytecode");
  if (index_size == sizeof(u2)) {
    get_2_byte_integer_at_bcp(index, AT, bcp_offset);
  } else if (index_size == sizeof(u4)) {
    assert(EnableInvokeDynamic, "giant index used only for JSR 292");
    get_4_byte_integer_at_bcp(index, AT, bcp_offset);
    // Check if the secondary index definition is still ~x, otherwise
    // we have to change the following assembler code to calculate the
    // plain index.
    assert(ConstantPool::decode_invokedynamic_index(~123) == 123, "else change next line");
    nor(index, index, R0);
    sll(index, index, 0);
  } else if (index_size == sizeof(u1)) {
    lbu(index, BCP, bcp_offset);
  } else {
    ShouldNotReachHere();
  }
}
    
void InterpreterMacroAssembler::get_method_counters(Register method,
                                                    Register mcs, Label& skip) {
  Label has_counters;
  ld(mcs, method, in_bytes(Method::method_counters_offset()));
  bne(mcs, R0, has_counters);
  nop();
  call_VM(noreg, CAST_FROM_FN_PTR(address,
          InterpreterRuntime::build_method_counters), method);
  ld(mcs, method, in_bytes(Method::method_counters_offset()));
  beq(mcs, R0, skip);   // No MethodCounters allocated, OutOfMemory
  nop();
  bind(has_counters);
}

 // Load object from cpool->resolved_references(index)
 void InterpreterMacroAssembler::load_resolved_reference_at_index(
                                            Register result, Register index) {
   assert_different_registers(result, index);
   // convert from field index to resolved_references() index and from
   // word index to byte offset. Since this is a java object, it can be compressed
   Register tmp = index;  // reuse
   shl(tmp, LogBytesPerHeapOop);
 
   get_constant_pool(result);
   // load pointer for resolved_references[] objArray
   ld(result, result, ConstantPool::resolved_references_offset_in_bytes());
   // JNIHandles::resolve(obj);
   // movptr(result, Address(result, 0));
   ld(result, result, 0); //? is needed?
   // Add in the index
   dadd(result, result, tmp);
   load_heap_oop(result, Address(result, arrayOopDesc::base_offset_in_bytes(T_OBJECT)));
 }

// Resets LVP to locals.  Register sub_klass cannot be any of the above.
void InterpreterMacroAssembler::gen_subtype_check( Register Rsup_klass, Register Rsub_klass, Label &ok_is_subtype ) {
  assert( Rsub_klass != Rsup_klass, "Rsup_klass holds superklass" );
  assert( Rsub_klass != T1, "T1 holds 2ndary super array length" );
  assert( Rsub_klass != T0, "T0 holds 2ndary super array scan ptr" );
 // Profile the not-null value's klass.
 // [20130904] Fu: Here T9 and T1 are used as temporary registers.
  profile_typecheck(T9, Rsub_klass, T1); // blows rcx, reloads rdi
 
// Do the check.
  check_klass_subtype(Rsub_klass, Rsup_klass, T1, ok_is_subtype); // blows rcx

// Profile the failure of the check.
  profile_typecheck_failed(T9); // blows rcx
}



// Java Expression Stack

void InterpreterMacroAssembler::pop_ptr(Register r) {
  pop(r);
  //if (TaggedStackInterpreter) addptr(rsp, 1 * wordSize);
//	if (TaggedStackInterpreter) addi(SP,SP, 1 * wordSize);
}
/*
void InterpreterMacroAssembler::pop_ptr(Register r, Register tag) {
  pop(r);
 // if (TaggedStackInterpreter) pop(tag);
}*/

void InterpreterMacroAssembler::pop_i(Register r) {
  // XXX can't use pop currently, upper half non clean
  //movl(r, Address(rsp, 0));
  //addptr(rsp, wordSize);
  lw(r, SP, 0);
  daddi(SP, SP, 8);
	//if (TaggedStackInterpreter) addptr(rsp, 1 * wordSize);
//	if (TaggedStackInterpreter) addi(SP,SP, 1 * wordSize);
}
/*
void InterpreterMacroAssembler::pop_l(Register r) {
  //movq(r, Address(rsp, 0));
  //addptr(rsp, 2 * Interpreter::stackElementSize());
	//FIXME, this directly call assembler. by aoqi 
	ld(r, SP, 0);
	addi(SP, SP, 8);
	if (TaggedStackInterpreter) addi(SP,SP, 2 * wordSize);
}
*/
//FIXME How many registers do push_l & pop_l use? aoqi
void InterpreterMacroAssembler::pop_l(Register lo, Register hi) {
  pop(lo); 
  //if (TaggedStackInterpreter) daddi(SP,SP, 1 * wordSize);       
  pop(hi); 
  //if (TaggedStackInterpreter) daddi(SP,SP, 1 * wordSize);
}

void InterpreterMacroAssembler::pop_f() {
  lwc1(FSF, SP, 0); 
  daddi(SP, SP, 1 * wordSize);
//  if (TaggedStackInterpreter) addi(SP,SP, 1 * wordSize);
}

void InterpreterMacroAssembler::pop_d() {
  pop_dtos_to_esp();
  ldc1(FSF, SP, 0); 
  daddi(SP, SP, 2 * wordSize);
}

// Pop the top of the java expression stack to execution stack (which
// happens to be the same place).
//FIXME ,I hava no idea which register to use
void InterpreterMacroAssembler::pop_dtos_to_esp() {
/*	if (TaggedStackInterpreter) {
		// Pop double value into scratch registers
		//  popl(eax);
		pop(V0); 
		//addl(esp, 1* wordSize);
		addi(SP,SP, 1* wordSize);
		//popl(edx);
		pop(V1);
		//addl(esp, 1* wordSize);
		addi(SP,SP, 1* wordSize);
		// pushl(edx);
		push(V1);
		//pushl(eax);
		push(V0);
	}*/
}

void InterpreterMacroAssembler::pop_ftos_to_esp() {
/*  if (TaggedStackInterpreter) {
		//  popl(eax);
		pop(V0);
		//addl(esp, 1 * wordSize);
		addi(SP,SP, 1 * wordSize);
		// pushl(eax);  // ftos is at esp
		push(V0);  // ftos is at esp
	}*/
}

void InterpreterMacroAssembler::push_ptr(Register r) {
  //if (TaggedStackInterpreter) push(frame::TagReference);
/*	if (TaggedStackInterpreter) {
		move(AT, frame::TagReference); 
		push(AT);
	}//pushl(r);*/
  push(r);
}
/*
void InterpreterMacroAssembler::push_ptr(Register r, Register tag) {
  //if (TaggedStackInterpreter) push(tag);
	if (TaggedStackInterpreter){
		move(AT, tag);
		push(AT);  // tag first
	} 
  push(r);
}*/

void InterpreterMacroAssembler::push_i(Register r) {
  //if (TaggedStackInterpreter) push(frame::TagValue);
/*	if (TaggedStackInterpreter) {
		move(AT, frame::TagValue);	
		push(AT);
	}*/
  push(r);
}
/*
void InterpreterMacroAssembler::push_l(Register r) {
  if (TaggedStackInterpreter) {
    //push(frame::TagValue);
    //subptr(rsp, 1 * wordSize);
    //push(frame::TagValue);
    //subptr(rsp, 1 * wordSize);
		move(AT, frame::TagValue);
		push(AT);
  } else {
    addi(SP, SP, (-2) * wordSize);
  }
  //movq(Address(rsp, 0), r);
	//FIXME, same as pop_l
	sd(r, SP, 0);
}
*/
//FIXME How many registers do push_l & pop_l use? aoqi
void InterpreterMacroAssembler::push_l(Register lo, Register hi) {
  //if (TaggedStackInterpreter) pushl(frame::TagValue);
  /*if (TaggedStackInterpreter) {
    move(AT, frame::TagValue);
		push(AT);
	}*/
	//pushl(hi);
	push(hi);
	//if (TaggedStackInterpreter) pushl(frame::TagValue);
/*	if (TaggedStackInterpreter) {
		move(AT, frame::TagValue);
		push(AT);
	}*/
	//pushl(lo);
	push(lo);
}
//void InterpreterMacroAssembler::push_f(XMMRegister r) {
void InterpreterMacroAssembler::push_f() {
 /* if (TaggedStackInterpreter) {
    move(AT, frame::TagValue);
    push(AT);
  }// Do not schedule for no AGI! Never write beyond esp!*/
  daddi(SP, SP, (-1) * wordSize);
  swc1(FSF, SP, 0 * wordSize);
  sw(R0, SP,  4);
}

//FIXME. aoqi
void InterpreterMacroAssembler::push_d(FloatRegister r) {
 /* if (TaggedStackInterpreter) {
    move(AT, frame::TagValue); 
    push(AT);
    addi(SP, SP, (-3) * wordSize);
    swc1(FSF, SP, 0 * wordSize);
    swc1(SSF, SP, 1 * wordSize);

    lwc1(r, SP, 1*wordSize);
    swc1(r, SP, 2*wordSize);
    move(AT, frame::TagValue);
    sw(AT, SP, 1*wordSize);
  } else {*/
    daddi(SP, SP, (-2) * wordSize);
    sdc1(FSF, SP, 0 * wordSize);
    sdc1(SSF, SP, 1 * wordSize);
 // }
}

void InterpreterMacroAssembler::pop(TosState state) {
  switch (state) {
    case atos: pop(FSR);      break; 
    case btos:
    case ctos:
    case stos:
    case itos: 
	       pop_i(FSR);	
	       break;
    case ltos: 
	       pop_l(FSR, SSR);
	       break;
    case ftos: pop_f();      						break;
    case dtos: pop_d();      						break; 
    case vtos: /* nothing to do */      break;
    default:   ShouldNotReachHere();
  }
  verify_oop(V0, state);
}

//FSR=V0,SSR=V1
void InterpreterMacroAssembler::push(TosState state) {
  verify_oop(V0, state);
  switch (state) {
    case atos:   push(FSR);    break;
    case btos:						     // fall through
    case ctos:						     // fall through
    case stos:						     // fall through
    case itos:
		 push_i(FSR);
		 break;
    case ltos:
    //FIXME aoqi.
		 daddi(SP, SP, (-2) * wordSize);
		 //sd(SSR, SP, 1 * wordSize);
		 sd(R0, SP, 1 * wordSize);
		 sd(FSR, SP, 0 * wordSize);
		 break;
    case ftos: 
		 push_f(); 
		 break;
    case dtos: 
		 //FIXME, I have no idea which register to use 
		 push_d(FSF); 
		 break;
    case vtos: /* nothing to do */                            break;
    default  : ShouldNotReachHere();
  }
}




// Tagged stack helpers for swap and dup
void InterpreterMacroAssembler::load_ptr(int n, Register val) {
  ld(val, SP, Interpreter::expr_offset_in_bytes(n));
  /*if (TaggedStackInterpreter) {
    ld(tag, SP, Interpreter::expr_tag_offset_in_bytes(n));
  }*/
}

void InterpreterMacroAssembler::store_ptr(int n, Register val) {
  sd(val, SP, Interpreter::expr_offset_in_bytes(n));
 /* if (TaggedStackInterpreter) {
    //movptr(Address(rsp, Interpreter::expr_tag_offset_in_bytes(n)), tag);
    sd(tag, SP, Interpreter::expr_tag_offset_in_bytes(n));
  }*/
}

/*
// Tagged local support
//LVP=S7, local variable pointer register , FIXME
void InterpreterMacroAssembler::tag_local(frame::Tag tag, int n) {
  if (TaggedStackInterpreter) {
    if (tag == frame::TagCategory2) {
      //movptr(Address(r14, Interpreter::local_tag_offset_in_bytes(n+1)),
      //     (int32_t)frame::TagValue);
			move(AT, (int)frame::TagValue); 
			sw(AT,LVP, Interpreter::local_tag_offset_in_bytes(n+1));
      //movptr(Address(r14, Interpreter::local_tag_offset_in_bytes(n)),
      //     (int32_t)frame::TagValue);
			sw(AT,LVP, Interpreter::local_tag_offset_in_bytes(n));
    } else {
      //movptr(Address(r14, Interpreter::local_tag_offset_in_bytes(n)), (int32_t)tag);
			move(AT, (int)tag);	   
			sw(AT,LVP, Interpreter::local_tag_offset_in_bytes(n));
    }
  }
}

void InterpreterMacroAssembler::tag_local(frame::Tag tag, Register idx) {
  if (TaggedStackInterpreter) {
    if (tag == frame::TagCategory2) {
      //movptr(Address(r14, idx, Address::times_8,
      //            Interpreter::local_tag_offset_in_bytes(1)), (int32_t)frame::TagValue);
      //movptr(Address(r14, idx, Address::times_8,
      //            Interpreter::local_tag_offset_in_bytes(0)), (int32_t)frame::TagValue);
			shl(idx, 3); 
			add(idx,LVP,idx); 
			move(AT,(int)frame::TagValue); 
			sw(AT, idx, Interpreter::local_tag_offset_in_bytes(1));	    
			shl(idx, 3); 
			add(idx,LVP,idx); 
			move(AT,(int)frame::TagValue); 
			sw(AT, idx, Interpreter::local_tag_offset_in_bytes(0));	    
    } else {
      //movptr(Address(r14, idx, Address::times_8, Interpreter::local_tag_offset_in_bytes(0)),
      //     (int32_t)tag);
			shl(idx, 3); 
			add(idx,LVP,idx); 
			move(AT,(int)tag); 
			sw(AT, idx, Interpreter::local_tag_offset_in_bytes(0));	    
    }
  }
}

void InterpreterMacroAssembler::tag_local(Register tag, Register idx) {
  if (TaggedStackInterpreter) {
    // can only be TagValue or TagReference
    //movptr(Address(r14, idx, Address::times_8, Interpreter::local_tag_offset_in_bytes(0)), tag);
		shl(idx, 3); 
		add(idx,LVP,idx); 
		sw(tag, idx, Interpreter::local_tag_offset_in_bytes(0));	    
  }
}


void InterpreterMacroAssembler::tag_local(Register tag, int n) {
  if (TaggedStackInterpreter) {
    // can only be TagValue or TagReference
    //movptr(Address(r14, Interpreter::local_tag_offset_in_bytes(n)), tag);
		sw(tag, LVP, Interpreter::local_tag_offset_in_bytes(n)); 
  }
}

#ifdef ASSERT
void InterpreterMacroAssembler::verify_local_tag(frame::Tag tag, int n) {
  if (TaggedStackInterpreter) {
     frame::Tag t = tag;
    if (tag == frame::TagCategory2) {
      Label nbl;
      t = frame::TagValue;  // change to what is stored in locals
      //cmpptr(Address(r14, Interpreter::local_tag_offset_in_bytes(n+1)), (int32_t)t);
      //jcc(Assembler::equal, nbl);
			lw(AT, LVP, Interpreter::local_tag_offset_in_bytes(n+1)); 
			addi(AT,AT, -(int)t); 
			beq(AT, R0, nbl); 
			delayed()->nop(); 
      stop("Local tag is bad for long/double");
      bind(nbl);
    }
    Label notBad;
    //cmpq(Address(r14, Interpreter::local_tag_offset_in_bytes(n)), (int32_t)t);
    //jcc(Assembler::equal, notBad);
		lw(AT, LVP, Interpreter::local_tag_offset_in_bytes(n)); 
		addi(AT,AT, -(int)t); 
		beq(AT, R0, notBad); 
		delayed()->nop(); 
    
		// Also compare if the local value is zero, then the tag might
    // not have been set coming from deopt.
    //cmpptr(Address(r14, Interpreter::local_offset_in_bytes(n)), 0);
    //jcc(Assembler::equal, notBad);
		lw(AT, LVP, Interpreter::local_tag_offset_in_bytes(n+1)); 
		beq(AT, R0, notBad); 
		delayed()->nop(); 
    stop("Local tag is bad");
    bind(notBad);
  }
}

void InterpreterMacroAssembler::verify_local_tag(frame::Tag tag, Register idx) {
  if (TaggedStackInterpreter) {
    frame::Tag t = tag;
    if (tag == frame::TagCategory2) {
      Label nbl;
      t = frame::TagValue;  // change to what is stored in locals
      //cmpptr(Address(r14, idx, Address::times_8, Interpreter::local_tag_offset_in_bytes(1)), (int32_t)t);
      //jcc(Assembler::equal, nbl);
			shl(idx, 3); 
			add(idx,LVP,idx); 
			lw(AT, idx, Interpreter::local_tag_offset_in_bytes(1));	
			addi(AT,AT, -(int)t); 
			beq(AT,R0, nbl); 
			delayed()->nop(); 
      stop("Local tag is bad for long/double");
      bind(nbl);
    }
    Label notBad;
    //cmpptr(Address(r14, idx, Address::times_8, Interpreter::local_tag_offset_in_bytes(0)), (int32_t)t);
    //jcc(Assembler::equal, notBad);
		shl(idx, 3); 
		add(idx,LVP,idx); 
		lw(AT, idx, Interpreter::local_tag_offset_in_bytes(0));	
		addi(AT,AT, -(int)t); 
		beq(AT,R0, notBad); 
		delayed()->nop(); 

    // Also compare if the local value is zero, then the tag might
    // not have been set coming from deopt.
    //cmpptr(Address(r14, idx, Address::times_8, Interpreter::local_offset_in_bytes(0)), 0);
    //jcc(Assembler::equal, notBad);
		shl(idx, 3); 
		add(idx,LVP,idx); 
		lw(AT, idx, Interpreter::local_tag_offset_in_bytes(0));	
		beq(AT,R0, notBad); 
		delayed()->nop(); 
    stop("Local tag is bad");
    bind(notBad);
  }
}
#endif // ASSERT
*/
/*
void InterpreterMacroAssembler::super_call_VM_leaf(address entry_point) {
  MacroAssembler::call_VM_leaf_base(entry_point, 0);
}


void InterpreterMacroAssembler::super_call_VM_leaf(address entry_point,
                                                   Register arg_1) {
  if (arg_1 != A0) move(A0, arg_1);
  MacroAssembler::call_VM_leaf_base(entry_point, 1);
}


void InterpreterMacroAssembler::super_call_VM_leaf(address entry_point,
                                                   Register arg_1,
                                                   Register arg_2) {
  if (arg_1 != A0) move(A0, arg_1);
  if (arg_2 != A1) move(A1, arg_2); assert(arg_2 != A0, "smashed argument");
  MacroAssembler::call_VM_leaf_base(entry_point, 2);
}

void InterpreterMacroAssembler::super_call_VM_leaf(address entry_point,
                                                   Register arg_1,
                                                   Register arg_2,
                                                   Register arg_3) {
  if (arg_1 != A0) move(A0, arg_1);
  if (arg_2 != A1) move(A1, arg_2); assert(arg_2 != A0, "smashed argument");
  if (arg_3 != A2) move(A2, arg_3); assert(arg_3 != A0 && arg_3 != A1, "smashed argument");
  MacroAssembler::call_VM_leaf_base(entry_point, 3);
}
*/
// Jump to from_interpreted entry of a call unless single stepping is possible
// in this thread in which case we must call the i2i entry
void InterpreterMacroAssembler::jump_from_interpreted(Register method, Register temp) {
  // record last_sp
  move(Rsender, SP);	
  sd(SP, FP, frame::interpreter_frame_last_sp_offset * wordSize);

  if (JvmtiExport::can_post_interpreter_events()) {
    Label run_compiled_code;
    // JVMTI events, such as single-stepping, are implemented partly by avoiding running
    // compiled code in threads for which the event is enabled.  Check here for
    // interp_only_mode if these events CAN be enabled.
#ifndef OPT_THREAD
	get_thread(temp); 
#else
	move(temp, TREG);
#endif
    // interp_only is an int, on little endian it is sufficient to test the byte only
    // Is a cmpl faster (ce
    //cmpb(Address(temp, JavaThread::interp_only_mode_offset()), 0);
    //jcc(Assembler::zero, run_compiled_code);
    lw(AT, temp, in_bytes(JavaThread::interp_only_mode_offset())); 
    beq(AT, R0, run_compiled_code); 
    delayed()->nop(); 
    //jmp(Address(method, methodOopDesc::interpreter_entry_offset()));
    ld(AT, method, in_bytes(Method::interpreter_entry_offset())); 
    jr(AT); 
    delayed()->nop(); 
    bind(run_compiled_code);
  }

  ld(AT, method, in_bytes(Method::from_interpreted_offset()));
  jr(AT); 
  delayed()->nop();
}


// The following two routines provide a hook so that an implementation
// can schedule the dispatch in two parts.  amd64 does not do this.
void InterpreterMacroAssembler::dispatch_prolog(TosState state, int step) {
  // Nothing amd64 specific to be done here
}

void InterpreterMacroAssembler::dispatch_epilog(TosState state, int step) {
  dispatch_next(state, step);
}

// assume the next bytecode in T8. 
void InterpreterMacroAssembler::dispatch_base(TosState state,
                                              address* table,
                                              bool verifyoop) {
  if (VerifyActivationFrameSize) {
    Label L;

    dsub(T2, FP, SP);
    int min_frame_size = (frame::link_offset - 
	frame::interpreter_frame_initial_sp_offset) * wordSize;
    daddi(T2, T2,- min_frame_size);
    bgez(T2, L);
    delayed()->nop();
    stop("broken stack frame");
    bind(L);
  }
  // FIXME: I do not know which register should pass to verify_oop
  if (verifyoop) verify_oop(FSR, state);
  dsll(T2, Rnext, LogBytesPerWord);

  if((long)table >= (long)Interpreter::dispatch_table(btos) &&
     (long)table <= (long)Interpreter::dispatch_table(vtos)
    ) {
     int table_size = (long)Interpreter::dispatch_table(ctos) - (long)Interpreter::dispatch_table(btos);
     int table_offset = ((int)state - (int)itos) * table_size; 
      
     // 2013/12/17 Fu: GP points to the starting address of Interpreter::dispatch_table(itos).
     // See StubGenerator::generate_call_stub(address& return_address) for the initialization of GP.
     if(table_offset != 0) {
        daddiu(T3, GP, table_offset);
        gsldx(T3, T2, T3, 0); // 2013/5/7 Jin: Godson3 extension instruction
     } else {
        gsldx(T3, T2, GP, 0);
     }
  } else {
     li(T3, (long)table);
     gsldx(T3, T2, T3, 0);
  }

  jr(T3);
  delayed()->nop();
}

void InterpreterMacroAssembler::dispatch_only(TosState state) {
  dispatch_base(state, Interpreter::dispatch_table(state));
}

void InterpreterMacroAssembler::dispatch_only_normal(TosState state) {
  dispatch_base(state, Interpreter::normal_table(state));
}

void InterpreterMacroAssembler::dispatch_only_noverify(TosState state) {
  dispatch_base(state, Interpreter::normal_table(state), false);
}


void InterpreterMacroAssembler::dispatch_next(TosState state, int step) {
  // load next bytecode (load before advancing r13 to prevent AGI)
  lbu(Rnext, BCP, step);
  increment(BCP, step);
  dispatch_base(state, Interpreter::dispatch_table(state));
}

void InterpreterMacroAssembler::dispatch_via(TosState state, address* table) {
  // load current bytecode
  lbu(Rnext, BCP, 0);
  dispatch_base(state, table);
}

// remove activation
//
// Unlock the receiver if this is a synchronized method.
// Unlock any Java monitors from syncronized blocks.
// Remove the activation from the stack.
//
// If there are locked Java monitors
//    If throw_monitor_exception
//       throws IllegalMonitorStateException
//    Else if install_monitor_exception
//       installs IllegalMonitorStateException
//    Else
//       no error processing
// used registers : T1, T2, T3, T8
// T1 : thread, method access flags
// T2 : monitor entry pointer
// T3 : method, monitor top
// T8 : unlock flag
void InterpreterMacroAssembler::remove_activation(
        TosState state,
        Register ret_addr,
        bool throw_monitor_exception,
        bool install_monitor_exception,
	bool notify_jvmdi) {
  // Note: Registers V0, V1 and F0, F1 may be in use for the result
  // check if synchronized method  
  Label unlocked, unlock, no_unlock;

  // get the value of _do_not_unlock_if_synchronized into T8
#ifndef OPT_THREAD
  Register thread = T1;
  get_thread(thread); 
#else
  Register thread = TREG;
#endif
  lb(T8, thread, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset()));
  // reset the flag
  sb(R0, thread, in_bytes(JavaThread::do_not_unlock_if_synchronized_offset())); 
  // get method access flags
  ld(T3, FP, frame::interpreter_frame_method_offset * wordSize); 	
  lw(T1, T3, in_bytes(Method::access_flags_offset()));
  andi(T1, T1, JVM_ACC_SYNCHRONIZED);
  beq(T1, R0, unlocked);
  delayed()->nop();

  // Don't unlock anything if the _do_not_unlock_if_synchronized flag is set.
  bne(T8, R0, no_unlock);
  delayed()->nop();
  // unlock monitor
  push(state);     // save result

  // BasicObjectLock will be first in list, 
  // since this is a synchronized method. However, need
  // to check that the object has not been unlocked by an explicit monitorexit bytecode.  
  daddiu(c_rarg0, FP, frame::interpreter_frame_initial_sp_offset * wordSize 
      - (int)sizeof(BasicObjectLock));
  // address of first monitor
  lw(T1, c_rarg0, BasicObjectLock::obj_offset_in_bytes());
  bne(T1, R0, unlock); 
  delayed()->nop(); 
  pop(state);
  if (throw_monitor_exception) {
    // Entry already unlocked, need to throw exception
    //I think mips do not need empty_FPU_stack 
    // remove possible return value from FPU-stack, otherwise stack could overflow

    empty_FPU_stack();	
    call_VM(NOREG, CAST_FROM_FN_PTR(address, 
	  InterpreterRuntime::throw_illegal_monitor_state_exception));
    should_not_reach_here();
  } else {
    // Monitor already unlocked during a stack unroll. 
    // If requested, install an illegal_monitor_state_exception.
    // Continue with stack unrolling.
    if (install_monitor_exception) {
      // remove possible return value from FPU-stack, 
      // otherwise stack could overflow
      empty_FPU_stack();  		
      call_VM(NOREG, CAST_FROM_FN_PTR(address, 
	    InterpreterRuntime::new_illegal_monitor_state_exception));

    }

    b(unlocked);
    delayed()->nop();
  }

  bind(unlock);  

  unlock_object(c_rarg0);              
  pop(state);
  // Check that for block-structured locking (i.e., that all locked objects has been unlocked)  
  bind(unlocked);  

  // V0, V1: Might contain return value

  // Check that all monitors are unlocked
  {
    Label loop, exception, entry, restart;
    const int entry_size  = frame::interpreter_frame_monitor_size() * wordSize;
    const Address monitor_block_top(FP, 
	frame::interpreter_frame_monitor_block_top_offset * wordSize);

    bind(restart);
    // points to current entry, starting with top-most entry (ecx)
    ld(c_rarg0, monitor_block_top); 
    // points to word before bottom of monitor block (ebx)
    daddiu(T3, FP, frame::interpreter_frame_initial_sp_offset * wordSize); 
    //  lw(AT, R0, 12); 
    b(entry);
    delayed()->nop();

    // Entry already locked, need to throw exception
    bind(exception); 

    if (throw_monitor_exception) {
      // Throw exception      
      // remove possible return value from FPU-stack, 
      // otherwise stack could overflow
      empty_FPU_stack();       
      MacroAssembler::call_VM(NOREG, CAST_FROM_FN_PTR(address, 
	    InterpreterRuntime::throw_illegal_monitor_state_exception));
      should_not_reach_here();
    } else {
      // Stack unrolling. Unlock object and install illegal_monitor_exception
      // Unlock does not block, so don't have to worry about the frame
      // We don't have to preserve eax, edx since we are going to 
      // throw an exception
      unlock_object(c_rarg0);
      if (install_monitor_exception) {
	empty_FPU_stack();  				
	call_VM(NOREG, CAST_FROM_FN_PTR(address, 
	      InterpreterRuntime::new_illegal_monitor_state_exception));
      }

      b(restart);
      delayed()->nop();
    }

    bind(loop);
    //    stop("before object excetpion"); 

    ld(T1, c_rarg0, BasicObjectLock::obj_offset_in_bytes());
    bne(T1, R0, exception);// check if current entry is used
    delayed()->nop();


    daddiu(c_rarg0, c_rarg0, entry_size);// otherwise advance to next entry
    bind(entry);
    bne(c_rarg0, T3, loop);	// check if bottom reached
    delayed()->nop();	// if not at bottom then check this entry
  }        

  bind(no_unlock);

  // jvmpi support (jvmdi does not generate MethodExit on exception / popFrame)
  if (notify_jvmdi) {
    //notify_method_exit(state);              // preserve TOSCA
    notify_method_exit(false,state,NotifyJVMTI);    // preserve TOSCA
  } else {
    // notify_jvmpi_method_exit(state);       // preserve TOSCA
    notify_method_exit(false,state,SkipNotifyJVMTI);// preserve TOSCA
  }

  // remove activation
  ld(SP, FP, frame::interpreter_frame_sender_sp_offset * wordSize); 
  ld(ret_addr, FP, frame::interpreter_frame_return_addr_offset * wordSize);
  ld(FP, FP, frame::interpreter_frame_sender_fp_offset * wordSize);
}

#endif // C_INTERP

// Lock object
//
// Args:
//      c_rarg1: BasicObjectLock to be used for locking
//
// Kills:
//      rax
//      c_rarg0, c_rarg1, c_rarg2, c_rarg3, .. (param regs)
//      rscratch1, rscratch2 (scratch regs)
void InterpreterMacroAssembler::lock_object(Register lock_reg) {
  assert(lock_reg == c_rarg0, "The argument is only for looks. It must be c_rarg0");

  if (UseHeavyMonitors) {
    call_VM(NOREG, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorenter), 
	lock_reg);
  } else {

    Label done;

    const Register swap_reg = T2;  // Must use eax for cmpxchg instruction
    const Register obj_reg  = T1;  // Will contain the oop

    const int obj_offset = BasicObjectLock::obj_offset_in_bytes();
    const int lock_offset = BasicObjectLock::lock_offset_in_bytes ();
    const int mark_offset = lock_offset 
      + BasicLock::displaced_header_offset_in_bytes(); 

    Label slow_case;
    // Load object pointer into obj_reg %ecx
    ld(obj_reg, lock_reg, obj_offset);
    if (UseBiasedLocking) {
      // Note: we use noreg for the temporary register since it's hard
      // to come up with a free register on all incoming code paths
      biased_locking_enter(lock_reg, obj_reg, swap_reg, noreg, false, 
	  done, &slow_case);
    }


    // Load (object->mark() | 1) into swap_reg %eax
    ld(AT, obj_reg, 0);
    ori(swap_reg, AT, 1);


    // Save (object->mark() | 1) into BasicLock's displaced header
    sd(swap_reg, lock_reg, mark_offset);

    assert(lock_offset == 0, "displached header must be first word in BasicObjectLock");
    //if (os::is_MP()) {
      //  lock();
    //}
    cmpxchg(lock_reg, Address(obj_reg, 0), swap_reg);

    if (PrintBiasedLockingStatistics) {
      Label L;
      beq(AT, R0, L);
      delayed()->nop();
      push(T0);
      push(T1);
      atomic_inc32((address)BiasedLocking::fast_path_entry_count_addr(), 1, T0, T1);
      pop(T1);
      pop(T0);
      bind(L);
    }

    bne(AT, R0, done);
    delayed()->nop();

    // Test if the oopMark is an obvious stack pointer, i.e.,
    //  1) (mark & 3) == 0, and
    //  2) SP <= mark < SP + os::pagesize()
    //
    // These 3 tests can be done by evaluating the following 
    // expression: ((mark - esp) & (3 - os::vm_page_size())),
    // assuming both stack pointer and pagesize have their
    // least significant 2 bits clear.
    // NOTE: the oopMark is in swap_reg %eax as the result of cmpxchg

    dsub(swap_reg, swap_reg, SP);
    move(AT, 3 - os::vm_page_size());
    andr(swap_reg, swap_reg, AT);
    // Save the test result, for recursive case, the result is zero
    sd(swap_reg, lock_reg, mark_offset);
    if (PrintBiasedLockingStatistics) {
      Label L;
      bne(swap_reg, R0, L);
      delayed()->nop();
      push(T0);
      push(T1);
      atomic_inc32((address)BiasedLocking::fast_path_entry_count_addr(), 1, T0, T1);
      pop(T1);
      pop(T0);
      bind(L);
    }

    beq(swap_reg, R0, done);
    delayed()->nop();
    bind(slow_case);
    // Call the runtime routine for slow case
    call_VM(NOREG, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorenter), lock_reg);

    bind(done);
  }   
}


// Unlocks an object. Used in monitorexit bytecode and
// remove_activation.  Throws an IllegalMonitorException if object is
// not locked by current thread.
//
// Args:
//      c_rarg1: BasicObjectLock for lock
//
// Kills:
//      rax
//      c_rarg0, c_rarg1, c_rarg2, c_rarg3, ... (param regs)
//      rscratch1, rscratch2 (scratch regs)
// Argument: T6 : Points to BasicObjectLock structure for lock
// Argument: c_rarg0 : Points to BasicObjectLock structure for lock
// Throw an IllegalMonitorException if object is not locked by current thread
void InterpreterMacroAssembler::unlock_object(Register lock_reg) {
  assert(lock_reg == c_rarg0, "The argument is only for looks. It must be c_rarg0");

  if (UseHeavyMonitors) {
    call_VM(noreg, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorexit), lock_reg);
  } else {
    Label done;

    const Register swap_reg   = T2;  // Must use eax for cmpxchg instruction
    const Register header_reg = T3;  // Will contain the old oopMark
    const Register obj_reg    = T1;  // Will contain the oop

    save_bcp(); // Save in case of exception

    // Convert from BasicObjectLock structure to object and BasicLock structure
    // Store the BasicLock address into %eax
    daddi(swap_reg, lock_reg, BasicObjectLock::lock_offset_in_bytes());

    // Load oop into obj_reg(%ecx)
    ld(obj_reg, lock_reg, BasicObjectLock::obj_offset_in_bytes ());
    //free entry 
    sd(R0, lock_reg, BasicObjectLock::obj_offset_in_bytes());
    if (UseBiasedLocking) {
      biased_locking_exit(obj_reg, header_reg, done);
    }

    // Load the old header from BasicLock structure
    ld(header_reg, swap_reg, BasicLock::displaced_header_offset_in_bytes());
    /*
    // Free entry
    sw(R0, lock_reg, BasicObjectLock::obj_offset_in_bytes());
     */
    // zero for recursive case
    beq(header_reg, R0, done);
    delayed()->nop();

    // Atomic swap back the old header
    if (os::is_MP()); //lock();
    cmpxchg(header_reg, Address(obj_reg, 0), swap_reg);

    // zero for recursive case
    bne(AT, R0, done);
    delayed()->nop();

    // Call the runtime routine for slow case.
    sd(obj_reg, lock_reg, BasicObjectLock::obj_offset_in_bytes()); // restore obj
    call_VM(NOREG, CAST_FROM_FN_PTR(address, InterpreterRuntime::monitorexit), 
	lock_reg);

    bind(done);

    restore_bcp();
  }
}

#ifndef CC_INTERP

void InterpreterMacroAssembler::test_method_data_pointer(Register mdp,
    Label& zero_continue) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  ld(mdp, Address(FP, frame::interpreter_frame_mdx_offset * wordSize));
  beq(mdp, R0, zero_continue);
  delayed()->nop();
}


// Set the method data pointer for the current bcp.
void InterpreterMacroAssembler::set_method_data_pointer_for_bcp() {
        assert(ProfileInterpreter, "must be profiling interpreter");
        Label set_mdp;

        // V0 and T0 will be used as two temporary registers.
        sd(V0, SP, (-1) * wordSize);
        sd(T0, SP, (-2) * wordSize);
        daddiu(SP, SP, (-2) * wordSize);

        get_method(T0);
        // Test MDO to avoid the call if it is NULL.
        ld(V0, T0, in_bytes(Method::method_data_offset()));
        beq(V0, R0, set_mdp);
        delayed()->nop();

        // method: T0
        // bcp: BCP --> S0
        call_VM_leaf(CAST_FROM_FN_PTR(address, InterpreterRuntime::bcp_to_di), T0, BCP);
        // mdi: V0
        // mdo is guaranteed to be non-zero here, we checked for it before the call.
        /* Jin: reload T0 */
        get_method(T0);
        ld(T0, T0, in_bytes(Method::method_data_offset()));
        daddiu(T0, T0, in_bytes(MethodData::data_offset()));
        daddu(V0, T0, V0);

        bind(set_mdp);

        sd(V0, FP, frame::interpreter_frame_mdx_offset * wordSize);

        daddiu(SP, SP, 2 * wordSize);
        ld(V0, SP, (-1) * wordSize);
        ld(T0, SP, (-2) * wordSize);
}

void InterpreterMacroAssembler::verify_method_data_pointer() {
assert(ProfileInterpreter, "must be profiling interpreter");
#ifdef ASSERT
  Label verify_continue;
  Register method = V0;
  Register mdp = V1;
  Register tmp = A0;
  push(method);
  push(mdp);
  push(tmp);
  test_method_data_pointer(mdp, verify_continue); // If mdp is zero, continue
  get_method(method);

  // If the mdp is valid, it will point to a DataLayout header which is
  // consistent with the bcp.  The converse is highly probable also.
  lhu(tmp, mdp, in_bytes(DataLayout::bci_offset()));
  ld(AT, method, in_bytes(Method::const_offset()));
  daddu(tmp, tmp, AT);
  daddiu(tmp, tmp, in_bytes(ConstMethod::codes_offset()));
  beq(tmp, BCP, verify_continue);
  nop();
  call_VM_leaf(CAST_FROM_FN_PTR(address, InterpreterRuntime::verify_mdp), method, BCP, mdp);
  bind(verify_continue);
  pop(tmp);
  pop(mdp);
  pop(method);
#endif // ASSERT
}


void InterpreterMacroAssembler::set_mdp_data_at(Register mdp_in,
                                                int constant,
                                                Register value) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  Address data(mdp_in, constant);
  sd(value, data);
}


void InterpreterMacroAssembler::increment_mdp_data_at(Register mdp_in,
                                                      int constant,
                                                      bool decrement) {
  // Counter address
  Address data(mdp_in, constant);

  increment_mdp_data_at(data, decrement);
}

void InterpreterMacroAssembler::increment_mdp_data_at(Address data,
                                                      bool decrement) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  // %%% this does 64bit counters at best it is wasting space
  // at worst it is a rare bug when counters overflow
  Register tmp = S0;
  push(tmp);
  if (decrement) {
    // Decrement the register.
    ld(AT, data);
    daddiu(tmp, AT, (int32_t) -DataLayout::counter_increment);
    // If the decrement causes the counter to overflow, stay negative
    Label L;
    slt(AT, tmp, R0);
    bne(AT, R0, L);
    nop();
    daddi(tmp, tmp, (int32_t) DataLayout::counter_increment);
    bind(L);
    sd(tmp, data);
  } else {
    assert(DataLayout::counter_increment == 1, 
           "flow-free idiom only works with 1");
    ld(AT, data);
    // Increment the register.
    daddiu(tmp, AT, DataLayout::counter_increment);
    // If the increment causes the counter to overflow, pull back by 1.
    slt(AT, tmp, R0);
    dsubu(tmp, tmp, AT);
    sd(tmp, data);
  }
  pop(tmp);
}


void InterpreterMacroAssembler::increment_mdp_data_at(Register mdp_in,
                                                      Register reg,
                                                      int constant,
                                                      bool decrement) {
  Register tmp = S0;
  push(S0);
  if (decrement) {
    // Decrement the register.
    daddu(AT, mdp_in, reg);
    assert(Assembler::is_simm16(constant), "constant is not a simm16 !");
    ld(AT, AT, constant);

    daddiu(tmp, AT, (int32_t) -DataLayout::counter_increment);
    // If the decrement causes the counter to overflow, stay negative
    Label L;
    slt(AT, tmp, R0);
    bne(AT, R0, L);
    nop();
    daddi(tmp, tmp, (int32_t) DataLayout::counter_increment);
    bind(L);

    daddu(AT, mdp_in, reg);
    sd(tmp, AT, constant);
  } else {
    daddu(AT, mdp_in, reg);
    assert(Assembler::is_simm16(constant), "constant is not a simm16 !");
    ld(AT, AT, constant);

    // Increment the register.
    daddiu(tmp, AT, DataLayout::counter_increment);
    // If the increment causes the counter to overflow, pull back by 1.
    slt(AT, tmp, R0);
    dsubu(tmp, tmp, AT);

    daddu(AT, mdp_in, reg);
    sd(tmp, AT, constant);
  }
  pop(S0);
}

void InterpreterMacroAssembler::set_mdp_flag_at(Register mdp_in,
                                                int flag_byte_constant) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  int header_offset = in_bytes(DataLayout::header_offset());
  int header_bits = DataLayout::flag_mask_to_header_mask(flag_byte_constant);
  // Set the flag
  lw(AT, Address(mdp_in, header_offset));
  if(Assembler::is_simm16(header_bits)) {
    ori(AT, AT, header_bits);
  } else {
    push(T8);
    // T8 is used as a temporary register.
    move(T8, header_bits);
    orr(AT, AT, T8);
    pop(T8);
  }
  sw(AT, Address(mdp_in, header_offset));
}



void InterpreterMacroAssembler::test_mdp_data_at(Register mdp_in,
                                                 int offset,
                                                 Register value,
                                                 Register test_value_out,
                                                 Label& not_equal_continue) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  if (test_value_out == noreg) {
    ld(AT, Address(mdp_in, offset));
    bne(AT, value, not_equal_continue);
    nop();
  } else {
    // Put the test value into a register, so caller can use it:
    ld(test_value_out, Address(mdp_in, offset));
    bne(value, test_value_out, not_equal_continue);
    nop();
  }
}


void InterpreterMacroAssembler::update_mdp_by_offset(Register mdp_in,
                                                     int offset_of_disp) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  assert(Assembler::is_simm16(offset_of_disp), "offset is not an simm16");
  ld(AT, mdp_in, offset_of_disp);
  daddu(mdp_in, mdp_in, AT);
  sd(mdp_in, Address(FP, frame::interpreter_frame_mdx_offset * wordSize));
}


void InterpreterMacroAssembler::update_mdp_by_offset(Register mdp_in,
                                                     Register reg,
                                                     int offset_of_disp) {
  assert(ProfileInterpreter, "must be profiling interpreter");
//  Attention: Until now (20121217), we do not support this kind of addressing on Loongson.
//  Address disp_address(mdp_in, reg, Address::times_1, offset_of_disp);
  daddu(AT, reg, mdp_in);
  assert(Assembler::is_simm16(offset_of_disp), "offset is not an simm16");
  ld(AT, AT, offset_of_disp);
  daddu(mdp_in, mdp_in, AT);
  sd(mdp_in, Address(FP, frame::interpreter_frame_mdx_offset * wordSize));
}


void InterpreterMacroAssembler::update_mdp_by_constant(Register mdp_in,
                                                       int constant) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  if(Assembler::is_simm16(constant)) {
    daddiu(mdp_in, mdp_in, constant);
  } else {
    move(AT, constant);
    daddu(mdp_in, mdp_in, AT);
  }
  sd(mdp_in, Address(FP, frame::interpreter_frame_mdx_offset * wordSize));
}


void InterpreterMacroAssembler::update_mdp_for_ret(Register return_bci) {
  assert(ProfileInterpreter, "must be profiling interpreter");
  push(return_bci); // save/restore across call_VM
  call_VM(noreg,
          CAST_FROM_FN_PTR(address, InterpreterRuntime::update_mdp_for_ret),
          return_bci);
  pop(return_bci);
}


void InterpreterMacroAssembler::profile_taken_branch(Register mdp,
                                                     Register bumped_count) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    // Otherwise, assign to mdp
    test_method_data_pointer(mdp, profile_continue);

    // We are taking a branch.  Increment the taken count.
    //increment_mdp_data_at(mdp, in_bytes(JumpData::taken_offset()));
    // We inline increment_mdp_data_at to return bumped_count in a register
    ld(bumped_count, mdp, in_bytes(JumpData::taken_offset()));
    assert(DataLayout::counter_increment == 1, 
           "flow-free idiom only works with 1");
    push(T8);
    // T8 is used as a temporary register.
    daddiu(T8, bumped_count, DataLayout::counter_increment);
    slt(AT, T8, R0);
    dsubu(bumped_count, T8, AT);
    pop(T8);
    sd(bumped_count, mdp, in_bytes(JumpData::taken_offset())); // Store back out
    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_offset(mdp, in_bytes(JumpData::displacement_offset()));
    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_not_taken_branch(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // We are taking a branch.  Increment the not taken count.
    increment_mdp_data_at(mdp, in_bytes(BranchData::not_taken_offset()));

    // The method data pointer needs to be updated to correspond to
    // the next bytecode
    update_mdp_by_constant(mdp, in_bytes(BranchData::branch_data_size()));
    bind(profile_continue);
  }  
}


void InterpreterMacroAssembler::profile_call(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // We are making a call.  Increment the count.
    increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));

    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_constant(mdp, in_bytes(CounterData::counter_data_size()));
    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_final_call(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);
    // We are making a call.  Increment the count.
    increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));

    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_constant(mdp, in_bytes(VirtualCallData:: virtual_call_data_size()));
    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_virtual_call(Register receiver,
                                                     Register mdp,
                                                     Register reg2,
                                                     bool receiver_can_be_null) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    Label skip_receiver_profile;
    if (receiver_can_be_null) {
       Label not_null;
       bne(receiver, R0, not_null);
       nop();
       // We are making a call.  Increment the count.
       increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));
       beq(R0, R0, skip_receiver_profile);
       nop();
       bind(not_null);
    }

    // Record the receiver type.
    record_klass_in_profile(receiver, mdp, reg2, true);
    bind(skip_receiver_profile);

    // The method data pointer needs to be updated to reflect the new target.
    update_mdp_by_constant(mdp, in_bytes(VirtualCallData::virtual_call_data_size()));
    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_checkcast(bool is_null, Register mdp) {
// In x86, this method does not exist.
#ifndef CORE
        if (ProfileInterpreter) {
                Label profile_continue;

                // If no method data exists, go to profile_continue.
                test_method_data_pointer(mdp, profile_continue);

                if (is_null)                // Set the flag to true.
                        set_mdp_flag_at(mdp, BitData::null_seen_byte_constant());
                        //set_mdp_flag_at(mdp, BitData::null_flag_constant());

                // The method data pointer needs to be updated.
                update_mdp_by_constant(mdp, in_bytes(BitData::bit_data_size()));

                bind (profile_continue);
        }
#endif // !CORE
}

// This routine creates a state machine for updating the multi-row
// type profile at a virtual call site (or other type-sensitive bytecode).
// The machine visits each row (of receiver/count) until the receiver type
// is found, or until it runs out of rows.  At the same time, it remembers
// the location of the first empty row.  (An empty row records null for its
// receiver, and can be allocated for a newly-observed receiver type.)
// Because there are two degrees of freedom in the state, a simple linear
// search will not work; it must be a decision tree.  Hence this helper
// function is recursive, to generate the required tree structured code.
// It's the interpreter, so we are trading off code space for speed.
// See below for example code.
void InterpreterMacroAssembler::record_klass_in_profile_helper(
                                        Register receiver, Register mdp,
                                        Register reg2,
                                        int start_row, Label& done, bool is_virtual_call) {
  if (TypeProfileWidth == 0) {
    if (is_virtual_call) {
      increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));
    }
    return;
  }

  int last_row = VirtualCallData::row_limit() - 1;
  assert(start_row <= last_row, "must be work left to do");
  // Test this row for both the receiver and for null.
  // Take any of three different outcomes:
  //   1. found receiver => increment count and goto done
  //   2. found null => keep looking for case 1, maybe allocate this cell
  //   3. found something else => keep looking for cases 1 and 2
  // Case 3 is handled by a recursive call.
  for (int row = start_row; row <= last_row; row++) {
    Label next_test;
    bool test_for_null_also = (row == start_row);

    // See if the receiver is receiver[n].
    int recvr_offset = in_bytes(VirtualCallData::receiver_offset(row));
    test_mdp_data_at(mdp, recvr_offset, receiver,
                     (test_for_null_also ? reg2 : noreg),
                     next_test);
    // (Reg2 now contains the receiver from the CallData.)

    // The receiver is receiver[n].  Increment count[n].
    int count_offset = in_bytes(VirtualCallData::receiver_count_offset(row));
    increment_mdp_data_at(mdp, count_offset);
    beq(R0, R0, done);
    nop();
    bind(next_test);

    if (test_for_null_also) {
      Label found_null;
      // Failed the equality check on receiver[n]...  Test for null.
      if (start_row == last_row) {
        // The only thing left to do is handle the null case.
        if (is_virtual_call) {
          beq(reg2, R0, found_null);
          nop();
          // Receiver did not match any saved receiver and there is no empty row for it.
          // Increment total counter to indicate polymorphic case.
          increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));
          beq(R0, R0, done);
          nop();
          bind(found_null);
        } else {
          bne(reg2, R0, done);
          nop();
        }
        break;
      }
      // Since null is rare, make it be the branch-taken case.
      beq(reg2, R0, found_null);
      nop();

      // Put all the "Case 3" tests here.
      record_klass_in_profile_helper(receiver, mdp, reg2, start_row + 1, done, is_virtual_call);

      // Found a null.  Keep searching for a matching receiver,
      // but remember that this is an empty (unused) slot.
      bind(found_null);
    }
  }

  // In the fall-through case, we found no matching receiver, but we
  // observed the receiver[start_row] is NULL.

  // Fill in the receiver field and increment the count.
  int recvr_offset = in_bytes(VirtualCallData::receiver_offset(start_row));
  set_mdp_data_at(mdp, recvr_offset, receiver);
  int count_offset = in_bytes(VirtualCallData::receiver_count_offset(start_row));
  move(reg2, DataLayout::counter_increment);
  set_mdp_data_at(mdp, count_offset, reg2);
  if (start_row > 0) {
    beq(R0, R0, done);
    nop();
  }
}

// Example state machine code for three profile rows:
//   // main copy of decision tree, rooted at row[1]
//   if (row[0].rec == rec) { row[0].incr(); goto done; }
//   if (row[0].rec != NULL) {
//     // inner copy of decision tree, rooted at row[1]
//     if (row[1].rec == rec) { row[1].incr(); goto done; }
//     if (row[1].rec != NULL) {
//       // degenerate decision tree, rooted at row[2]
//       if (row[2].rec == rec) { row[2].incr(); goto done; }
//       if (row[2].rec != NULL) { goto done; } // overflow
//       row[2].init(rec); goto done;
//     } else {
//       // remember row[1] is empty
//       if (row[2].rec == rec) { row[2].incr(); goto done; }
//       row[1].init(rec); goto done;
//     }
//   } else {
//     // remember row[0] is empty
//     if (row[1].rec == rec) { row[1].incr(); goto done; }
//     if (row[2].rec == rec) { row[2].incr(); goto done; }
//     row[0].init(rec); goto done;
//   }

void InterpreterMacroAssembler::record_klass_in_profile(Register receiver,
                                                        Register mdp,
                                                        Register reg2, bool is_virtual_call) {
  assert(ProfileInterpreter, "must be profiling");
  Label done;

  record_klass_in_profile_helper(receiver, mdp, reg2, 0, done, is_virtual_call);

  bind (done);
}

void InterpreterMacroAssembler::profile_ret(Register return_bci,
                                            Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;
    uint row;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // Update the total ret count.
    increment_mdp_data_at(mdp, in_bytes(CounterData::count_offset()));

    for (row = 0; row < RetData::row_limit(); row++) {
      Label next_test;

      // See if return_bci is equal to bci[n]:
      test_mdp_data_at(mdp,
                       in_bytes(RetData::bci_offset(row)),
                       return_bci, noreg,
                       next_test);

      // return_bci is equal to bci[n].  Increment the count.
      increment_mdp_data_at(mdp, in_bytes(RetData::bci_count_offset(row)));

      // The method data pointer needs to be updated to reflect the new target.
      update_mdp_by_offset(mdp,
                           in_bytes(RetData::bci_displacement_offset(row)));
      beq(R0, R0, profile_continue);
      nop();
      bind(next_test);
    }

    update_mdp_for_ret(return_bci);

    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_null_seen(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    set_mdp_flag_at(mdp, BitData::null_seen_byte_constant());

    // The method data pointer needs to be updated.
    int mdp_delta = in_bytes(BitData::bit_data_size());
    if (TypeProfileCasts) {
      mdp_delta = in_bytes(VirtualCallData::virtual_call_data_size());
    }
    update_mdp_by_constant(mdp, mdp_delta);

    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_typecheck_failed(Register mdp) {
  if (ProfileInterpreter && TypeProfileCasts) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    int count_offset = in_bytes(CounterData::count_offset());
    // Back up the address, since we have already bumped the mdp.
    count_offset -= in_bytes(VirtualCallData::virtual_call_data_size());

    // *Decrement* the counter.  We expect to see zero or small negatives.
    increment_mdp_data_at(mdp, count_offset, true);

    bind (profile_continue);
  }
}


void InterpreterMacroAssembler::profile_typecheck(Register mdp, Register klass, Register reg2) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // The method data pointer needs to be updated.
    int mdp_delta = in_bytes(BitData::bit_data_size());
    if (TypeProfileCasts) {

      mdp_delta = in_bytes(VirtualCallData::virtual_call_data_size());

      // Record the object type.
      record_klass_in_profile(klass, mdp, reg2, false);
    }
    update_mdp_by_constant(mdp, mdp_delta);

    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_switch_default(Register mdp) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // Update the default case count
    increment_mdp_data_at(mdp, in_bytes(MultiBranchData::default_count_offset()));

    // The method data pointer needs to be updated.
    update_mdp_by_offset(mdp, in_bytes(MultiBranchData:: default_displacement_offset()));

    bind(profile_continue);
  }
}


void InterpreterMacroAssembler::profile_switch_case(Register index,
                                                    Register mdp,
                                                    Register reg2) {
  if (ProfileInterpreter) {
    Label profile_continue;

    // If no method data exists, go to profile_continue.
    test_method_data_pointer(mdp, profile_continue);

    // Build the base (index * per_case_size_in_bytes()) + case_array_offset_in_bytes()
    move(reg2, in_bytes(MultiBranchData::per_case_size()));
    if (UseLoongsonISA) {
      gsdmult(index, index, reg2);
    } else {
      dmult(index, reg2);
      mflo(index);
    }
//    addptr(index, in_bytes(MultiBranchData::case_array_offset())); // XXX l ?
    daddiu(index, index, in_bytes(MultiBranchData::case_array_offset()));

    // Update the case count
    increment_mdp_data_at(mdp, index, in_bytes(MultiBranchData::relative_count_offset()));

    // The method data pointer needs to be updated.
    update_mdp_by_offset(mdp, index, in_bytes(MultiBranchData:: relative_displacement_offset()));

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_obj_type(Register obj, const Address& mdo_addr) {
  Label update, next, none;

  verify_oop(obj);

  //testptr(obj, obj);
  //jccb(Assembler::notZero, update);
  bne(obj, R0, update);
  nop();

  //orptr(mdo_addr, TypeEntries::null_seen);
  push(T1);
  if (mdo_addr.index() == noreg) {
    ld(T1, mdo_addr);
  } else {
    guarantee(T1 != mdo_addr.base(), "The base register will be corrupted !");
    guarantee(T1 != mdo_addr.index(), "The index register will be corrupted !");

    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    ld(T1, AT, mdo_addr.disp());
  }
  li(AT, TypeEntries::null_seen);
  orr(AT, T1, AT);
  if (mdo_addr.index() == noreg) {
    sd(AT, mdo_addr);
  } else {
    guarantee(T1 != mdo_addr.base(), "The base register will be corrupted !");
    guarantee(T1 != mdo_addr.index(), "The index register will be corrupted !");

    dsll(T1, mdo_addr.index(), mdo_addr.scale());
    daddu(T1, T1, mdo_addr.base());
    sd(AT, T1, mdo_addr.disp());
  }
  pop(T1);

  //jmpb(next);
  beq(R0, R0, next);
  nop();

  bind(update);
  load_klass(obj, obj);

  //xorptr(obj, mdo_addr);
  if (mdo_addr.index() == noreg) {
    ld(AT, mdo_addr);
  } else {
    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    ld(AT, AT, mdo_addr.disp());
  }
  xorr(obj, obj, AT);

  //testptr(obj, TypeEntries::type_klass_mask);
  //jccb(Assembler::zero, next); // klass seen before, nothing to
                               // do. The unknown bit may have been
                               // set already but no need to check.
  li(AT, TypeEntries::type_klass_mask);
  andr(AT, obj, AT);
  beq(AT, R0, next);
  nop();

  //testptr(obj, TypeEntries::type_unknown);
  //jccb(Assembler::notZero, next); // already unknown. Nothing to do anymore.
  li(AT, TypeEntries::type_unknown);
  andr(AT, AT, obj);
  bne(AT, R0, next);
  nop();

  //cmpptr(mdo_addr, 0);
  //jccb(Assembler::equal, none);
  if (mdo_addr.index() == noreg) {
    ld(AT, mdo_addr);
  } else {
    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    ld(AT, AT, mdo_addr.disp());
  }
  beq(AT, R0, none);
  nop();
  

  //cmpptr(mdo_addr, TypeEntries::null_seen);
  //jccb(Assembler::equal, none);
  push(T1);
  if (mdo_addr.index() == noreg) {
    ld(T1, mdo_addr);
  } else {
    guarantee(T1 != mdo_addr.base(), "The base register will be corrupted !");
    guarantee(T1 != mdo_addr.index(), "The index register will be corrupted !");

    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    ld(T1, AT, mdo_addr.disp());
  }
  li(AT, TypeEntries::null_seen);
  subu(AT, AT, T1);
  pop(T1);
  beq(AT, R0, none);
  nop();

  // There is a chance that the checks above (re-reading profiling
  // data from memory) fail if another thread has just set the
  // profiling to this obj's klass
  //xorptr(obj, mdo_addr);
  //testptr(obj, TypeEntries::type_klass_mask);
  //jccb(Assembler::zero, next);
  if (mdo_addr.index() == noreg) {
    ld(AT, mdo_addr);
  } else {
    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    ld(AT, AT, mdo_addr.disp());
  }
  xorr(obj, obj, AT);
  li(AT, TypeEntries::type_klass_mask);
  andr(AT, obj, AT);
  beq(AT, R0, next);
  nop();

  // different than before. Cannot keep accurate profile.
  //orptr(mdo_addr, TypeEntries::type_unknown);
  //jmpb(next);
  push(T1);
  if (mdo_addr.index() == noreg) {
    ld(T1, mdo_addr); 
  } else {
    guarantee(T1 != mdo_addr.base(), "The base register will be corrupted !");
    guarantee(T1 != mdo_addr.index(), "The index register will be corrupted !");

    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    ld(T1, AT, mdo_addr.disp());
  }
  li(AT, TypeEntries::type_unknown);
  orr(AT, T1, AT);
  if (mdo_addr.index() == noreg) {
    sd(AT, mdo_addr);
  } else {
    guarantee(T1 != mdo_addr.base(), "The base register will be corrupted !");
    guarantee(T1 != mdo_addr.index(), "The index register will be corrupted !");

    dsll(T1, mdo_addr.index(), mdo_addr.scale());
    daddu(T1, T1, mdo_addr.base());
    sd(AT, T1, mdo_addr.disp());
  }
  pop(T1);
  beq(R0, R0, next);
  nop();
  

  bind(none);
  // first time here. Set profile type.
  //movptr(mdo_addr, obj);
  if (mdo_addr.index() == noreg) {
    sd(obj, mdo_addr);
  } else {
    dsll(AT, mdo_addr.index(), mdo_addr.scale());
    daddu(AT, AT, mdo_addr.base());
    sd(obj, AT, mdo_addr.disp());
  }

  bind(next);
}

void InterpreterMacroAssembler::profile_arguments_type(Register mdp, Register callee, Register tmp, bool is_virtual) {
  if (!ProfileInterpreter) {
    return;
  }

  if (MethodData::profile_arguments() || MethodData::profile_return()) {
    Label profile_continue;

    test_method_data_pointer(mdp, profile_continue);

    int off_to_start = is_virtual ? in_bytes(VirtualCallData::virtual_call_data_size()) : in_bytes(CounterData::counter_data_size());

    //cmpb(Address(mdp, in_bytes(DataLayout::tag_offset()) - off_to_start), is_virtual ? DataLayout::virtual_call_type_data_tag : DataLayout::call_type_data_tag);
    //jcc(Assembler::notEqual, profile_continue);
    lb(AT, mdp, in_bytes(DataLayout::tag_offset()) - off_to_start);
    li(tmp, is_virtual ? DataLayout::virtual_call_type_data_tag : DataLayout::call_type_data_tag);
    bne(tmp, AT, profile_continue);
    nop();


    if (MethodData::profile_arguments()) {
      Label done;
      int off_to_args = in_bytes(TypeEntriesAtCall::args_data_offset());
      //addptr(mdp, off_to_args);
      if (Assembler::is_simm16(off_to_args)) {
        daddiu(mdp, mdp, off_to_args);
      } else {
        move(AT, off_to_args);
        daddu(mdp, mdp, AT);
      }


      for (int i = 0; i < TypeProfileArgsLimit; i++) {
        if (i > 0 || MethodData::profile_return()) {
          // If return value type is profiled we may have no argument to profile
          //movptr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())-off_to_args));
          ld(tmp, mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())-off_to_args);

          //subl(tmp, i*TypeStackSlotEntries::per_arg_count());
          if (Assembler::is_simm16(-1 * i * TypeStackSlotEntries::per_arg_count())) {
            addiu(tmp, tmp, -1 * i * TypeStackSlotEntries::per_arg_count());
          } else {
            li(AT, i*TypeStackSlotEntries::per_arg_count());
            subu(tmp, tmp, AT);
          }

          //cmpl(tmp, TypeStackSlotEntries::per_arg_count());
          //jcc(Assembler::less, done);
          li(AT, TypeStackSlotEntries::per_arg_count());
          slt(AT, tmp, AT);
          bne(AT, R0, done);
          nop();
        }
        //movptr(tmp, Address(callee, Method::const_offset()));
        ld(tmp, callee, in_bytes(Method::const_offset())); 

        //load_unsigned_short(tmp, Address(tmp, ConstMethod::size_of_parameters_offset()));
        lhu(tmp, tmp, in_bytes(ConstMethod::size_of_parameters_offset()));

        // stack offset o (zero based) from the start of the argument
        // list, for n arguments translates into offset n - o - 1 from
        // the end of the argument list
        //subptr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::stack_slot_offset(i))-off_to_args));
        ld(AT, mdp, in_bytes(TypeEntriesAtCall::stack_slot_offset(i))-off_to_args);
        subu(tmp, tmp, AT);

        //subl(tmp, 1);
        addiu(tmp, tmp, -1);

        Address arg_addr = argument_address(tmp);
        //movptr(tmp, arg_addr);
        ld(tmp, arg_addr);

        Address mdo_arg_addr(mdp, in_bytes(TypeEntriesAtCall::argument_type_offset(i))-off_to_args);
        profile_obj_type(tmp, mdo_arg_addr);

        int to_add = in_bytes(TypeStackSlotEntries::per_arg_size());
        //addptr(mdp, to_add);
        if (Assembler::is_simm16(to_add)) {
          daddiu(mdp, mdp, to_add);
        } else {
          move(AT, to_add);
          daddu(mdp, mdp, AT);
        }

        off_to_args += to_add;
      }

      if (MethodData::profile_return()) {
        //movptr(tmp, Address(mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())-off_to_args));
        ld(tmp, mdp, in_bytes(TypeEntriesAtCall::cell_count_offset())-off_to_args);

        //subl(tmp, TypeProfileArgsLimit*TypeStackSlotEntries::per_arg_count());
        int tmp_arg_counts = TypeProfileArgsLimit*TypeStackSlotEntries::per_arg_count();
        if (Assembler::is_simm16(-1 * tmp_arg_counts)) {
          addiu(tmp, tmp, -1 * tmp_arg_counts);
        } else {
          move(AT, tmp_arg_counts);
          subu(mdp, mdp, AT);
        }
      }

      bind(done);

      if (MethodData::profile_return()) {
        // We're right after the type profile for the last
        // argument. tmp is the number of cells left in the
        // CallTypeData/VirtualCallTypeData to reach its end. Non null
        // if there's a return to profile.
        assert(ReturnTypeEntry::static_cell_count() < TypeStackSlotEntries::per_arg_count(), "can't move past ret type");
        //shll(tmp, exact_log2(DataLayout::cell_size));
        //addptr(mdp, tmp);
        sll(tmp, tmp, exact_log2(DataLayout::cell_size));
        daddu(mdp, mdp, tmp);
      }
      //movptr(Address(rbp, frame::interpreter_frame_mdx_offset * wordSize), mdp);
      sd(mdp, FP, frame::interpreter_frame_mdx_offset * wordSize);
    } else {
      assert(MethodData::profile_return(), "either profile call args or call ret");
      update_mdp_by_constant(mdp, in_bytes(TypeEntriesAtCall::return_only_size()));
    }

    // mdp points right after the end of the
    // CallTypeData/VirtualCallTypeData, right after the cells for the
    // return value type if there's one

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_return_type(Register mdp, Register ret, Register tmp) {
  assert_different_registers(mdp, ret, tmp, _bcp_register);
  if (ProfileInterpreter && MethodData::profile_return()) {
    Label profile_continue, done;

    test_method_data_pointer(mdp, profile_continue);

    if (MethodData::profile_return_jsr292_only()) {
      // If we don't profile all invoke bytecodes we must make sure
      // it's a bytecode we indeed profile. We can't go back to the
      // begining of the ProfileData we intend to update to check its
      // type because we're right after it and we don't known its
      // length
      Label do_profile;
      //cmpb(Address(_bcp_register, 0), Bytecodes::_invokedynamic);
      //jcc(Assembler::equal, do_profile);
      lb(AT, _bcp_register, 0);
      daddiu(AT, AT, -1 * Bytecodes::_invokedynamic);
      beq(AT, R0, do_profile);
      nop();

      //cmpb(Address(_bcp_register, 0), Bytecodes::_invokehandle);
      //jcc(Assembler::equal, do_profile);
      lb(AT, _bcp_register, 0);
      daddiu(AT, AT, -1 * Bytecodes::_invokehandle);
      beq(AT, R0, do_profile);
      nop();
      
      get_method(tmp);
      //cmpb(Address(tmp, Method::intrinsic_id_offset_in_bytes()), vmIntrinsics::_compiledLambdaForm);
      //jcc(Assembler::notEqual, profile_continue);
      lb(tmp, tmp, Method::intrinsic_id_offset_in_bytes());
      li(AT, vmIntrinsics::_compiledLambdaForm);
      bne(tmp, AT, profile_continue);
      nop();

      bind(do_profile);
    }

    Address mdo_ret_addr(mdp, -in_bytes(ReturnTypeEntry::size()));
    //mov(tmp, ret);
    daddu(tmp, ret, R0);
    profile_obj_type(tmp, mdo_ret_addr);

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::profile_parameters_type(Register mdp, Register tmp1, Register tmp2) {
  guarantee(T9 == tmp1, "You are reqired to use T9 as the index register for MIPS !");

  if (ProfileInterpreter && MethodData::profile_parameters()) {
    Label profile_continue, done;

    test_method_data_pointer(mdp, profile_continue);

    // Load the offset of the area within the MDO used for
    // parameters. If it's negative we're not profiling any parameters
    //movl(tmp1, Address(mdp, in_bytes(MethodData::parameters_type_data_di_offset()) - in_bytes(MethodData::data_offset())));
    //testl(tmp1, tmp1);
    //jcc(Assembler::negative, profile_continue);
    lw(tmp1, mdp, in_bytes(MethodData::parameters_type_data_di_offset()) - in_bytes(MethodData::data_offset()));
    bltz(tmp1, profile_continue);
    nop();

    // Compute a pointer to the area for parameters from the offset
    // and move the pointer to the slot for the last
    // parameters. Collect profiling from last parameter down.
    // mdo start + parameters offset + array length - 1
    //addptr(mdp, tmp1);
    //movptr(tmp1, Address(mdp, ArrayData::array_len_offset()));
    daddu(mdp, mdp, tmp1);
    ld(tmp1, mdp, in_bytes(ArrayData::array_len_offset()));
    decrement(tmp1, TypeStackSlotEntries::per_arg_count());
    

    Label loop;
    bind(loop);

    int off_base = in_bytes(ParametersTypeData::stack_slot_offset(0));
    int type_base = in_bytes(ParametersTypeData::type_offset(0));
    Address::ScaleFactor per_arg_scale = Address::times(DataLayout::cell_size);
    //Address arg_off(mdp, tmp1, per_arg_scale, off_base);
    Address arg_type(mdp, tmp1, per_arg_scale, type_base);

    // load offset on the stack from the slot for this parameter
    //movptr(tmp2, arg_off);
    dsll(AT, tmp1, per_arg_scale);
    daddu(AT, AT, mdp);
    ld(tmp2, AT, off_base);

    //negptr(tmp2);
    subu(tmp2, R0, tmp2);

    // read the parameter from the local area
    //movptr(tmp2, Address(_locals_register, tmp2, Interpreter::stackElementScale()));
    dsll(AT, tmp2, Interpreter::stackElementScale());
    daddu(AT, AT, _locals_register);
    ld(tmp2, AT, 0);

    // profile the parameter
    profile_obj_type(tmp2, arg_type);

    // go to next parameter
    decrement(tmp1, TypeStackSlotEntries::per_arg_count());
    //jcc(Assembler::positive, loop);
    bgtz(tmp1, loop);
    nop();

    bind(profile_continue);
  }
}

void InterpreterMacroAssembler::verify_oop(Register reg, TosState state) {
  if (state == atos) {
    MacroAssembler::verify_oop(reg);
  }
}

void InterpreterMacroAssembler::verify_FPU(int stack_depth, TosState state) {
  // only if +VerifyFPU  && (state == ftos || state == dtos)
  // For now, do nothing.
}
#endif // !CC_INTERP


//FIXME, aoqi:see UltraViolet
void InterpreterMacroAssembler::notify_method_entry() {
  // Whenever JVMTI is interp_only_mode, method entry/exit events are sent to
  // track stack depth.  If it is possible to enter interp_only_mode we add
  // the code to check if the event should be sent.
  //Register tempreg = Rscratch0;
  Register tempreg = T0;
  if (JvmtiExport::can_post_interpreter_events()) {
    Label L;
    //movl(rdx, Address(r15_thread, JavaThread::interp_only_mode_offset()));
    //testl(rdx, rdx);
    //jcc(Assembler::zero, L);
    //lw(tempreg, in_bytes(JavaThread::interp_only_mode_offset()), Rthread);
    get_thread(AT);
    lw(tempreg, AT, in_bytes(JavaThread::interp_only_mode_offset()));
    beq(tempreg, R0, L);
    delayed()->nop();
    call_VM(noreg, CAST_FROM_FN_PTR(address,
                                    InterpreterRuntime::post_method_entry));
    bind(L);
  }

  {
    //SkipIfEqual skip_if(this, tempreg, R0, &DTraceMethodProbes, 1);
    SkipIfEqual skip_if(this, &DTraceMethodProbes, 0);
    call_VM_leaf(CAST_FROM_FN_PTR(address, SharedRuntime::dtrace_method_entry),
		 //Rthread,
		 AT,
		 //Rmethod);
		 S3);
  }

}

//FIXME, aoqi:see UltraViolet
void InterpreterMacroAssembler::notify_method_exit(
    //TosState state, NotifyMethodExitMode mode) {
    bool is_native_method, TosState state, NotifyMethodExitMode mode) {
  // Whenever JVMTI is interp_only_mode, method entry/exit events are sent to
  // track stack depth.  If it is possible to enter interp_only_mode we add
  // the code to check if the event should be sent.
  //Register tempreg = Rscratch0;
  Register tempreg = T0;
  if (mode == NotifyJVMTI && JvmtiExport::can_post_interpreter_events()) {
    Label skip;
    //lw(tempreg, in_bytes(JavaThread::interp_only_mode_offset()), Rthread);
    get_thread(AT);
    lw(tempreg, AT, in_bytes(JavaThread::interp_only_mode_offset()));
    beq(tempreg, R0, skip);
    delayed()->nop();
    // Note: frame::interpreter_frame_result has a dependency on how the
    // method result is saved across the call to post_method_exit. If this
    // is changed then the interpreter_frame_result implementation will
    // need to be updated too.

    // For c++ interpreter the result is always stored at a known location in the frame
    // template interpreter will leave it on the top of the stack.
    save_return_value(state, is_native_method);
    call_VM(noreg,
            CAST_FROM_FN_PTR(address, InterpreterRuntime::post_method_exit));
    restore_return_value(state, is_native_method);
    bind(skip);
  }

  {
    // Dtrace notification
    //SkipIfEqual skip_if(this, tempreg, R0, &DTraceMethodProbes, equal);
    SkipIfEqual skip_if(this, &DTraceMethodProbes, 0);
    save_return_value(state, is_native_method);
    call_VM_leaf(
		 CAST_FROM_FN_PTR(address, SharedRuntime::dtrace_method_exit),
		 //Rthread, Rmethod);
		 AT, S3);
    restore_return_value(state, is_native_method);
  }
}

//FIXME  yyq native return 64 bits
void InterpreterMacroAssembler::save_return_value(
    TosState state, bool is_native_call) {
  if (is_native_call) {
    // save any potential method result value
    //sd(V0, frame::interpreter_frame_l_scratch_offset * wordSize, FP);
    //sdc1(F0, frame::interpreter_frame_d_scratch_offset * wordSize, FP);
    sw(V0, FP, (-9) * wordSize);
    swc1(F0, FP, (-10) * wordSize);

//    sd(V0, FP, (-9) * wordSize);
//    sdc1(F0, FP, (-10) * wordSize);
  } else {
    push(state);
  }
}

//FIXME  yyq native return 64 bits
void InterpreterMacroAssembler::restore_return_value(
    TosState state, bool is_native_call) {
  if (is_native_call) {
    // Restore any method result value
    //ld(V0, frame::interpreter_frame_l_scratch_offset * wordSize, FP);
    //ldc1(F0, frame::interpreter_frame_d_scratch_offset * wordSize, FP);
    lw(V0, FP, (-9) * wordSize);
    lwc1(F0, FP, (-10) * wordSize);
  } else {
    pop(state);
  }
}
