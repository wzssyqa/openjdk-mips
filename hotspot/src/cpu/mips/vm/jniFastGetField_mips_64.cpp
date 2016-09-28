/*
 * Copyright (c) 2004, 2010, Oracle and/or its affiliates. All rights reserved.
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
#include "memory/resourceArea.hpp"
#include "prims/jniFastGetField.hpp"
#include "prims/jvm_misc.hpp"
#include "runtime/safepoint.hpp"

#define __ masm->

#define BUFFER_SIZE 30*wordSize

// Instead of issuing lfence for LoadLoad barrier, we create data dependency
// between loads, which is more efficient than lfence.

address JNI_FastGetField::generate_fast_get_int_field0(BasicType type) {
  const char *name;
  switch (type) {
    case T_BOOLEAN: name = "jni_fast_GetBooleanField"; break;
    case T_BYTE:    name = "jni_fast_GetByteField";    break;
    case T_CHAR:    name = "jni_fast_GetCharField";    break;
    case T_SHORT:   name = "jni_fast_GetShortField";   break;
    case T_INT:     name = "jni_fast_GetIntField";     break;
    case T_LONG:    name = "jni_fast_GetLongField";    break;
    default:        ShouldNotReachHere();
  }
  ResourceMark rm;
  BufferBlob* blob = BufferBlob::create(name, BUFFER_SIZE);
    CodeBuffer cbuf(blob);
  MacroAssembler* masm = new MacroAssembler(&cbuf);
  address fast_entry = __ pc();

  Label slow;

  //  return pc        RA
  //  jni env          A0
  //  obj              A1
  //  jfieldID         A2

  address counter_addr = SafepointSynchronize::safepoint_counter_addr();
  __ li48(AT, (long)counter_addr);
  __ lw(T1, AT, 0);

  /* 2012/4/28 Jin: the parameters(A0~A3) should not be modified, since
   *   they will be used in slow path. */
  __ andi(AT, T1, 1);
  __ bne(AT, R0, slow);
  __ delayed()->nop();

  __ ld(T0, A1, 0);              // unbox, *obj 
  __ move(T2, A2);
  __ shr(T2, 2);                 // offset
  __ dadd(T0, T0, T2);

  assert(count < LIST_CAPACITY, "LIST_CAPACITY too small");
  speculative_load_pclist[count] = __ pc();
  switch (type) {
    case T_BOOLEAN: __ lbu (V0, T0, 0); break;
    case T_BYTE:    __ lb  (V0, T0, 0); break;
    case T_CHAR:    __ lhu (V0, T0, 0); break;
    case T_SHORT:   __ lh  (V0, T0, 0); break;
    case T_INT:     __ lw  (V0, T0, 0); break;
    case T_LONG:    __ ld  (V0, T0, 0); break;
    default:        ShouldNotReachHere();
  }

  __ li48(AT, (long)counter_addr);
  __ lw(AT, AT, 0);
  __ bne(T1, AT, slow);
  __ delayed()->nop();

  __ jr(RA);
  __ delayed()->nop();

  slowcase_entry_pclist[count++] = __ pc();
  __ bind (slow);
  address slow_case_addr;
  switch (type) {
    case T_BOOLEAN: slow_case_addr = jni_GetBooleanField_addr(); break;
    case T_BYTE:    slow_case_addr = jni_GetByteField_addr();    break;
    case T_CHAR:    slow_case_addr = jni_GetCharField_addr();    break;
    case T_SHORT:   slow_case_addr = jni_GetShortField_addr();   break;
    case T_INT:     slow_case_addr = jni_GetIntField_addr();     break;
    case T_LONG:    slow_case_addr = jni_GetLongField_addr();
  }
  __ jmp(slow_case_addr);
  __ delayed()->nop();

  __ flush ();

  return fast_entry;
}

address JNI_FastGetField::generate_fast_get_boolean_field() {
  return generate_fast_get_int_field0(T_BOOLEAN);
}

address JNI_FastGetField::generate_fast_get_byte_field() {
  return generate_fast_get_int_field0(T_BYTE);
}

address JNI_FastGetField::generate_fast_get_char_field() {
  return generate_fast_get_int_field0(T_CHAR);
}

address JNI_FastGetField::generate_fast_get_short_field() {
  return generate_fast_get_int_field0(T_SHORT);
}

address JNI_FastGetField::generate_fast_get_int_field() {
  return generate_fast_get_int_field0(T_INT);
}

address JNI_FastGetField::generate_fast_get_long_field() {
  return generate_fast_get_int_field0(T_LONG);
/*
  const char *name = "jni_fast_GetLongField";
  ResourceMark rm;
  BufferBlob* b = BufferBlob::create(name, BUFFER_SIZE*wordSize);
  address fast_entry = b->instructions_begin();
  // CodeBuffer* cbuf = new CodeBuffer(fast_entry, b->instructions_size());
  CodeBuffer  cbuf (fast_entry, b->instructions_size());
  MacroAssembler* masm = new MacroAssembler(&cbuf);

  Label slow;

  //  return pc        RA
  //  jni env          A0
  //  obj              A1
  //  jfieldID         A2

  address counter_addr = SafepointSynchronize::safepoint_counter_addr();
  //__ move(AT, (int)counter_addr);
  //__ lw(T1, AT, 0);
  __ lui(AT, Assembler::split_high((intptr_t)counter_addr));
  __ lw(T1, AT, Assembler::split_low((intptr_t)counter_addr));
  __ andi(AT, T1, 1);	
  __ bne(AT, R0, slow);
  __ delayed()->nop();

  __ ld (A1, A1, 0);              // unbox, *obj
  __ shr(A2, 2);              		// offset
  __ dadd(A1, A1, A2);

  assert(count < LIST_CAPACITY-1, "LIST_CAPACITY too small");
  speculative_load_pclist[count++] = __ pc();
  __ ld(V0, A1, 0);							// eax
  speculative_load_pclist[count] = __ pc();
  __ ld(V1, A1, 8);

  __ lui(AT, Assembler::split_high((intptr_t)counter_addr));
  __ lw(AT, AT, Assembler::split_low((intptr_t)counter_addr));
  __ bne(T1, AT, slow);
  __ delayed()->nop();

  __ jr(RA);
  __ delayed()->nop();

  slowcase_entry_pclist[count-1] = __ pc();
  slowcase_entry_pclist[count++] = __ pc();
  __ bind (slow);
  address slow_case_addr = jni_GetLongField_addr();;
  // tail call
  __ jmp(slow_case_addr);
  __ delayed()->nop();

  __ flush();
  return fast_entry;
  */
}

address JNI_FastGetField::generate_fast_get_float_field0(BasicType type) {
  const char *name;
  switch (type) {
    case T_FLOAT:     name = "jni_fast_GetFloatField";     break;
    case T_DOUBLE:    name = "jni_fast_GetDoubleField";    break;
    default:          ShouldNotReachHere();
  }
  ResourceMark rm;
  BufferBlob* blob = BufferBlob::create(name, BUFFER_SIZE);
    CodeBuffer cbuf(blob);
  MacroAssembler* masm = new MacroAssembler(&cbuf);
address fast_entry = __ pc();

  Label slow;

	//  return pc        RA
	//  jni env          A0
	//  obj              A1
	//  jfieldID         A2

	address counter_addr = SafepointSynchronize::safepoint_counter_addr();
	//__ move(AT, (int)counter_addr);
	//__ lw(T1, AT, 0);
#ifdef _LP64
	__ li48(AT, (intptr_t)counter_addr);
	__ lw(T1, AT, 0);
#else
	__ lui(AT, Assembler::split_high((intptr_t)counter_addr));
	__ lw(T1, AT, Assembler::split_low((intptr_t)counter_addr));
#endif
	__ andi(AT, T1, 1);
	__ bne(AT, R0, slow);
	__ delayed()->nop();	

#ifdef _LP64
	__ ld(A1, A1, 0);              // unbox, *obj
#else
	__ lw(A1, A1, 0);              // unbox, *obj
#endif
	__ shr(A2, 2);             		 // offset
	__ add(A1, A1, A2);

	assert(count < LIST_CAPACITY, "LIST_CAPACITY too small");
	speculative_load_pclist[count] = __ pc();
	switch (type) {
		case T_FLOAT:  
			__ lwc1(F0, A1, 0);
			break;
		case T_DOUBLE: 
#ifdef _LP64
			__ ldc1(F0, A1, 0);
#else
			__ lwc1(F0, A1, 0);
			__ lwc1(F1, A1, 4);
#endif
			break;
		default:       ShouldNotReachHere();
	}

#ifdef _LP64
	__ li48(AT, (intptr_t)counter_addr);
	__ lw(AT, AT, 0);
#else
	__ lui(AT, Assembler::split_high((intptr_t)counter_addr));
	__ lw(AT, AT, Assembler::split_low((intptr_t)counter_addr));
#endif
	__ bne(T1, AT, slow);
	__ delayed()->nop();

	__ jr(RA);
	__ delayed()->nop();


	slowcase_entry_pclist[count++] = __ pc();
	__ bind (slow);
	address slow_case_addr;
	switch (type) {
		case T_FLOAT:  slow_case_addr = jni_GetFloatField_addr();  break;
		case T_DOUBLE: slow_case_addr = jni_GetDoubleField_addr(); break;
		default:       ShouldNotReachHere();
	}
	__ jmp(slow_case_addr);
	__ delayed()->nop();

	__ flush ();
	return fast_entry;
}

address JNI_FastGetField::generate_fast_get_float_field() {
  return generate_fast_get_float_field0(T_FLOAT);
}

address JNI_FastGetField::generate_fast_get_double_field() {
  return generate_fast_get_float_field0(T_DOUBLE);
}
