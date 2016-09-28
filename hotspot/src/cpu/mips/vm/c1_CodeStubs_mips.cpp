/*
 * Copyright (c) 1999, 2013, Oracle and/or its affiliates. All rights reserved.
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
#include "c1/c1_CodeStubs.hpp"
#include "c1/c1_FrameMap.hpp"
#include "c1/c1_LIRAssembler.hpp"
#include "c1/c1_MacroAssembler.hpp"
#include "c1/c1_Runtime1.hpp"
#include "nativeInst_mips.hpp"
#include "runtime/sharedRuntime.hpp"
#include "vmreg_mips.inline.hpp"
#ifndef SERIALGC
#include "gc_implementation/g1/g1SATBCardTableModRefBS.hpp"
#endif


#define __ ce->masm()->

float ConversionStub::float_zero = 0.0;
double ConversionStub::double_zero = 0.0;

void ConversionStub::emit_code(LIR_Assembler* ce) {
	__ bind(_entry);
	assert(bytecode() == Bytecodes::_f2i || bytecode() == Bytecodes::_d2i, "other conversions do not require stub");
}

#ifdef TIERED
void CounterOverflowStub::emit_code(LIR_Assembler* ce) {
	__ bind(_entry);
	ce->store_parameter(_bci, 0);
	//__ call(RuntimeAddress(Runtime1::entry_for(Runtime1::counter_overflow_id)));
	__ call(Runtime1::entry_for(Runtime1::counter_overflow_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	ce->verify_oop_map(_info);

	//__ jmp(_continuation);
	__ b_far(_continuation);
	__ delayed()->nop();
}
#endif // TIERED



RangeCheckStub::RangeCheckStub(CodeEmitInfo* info, LIR_Opr index,
		bool throw_index_out_of_bounds_exception)
	: _throw_index_out_of_bounds_exception(throw_index_out_of_bounds_exception)
	  , _index(index)
{
	_info = info == NULL ? NULL : new CodeEmitInfo(info);
}


void RangeCheckStub::emit_code(LIR_Assembler* ce) {
#ifdef OPT_RANGECHECK
	if (_throw_pc != -1) {
		ce->compilation()->null_check_table()->append(_throw_pc, __ offset());
	}
#endif
	__ bind(_entry);
	//// Pass the array index in eax since the runtime stub will add register state to the stack
	// pass the array index on stack because all registers must be preserved

	if (_index->is_cpu_register()) {
		ce->store_parameter(_index->as_register(), 0);
	} else {
		ce->store_parameter(_index->as_jint(), 0);
	}

	if (_throw_index_out_of_bounds_exception) {
		__ call(Runtime1::entry_for(Runtime1::throw_index_exception_id), relocInfo::runtime_call_type);
	} else {
		__ call(Runtime1::entry_for(Runtime1::throw_range_check_failed_id), relocInfo::runtime_call_type);
	}
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	debug_only(__ should_not_reach_here());	
}

PredicateFailedStub::PredicateFailedStub(CodeEmitInfo* info) {   // Fu:20130814
	_info = new CodeEmitInfo(info);
}

void PredicateFailedStub::emit_code(LIR_Assembler* ce) {         // Fu:20130814
	tty->print_cr("PredicateFailedStub::emit_code unimplemented yet!");
	Unimplemented();
}

void DivByZeroStub::emit_code(LIR_Assembler* ce) {
	if (_offset != -1) {
		//		ce->compilation()->null_check_table()->append(_offset, __ offset());
		ce->compilation()->implicit_exception_table()->append(_offset, __ offset());
	}
	__ bind(_entry);
	__ call(Runtime1::entry_for(Runtime1::throw_div0_exception_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	debug_only(__ should_not_reach_here());

}


// Implementation of NewInstanceStub

NewInstanceStub::NewInstanceStub(LIR_Opr klass_reg, LIR_Opr result, ciInstanceKlass* klass, CodeEmitInfo* info, Runtime1::StubID stub_id) {
	_result = result;
	_klass = klass;
	_klass_reg = klass_reg;
	_info = new CodeEmitInfo(info);
	assert(stub_id == Runtime1::new_instance_id                 ||
			stub_id == Runtime1::fast_new_instance_id            ||
			stub_id == Runtime1::fast_new_instance_init_check_id,
			"need new_instance id");
	_stub_id   = stub_id;
}

// i use T4 as klass register, V0 as result register. MUST accord with Runtime1::generate_code_for.
void NewInstanceStub::emit_code(LIR_Assembler* ce) {
	assert(__ sp_offset() == 0, "frame size should be fixed");
	__ bind(_entry);
	//__ movptr(rdx, _klass_reg->as_register());
	//__ call(RuntimeAddress(Runtime1::entry_for(_stub_id)));
#ifndef _LP64
	assert(_klass_reg->as_register() == T4, "klass_reg must in T4");
#else
	//FIXME. in A4? aoqi
	assert(_klass_reg->as_register() == A4, "klass_reg must in A4");
#endif
	

	__ call(Runtime1::entry_for(_stub_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	ce->verify_oop_map(_info);
	assert(_result->as_register() == V0, "result must in V0,");
	__ b_far(_continuation);
	__ delayed()->nop();
}


// Implementation of NewTypeArrayStub

NewTypeArrayStub::NewTypeArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info) {
	_klass_reg = klass_reg;
	_length = length;
	_result = result;
	_info = new CodeEmitInfo(info);
}

// i use T2 as length register, T4 as klass register, V0 as result register. 
// MUST accord with Runtime1::generate_code_for
void NewTypeArrayStub::emit_code(LIR_Assembler* ce) {
	assert(__ sp_offset() == 0, "frame size should be fixed");
	__ bind(_entry);
	assert(_length->as_register() == T2, "length must in T2,");
#ifndef _LP64
	assert(_klass_reg->as_register() == T4, "klass_reg must in T4");
#else
	//FIXME. in A4? aoqi
	assert(_klass_reg->as_register() == A4, "klass_reg must in A4");
#endif

	//__ call(RuntimeAddress(Runtime1::entry_for(Runtime1::new_type_array_id)));
	__ call(Runtime1::entry_for(Runtime1::new_type_array_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	ce->verify_oop_map(_info);

	assert(_result->as_register() == V0, "result must in V0,");
	__ b_far(_continuation);
	__ delayed()->nop();
}


// Implementation of NewObjectArrayStub

NewObjectArrayStub::NewObjectArrayStub(LIR_Opr klass_reg, LIR_Opr length, LIR_Opr result, CodeEmitInfo* info) {
	_klass_reg = klass_reg;
	_result = result;
	_length = length;
	_info = new CodeEmitInfo(info);
}


void NewObjectArrayStub::emit_code(LIR_Assembler* ce) {
	assert(__ sp_offset() == 0, "frame size should be fixed");
	__ bind(_entry);
	//assert(_length->as_register() == rbx, "length must in rbx,");
	//assert(_klass_reg->as_register() == rdx, "klass_reg must in rdx");
	//__ call(RuntimeAddress(Runtime1::entry_for(Runtime1::new_object_array_id)));
	assert(_length->as_register() == T2, "length must in ebx");
#ifndef _LP64
	assert(_klass_reg->as_register() == T4, "klass_reg must in T4");
#else
	//FIXME. in A4? aoqi
	assert(_klass_reg->as_register() == A4, "klass_reg must in A4");
#endif
	__ call(Runtime1::entry_for(Runtime1::new_object_array_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	ce->verify_oop_map(_info);
	//assert(_result->as_register() == rax, "result must in rax,");
	//__ jmp(_continuation);
	assert(_result->as_register() == V0, "result must in eax");
	__ b_far(_continuation);
	__ delayed()->nop();
}


// Implementation of MonitorAccessStubs

MonitorEnterStub::MonitorEnterStub(LIR_Opr obj_reg, LIR_Opr lock_reg, CodeEmitInfo* info)
: MonitorAccessStub(obj_reg, lock_reg)
{
	_info = new CodeEmitInfo(info);
}


void MonitorEnterStub::emit_code(LIR_Assembler* ce) {
	assert(__ sp_offset() == 0, "frame size should be fixed");
	__ bind(_entry);
	ce->store_parameter(_obj_reg->as_register(),  1);
	ce->store_parameter(_lock_reg->is_single_cpu()? _lock_reg->as_register() : _lock_reg->as_register_lo(), 0);
	/*
	   Runtime1::StubID enter_id;
	   if (ce->compilation()->has_fpu_code()) {
	   enter_id = Runtime1::monitorenter_id;
	   } else {
	   enter_id = Runtime1::monitorenter_nofpu_id;
	   }
	   __ call(RuntimeAddress(Runtime1::entry_for(enter_id)));
	 */
	if (ce->compilation()->has_fpu_code()) {
		__ call(Runtime1::entry_for(Runtime1::monitorenter_id), relocInfo::runtime_call_type);
	} else {
		__ call(Runtime1::entry_for(Runtime1::monitorenter_nofpu_id), relocInfo::runtime_call_type);
	}
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	ce->verify_oop_map(_info);
	//__ jmp(_continuation);
	__ b_far(_continuation);
	__ delayed()->nop();
}


void MonitorExitStub::emit_code(LIR_Assembler* ce) {
	__ bind(_entry);
	if (_compute_lock) {
		// lock_reg was destroyed by fast unlocking attempt => recompute it
		ce->monitor_address(_monitor_ix, _lock_reg);
	}
	ce->store_parameter(_lock_reg->as_register(), 0);
	// note: non-blocking leaf routine => no call info needed
	/*
	   Runtime1::StubID exit_id;
	   if (ce->compilation()->has_fpu_code()) {
	   exit_id = Runtime1::monitorexit_id;
	   } else {
	   exit_id = Runtime1::monitorexit_nofpu_id;
	   }
	   __ call(RuntimeAddress(Runtime1::entry_for(exit_id)));
	   __ jmp(_continuation);
	 */
	if (ce->compilation()->has_fpu_code()) {
		__ call(Runtime1::entry_for(Runtime1::monitorexit_id), relocInfo::runtime_call_type);
	} else {
		__ call(Runtime1::entry_for(Runtime1::monitorexit_nofpu_id), relocInfo::runtime_call_type);
	}
	__ delayed()->nop();

	//__ jmp(_continuation);
	__ b_far(_continuation);
	__ delayed()->nop();
}


// Implementation of patching:
// - Copy the code at given offset to an inlined buffer (first the bytes, then the number of bytes)
// - Replace original code with a call to the stub
// At Runtime:
// - call to stub, jump to runtime
// - in runtime: preserve all registers (especially objects, i.e., source and destination object)
// - in runtime: after initializing class, restore original code, reexecute instruction

//int PatchingStub::_patch_info_offset = -NativeGeneralJump::instruction_size;
int PatchingStub::_patch_info_offset = -NativeCall::instruction_size;

void PatchingStub::align_patch_site(MacroAssembler* masm) {
	// We're patching a 5-7 byte instruction on intel and we need to
	// make sure that we don't see a piece of the instruction.  It
	// appears mostly impossible on Intel to simply invalidate other
	// processors caches and since they may do aggressive prefetch it's
	// very hard to make a guess about what code might be in the icache.
	// Force the instruction to be double word aligned so that it
	// doesn't span a cache line.

	// the NativeJump is not finished, i am not sure what to do here. FIXME
	//masm->align(round_to(NativeGeneralJump::instruction_size, wordSize));
}

void PatchingStub::emit_code(LIR_Assembler* ce) {
  assert(NativeCall::instruction_size <= _bytes_to_copy && _bytes_to_copy <= 0xFF, "not enough room for call");
  assert(_bytes_to_copy <= 0xFF, "not enough room for call");

  Label call_patch;

  // static field accesses have special semantics while the class
  // initializer is being run so we emit a test which can be used to
  // check that this code is being executed by the initializing
  // thread.
  address being_initialized_entry = __ pc();
  if (CommentedAssembly) {
    __ block_comment(" patch template");
  }
  if (_id == load_klass_id) {
    // produce a copy of the load klass instruction for use by the being initialized case
    address start = __ pc();
    jobject o = NULL;
    int oop_index = __ oop_recorder()->allocate_oop_index(o);
    RelocationHolder rspec = oop_Relocation::spec(oop_index);
    __ relocate(rspec);
#ifndef _LP64
    //by_css
    __ lui(_obj, Assembler::split_high((int)o));
    __ addiu(_obj, _obj, Assembler::split_low((int)o));
#else
    //This should be same as jobject2reg_with_patching.
    __ li48(_obj, (long)o);
#endif
    while ((intx)__ pc() - (intx)start < NativeCall::instruction_size) {
      __ nop();
    }
#ifdef ASSERT
    for (int i = 0; i < _bytes_to_copy; i++) {
      address ptr = (address)(_pc_start + i);
      int a_byte = (*ptr) & 0xFF;
      assert(a_byte == *start++, "should be the same code");
    }
#endif
  } else {

    // make a copy the code which is going to be patched.
    assert((_bytes_to_copy&3)==0, "change this code");
    address start = __ pc();
    for ( int i = 0; i < _bytes_to_copy; i+=4) {
      __ emit_int32(*(int*)(_pc_start + i));
      //make the site look like a nop, @jerome 
      *(int*)(_pc_start + i)=0;
    }
    while ((intx)__ pc() - (intx)start < NativeCall::instruction_size) {
      __ nop();
    }
  }

  address end_of_patch = __ pc();
  int bytes_to_skip = 0;
  if (_id == load_klass_id) {
    int offset = __ offset();
    if (CommentedAssembly) {
      __ block_comment(" being_initialized check");
    }
    assert(_obj != NOREG, "must be a valid register");
#ifndef OPT_THREAD
    //FIXME, T8 need be saved ?
    Register thread = T8;
    __ get_thread(thread);
#else
    Register thread = TREG;
#endif
    __ ld(AT, _obj, in_bytes(InstanceKlass::init_thread_offset()));
    __ bne(thread, AT, call_patch);
    __ delayed()->nop();

    // access_field patches may execute the patched code before it's
    // copied back into place so we need to jump back into the main
    // code of the nmethod to continue execution.
    /*		address temppc = __ pc();
		__ b(_patch_site_continuation);
		__ delayed()->nop();
		bytes_to_skip += (__ pc() - temppc);
     */ 
    __ b_far(_patch_site_continuation);
    __ delayed()->nop();
    bytes_to_skip += __ offset() - offset;

  }

  if (CommentedAssembly) {
    __ block_comment("patch data");
  }
  // Now emit the patch record telling the runtime how to find the
  // pieces of the patch.  We only need 3 bytes but for alignment, we 
  // need 4 bytes
  int sizeof_patch_record = 4;
  bytes_to_skip += sizeof_patch_record;

  // emit the offsets needed to find the code to patch
  int being_initialized_entry_offset = __ pc() - being_initialized_entry + patch_info_size;

#ifdef _LP64
  /* Jin: In MIPS64, byte_skip is much larger than that in X86. It can not be contained in a byte:
   *   - bytes_to_skip = 0x190;
   *   - _bytes_to_copy = 0x20;
   *   - being_initialized_entry_offset = 0x1b0;
   * 
   *   To minimize the modification of share codes, the values are decreased 4 times when generated,
   *   thus can be packed into a long type.
   *
   *   See [share/vm/c1/c1_Runtime1.cpp 918] Runtime1::patch_code()
   */
  being_initialized_entry_offset /= 4;
  _bytes_to_copy /= 4;
  bytes_to_skip /= 4;
#endif
  // patch_info_pc offset | size of b instruction(8)| patched code size
  assert((char)being_initialized_entry_offset==being_initialized_entry_offset, "just check");
  assert((char)bytes_to_skip==bytes_to_skip, "just check");
  assert((char)_bytes_to_copy==_bytes_to_copy, "just check");
  __ emit_int32(being_initialized_entry_offset<<8 | (bytes_to_skip<<16) | (_bytes_to_copy<<24) );

  address patch_info_pc = __ pc();
#ifdef _LP64
  assert(patch_info_pc - end_of_patch == bytes_to_skip * 4, "incorrect patch info");
#else
  assert(patch_info_pc - end_of_patch == bytes_to_skip, "incorrect patch info");
#endif

  address entry = __ pc();
  NativeGeneralJump::insert_unconditional((address)_pc_start, entry);
  address target = NULL;
  switch (_id) {
    case access_field_id:  target = Runtime1::entry_for(Runtime1::access_field_patching_id); break;
    case load_klass_id:    target = Runtime1::entry_for(Runtime1::load_klass_patching_id); break;
    default: ShouldNotReachHere();
  }
  __ bind(call_patch);


  if (CommentedAssembly) {
    __ block_comment("patch entry point");
  }
  //__ call(RuntimeAddress(target));
#ifndef _LP64 
  //by_css
  __ lui(T9, Assembler::split_high((int)target));
  __ addiu(T9, T9, Assembler::split_low((int)target));
#else
  __ li48(T9, (long)target);
#endif
  __ jalr(T9);
  __ delayed()->nop();
  assert(_patch_info_offset == (patch_info_pc - __ pc()), "must not change");
  ce->add_call_info_here(_info);
  int jmp_off = __ offset();
  __ b_far(_patch_site_entry);
  __ delayed()->nop();
  // Add enough nops so deoptimization can overwrite the jmp above with a call
  // and not destroy the world.
  for (int j = __ offset(); j < jmp_off + NativeCall::instruction_size; j += 4 ) {
    __ nop();
  }
  if (_id == load_klass_id) {
    CodeSection* cs = __ code_section();
    address pc = (address)_pc_start;  
    RelocIterator iter(cs, pc, pc + 1);
    relocInfo::change_reloc_info_for_address(&iter, pc, relocInfo::oop_type, relocInfo::none);
  }
}


void ImplicitNullCheckStub::emit_code(LIR_Assembler* ce) {
	ce->compilation()->implicit_exception_table()->append(_offset, __ offset());
	__ bind(_entry);
	__ call(Runtime1::entry_for(Runtime1::throw_null_pointer_exception_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	debug_only(__ should_not_reach_here());
}


// i dont know which register to use here, i just assume A1 here. FIXME
void SimpleExceptionStub::emit_code(LIR_Assembler* ce) {
	assert(__ sp_offset() == 0, "frame size should be fixed");

	__ bind(_entry);
	// pass the object on stack because all registers must be preserved
	if (_obj->is_cpu_register()) {
		ce->store_parameter(_obj->as_register(), 0);
	}
	__ call(Runtime1::entry_for(_stub), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	debug_only(__ should_not_reach_here());
}

/*
ArrayStoreExceptionStub::ArrayStoreExceptionStub(CodeEmitInfo* info):
	_info(info) {
	}


void ArrayStoreExceptionStub::emit_code(LIR_Assembler* ce) {
	assert(__ sp_offset() == 0, "frame size should be fixed");
	__ bind(_entry);
	//__ call(RuntimeAddress(Runtime1::entry_for(Runtime1::throw_array_store_exception_id)));
	__ call(Runtime1::entry_for(Runtime1::throw_array_store_exception_id), relocInfo::runtime_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(_info);
	debug_only(__ should_not_reach_here());
}

*/
void ArrayCopyStub::emit_code(LIR_Assembler* ce) {
	//---------------slow case: call to native-----------------
	__ bind(_entry);
	// Figure out where the args should go
	// This should really convert the IntrinsicID to the methodOop and signature
	// but I don't know how to do that.
	//
	VMRegPair args[5];
	BasicType signature[5] = { T_OBJECT, T_INT, T_OBJECT, T_INT, T_INT};
	SharedRuntime::java_calling_convention(signature, args, 5, true);

	// push parameters
	// (src, src_pos, dest, destPos, length)
	Register r[5];
	r[0] = src()->as_register();
	r[1] = src_pos()->as_register();
	r[2] = dst()->as_register();
	r[3] = dst_pos()->as_register();
	r[4] = length()->as_register();

	// next registers will get stored on the stack
	for (int i = 0; i < 5 ; i++ ) {
		VMReg r_1 = args[i].first();
		if (r_1->is_stack()) {
			int st_off = r_1->reg2stack() * wordSize;
			//__ movptr (Address(rsp, st_off), r[i]);
			__ sw( r[i],  SP, st_off); 
		} else {
			assert(r[i] == args[i].first()->as_Register(), "Wrong register for arg ");
		}
	}

	ce->align_call(lir_static_call);

	ce->emit_static_call_stub();
	//AddressLiteral resolve(SharedRuntime::get_resolve_static_call_stub(),
	//                       relocInfo::static_call_type);
	//__ call(resolve);
	__ call(SharedRuntime::get_resolve_static_call_stub(), relocInfo::static_call_type);
	__ delayed()->nop();
	ce->add_call_info_here(info());

#ifndef PRODUCT
	//__ incrementl(ExternalAddress((address)&Runtime1::_arraycopy_slowcase_cnt));
#ifndef _LP64
	__ lui(T8, Assembler::split_high((int)&Runtime1::_arraycopy_slowcase_cnt));
	__ lw(AT, T8, Assembler::split_low((int)&Runtime1::_arraycopy_slowcase_cnt));
	__ addiu(AT, AT, 1);
	__ sw(AT, T8, Assembler::split_low((int)&Runtime1::_arraycopy_slowcase_cnt));
#else
	__ li(T8, (long)&Runtime1::_arraycopy_slowcase_cnt);
	__ lw(AT, T8, 0);
	__ daddiu(AT, AT, 1);
	__ sw(AT, T8, 0); 
#endif
#endif

	__ b_far(_continuation);
	__ delayed()->nop();
}

/////////////////////////////////////////////////////////////////////////////
#ifndef SERIALGC

void G1PreBarrierStub::emit_code(LIR_Assembler* ce) {
	Unimplemented();
}
/*
   jbyte* G1PostBarrierStub::_byte_map_base = NULL;

   jbyte* G1PostBarrierStub::byte_map_base_slow() {
   BarrierSet* bs = Universe::heap()->barrier_set();
   assert(bs->is_a(BarrierSet::G1SATBCTLogging),
   "Must be if we're using this.");
   return ((G1SATBCardTableModRefBS*)bs)->byte_map_base;
   }
 */
void G1PostBarrierStub::emit_code(LIR_Assembler* ce) {
	Unimplemented();
}

#endif // SERIALGC
/////////////////////////////////////////////////////////////////////////////

#undef __
