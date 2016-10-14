/*
 * Copyright (c) 1997, 2013, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_MIPS_VM_MACROASSEMBLER_MIPS_HPP
#define CPU_MIPS_VM_MACROASSEMBLER_MIPS_HPP

// MacroAssembler extends Assembler by frequently used macros.
//
// Instructions for which a 'better' code sequence exists depending
// on arguments should also go in here.

class MacroAssembler: public Assembler {
  friend class LIR_Assembler;
  friend class Runtime1;      // as_Address()

public:
static intptr_t	i[32];
static float	f[32];
static void print(outputStream *s);

static int i_offset(unsigned int k);
static int f_offset(unsigned int k);
	
static void save_registers(MacroAssembler *masm);
static void restore_registers(MacroAssembler *masm);

 protected:

  Address as_Address(AddressLiteral adr);
  Address as_Address(ArrayAddress adr);

  // Support for VM calls
  //
  // This is the base routine called by the different versions of call_VM_leaf. The interpreter
  // may customize this version by overriding it for its purposes (e.g., to save/restore
  // additional registers when doing a VM call).
#ifdef CC_INTERP
  // c++ interpreter never wants to use interp_masm version of call_VM
  #define VIRTUAL
#else
  #define VIRTUAL virtual
#endif

  VIRTUAL void call_VM_leaf_base(
    address entry_point,               // the entry point
    int     number_of_arguments        // the number of arguments to pop after the call
  );

  // This is the base routine called by the different versions of call_VM. The interpreter
  // may customize this version by overriding it for its purposes (e.g., to save/restore
  // additional registers when doing a VM call).
  //
  // If no java_thread register is specified (noreg) than rdi will be used instead. call_VM_base
  // returns the register which contains the thread upon return. If a thread register has been
  // specified, the return value will correspond to that register. If no last_java_sp is specified
  // (noreg) than rsp will be used instead.
  VIRTUAL void call_VM_base(           // returns the register containing the thread upon return
    Register oop_result,               // where an oop-result ends up if any; use noreg otherwise
    Register java_thread,              // the thread if computed before     ; use noreg otherwise
    Register last_java_sp,             // to set up last_Java_frame in stubs; use noreg otherwise
    address  entry_point,              // the entry point
    int      number_of_arguments,      // the number of arguments (w/o thread) to pop after the call
    bool     check_exceptions          // whether to check for pending exceptions after return
  );

  // These routines should emit JVMTI PopFrame and ForceEarlyReturn handling code.
  // The implementation is only non-empty for the InterpreterMacroAssembler,
  // as only the interpreter handles PopFrame and ForceEarlyReturn requests.
  virtual void check_and_handle_popframe(Register java_thread);
  virtual void check_and_handle_earlyret(Register java_thread);

  void call_VM_helper(Register oop_result, address entry_point, int number_of_arguments, bool check_exceptions = true);

  // helpers for FPU flag access
  // tmp is a temporary register, if none is available use noreg
  //void save_rax   (Register tmp);
  //void restore_rax(Register tmp);

 public:
  MacroAssembler(CodeBuffer* code) : Assembler(code) {}

  // Support for NULL-checks
  //
  // Generates code that causes a NULL OS exception if the content of reg is NULL.
  // If the accessed location is M[reg + offset] and the offset is known, provide the
  // offset. No explicit code generation is needed if the offset is within a certain
  // range (0 <= offset <= page_size).
  // use "teq 83, reg" in mips now, by yjl 6/20/2005
  void null_check(Register reg, int offset = -1);
  static bool needs_explicit_null_check(intptr_t offset);
	
	// Required platform-specific helpers for Label::patch_instructions.
  // They _shadow_ the declarations in AbstractAssembler, which are undefined.
  void pd_patch_instruction(address branch, address target);

  // Alignment
  void align(int modulus);

  // Misc
  //void fat_nop(); // 5 byte nop

  // Stack frame creation/removal
  void enter();
  void leave();

  // Support for getting the JavaThread pointer (i.e.; a reference to thread-local information)
  // The pointer will be loaded into the thread register.
  void get_thread(Register thread);


  // Support for VM calls
  //
  // It is imperative that all calls into the VM are handled via the call_VM macros.
  // They make sure that the stack linkage is setup correctly. call_VM's correspond
  // to ENTRY/ENTRY_X entry points while call_VM_leaf's correspond to LEAF entry points.


  void call_VM(Register oop_result,
               address entry_point,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1, Register arg_2,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               address entry_point,
               Register arg_1, Register arg_2, Register arg_3,
               bool check_exceptions = true);
  // Super call_VM calls - correspond to MacroAssembler::call_VM(_leaf) calls
  void super_call_VM_leaf(address entry_point);
  void super_call_VM_leaf(address entry_point, Register arg_1);
  void super_call_VM_leaf(address entry_point, Register arg_1, Register arg_2);
  void super_call_VM_leaf(address entry_point,
                          Register arg_1, Register arg_2, Register arg_3);

  // Overloadings with last_Java_sp
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               int number_of_arguments = 0,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, bool
               check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, Register arg_2,
               bool check_exceptions = true);
  void call_VM(Register oop_result,
               Register last_java_sp,
               address entry_point,
               Register arg_1, Register arg_2, Register arg_3,
               bool check_exceptions = true);

  void call_VM_leaf(address entry_point,
                    int number_of_arguments = 0);
  void call_VM_leaf(address entry_point,
                    Register arg_1);
  void call_VM_leaf(address entry_point,
                    Register arg_1, Register arg_2);
  void call_VM_leaf(address entry_point,
                    Register arg_1, Register arg_2, Register arg_3);

  // last Java Frame (fills frame anchor)
  void set_last_Java_frame(Register thread,
                           Register last_java_sp,
                           Register last_java_fp,
                           address last_java_pc);

  // thread in the default location (r15_thread on 64bit)
  void set_last_Java_frame(Register last_java_sp,
                           Register last_java_fp,
                           address last_java_pc);

  void reset_last_Java_frame(Register thread, bool clear_fp, bool clear_pc);

  // thread in the default location (r15_thread on 64bit)
  void reset_last_Java_frame(bool clear_fp, bool clear_pc);

  // Stores
  void store_check(Register obj);                // store check for obj - register is destroyed afterwards
  void store_check(Register obj, Address dst);   // same as above, dst is exact store location (reg. is destroyed)


  void g1_write_barrier_pre(Register obj,
#ifndef _LP64
                            Register thread,
#endif
                            Register tmp,
                            Register tmp2,
                            bool     tosca_live);
  void g1_write_barrier_post(Register store_addr,
                             Register new_val,
#ifndef _LP64
                             Register thread,
#endif
                             Register tmp,
                             Register tmp2);



  // split store_check(Register obj) to enhance instruction interleaving
  void store_check_part_1(Register obj);
  void store_check_part_2(Register obj);

  // C 'boolean' to Java boolean: x == 0 ? 0 : 1
  void c2bool(Register x);
  //add for compressedoops
  void load_klass(Register dst, Register src);
  void store_klass(Register dst, Register src);
  void load_prototype_header(Register dst, Register src);
/*
  // C++ bool manipulation

  void movbool(Register dst, Address src);
  void movbool(Address dst, bool boolconst);
  void movbool(Address dst, Register src);
  void testbool(Register dst);

  // oop manipulations
  void load_klass(Register dst, Register src);
  void store_klass(Register dst, Register src);

  void load_prototype_header(Register dst, Register src);*/

#ifdef _LP64
  void store_klass_gap(Register dst, Register src);

  void load_heap_oop(Register dst, Address src);
  void store_heap_oop(Address dst, Register src);
  void encode_heap_oop(Register r);
  void decode_heap_oop(Register r);
  void encode_heap_oop_not_null(Register r);
  void decode_heap_oop_not_null(Register r);
  void encode_heap_oop_not_null(Register dst, Register src);
  void decode_heap_oop_not_null(Register dst, Register src);

  void encode_klass_not_null(Register r);
  void decode_klass_not_null(Register r);
  void encode_klass_not_null(Register dst, Register src);
  void decode_klass_not_null(Register dst, Register src);

  //void set_narrow_oop(Register dst, jobject obj);

  // Returns the byte size of the instructions generated by decode_klass_not_null()
  // when compressed klass pointers are being used.
  static int instr_size_for_decode_klass_not_null();

  // if heap base register is used - reinit it with the correct value
  void reinit_heapbase();
  DEBUG_ONLY(void verify_heapbase(const char* msg);)

#endif // _LP64

  void incrementl(Register reg, int value = 1);

  void decrementl(Register reg, int value = 1);

/*
  // Int division/remainder for Java
  // (as idivl, but checks for special case as described in JVM spec.)
  // returns idivl instruction offset for implicit exception handling
  int corrected_idivl(Register reg);

  // Long division/remainder for Java
  // (as idivq, but checks for special case as described in JVM spec.)
  // returns idivq instruction offset for implicit exception handling
  int corrected_idivq(Register reg);
*/

  void int3();
/*
  // Long operation macros for a 32bit cpu
  // Long negation for Java
  void lneg(Register hi, Register lo);

  // Long multiplication for Java
  // (destroys contents of eax, ebx, ecx and edx)
  void lmul(int x_rsp_offset, int y_rsp_offset); // rdx:rax = x * y

  // Long shifts for Java
  // (semantics as described in JVM spec.)
  void lshl(Register hi, Register lo);                               // hi:lo << (rcx & 0x3f)
  void lshr(Register hi, Register lo, bool sign_extension = false);  // hi:lo >> (rcx & 0x3f)

  // Long compare for Java
  // (semantics as described in JVM spec.)
  void lcmp2int(Register x_hi, Register x_lo, Register y_hi, Register y_lo); // x_hi = lcmp(x, y)


  // misc
*/
  // Sign extension
#ifdef _LP64
  void sign_extend_short(Register reg) 	{ dsll32(reg, reg, 16); dsra32(reg, reg, 16); }
  void sign_extend_byte(Register reg)	{ dsll32(reg, reg, 24); dsra32(reg, reg, 24); }
#else
  void sign_extend_short(Register reg) 	{ sll(reg, reg, 16); sra(reg, reg, 16); }
  void sign_extend_byte(Register reg)	{ sll(reg, reg, 24); sra(reg, reg, 24); }
#endif
  void rem_s(FloatRegister fd, FloatRegister fs, FloatRegister ft, FloatRegister tmp);
	void rem_d(FloatRegister fd, FloatRegister fs, FloatRegister ft, FloatRegister tmp);

  // Inlined sin/cos generator for Java; must not use CPU instruction
  // directly on Intel as it does not have high enough precision
  // outside of the range [-pi/4, pi/4]. Extra argument indicate the
  // number of FPU stack slots in use; all but the topmost will
  // require saving if a slow case is necessary. Assumes argument is
  // on FP TOS; result is on FP TOS.  No cpu registers are changed by
  // this code.
  void trigfunc(char trig, int num_fpu_regs_in_use = 1);
/*
  // branch to L if FPU flag C2 is set/not set
  // tmp is a temporary register, if none is available use noreg
  void jC2 (Register tmp, Label& L);
  void jnC2(Register tmp, Label& L);

  // Pop ST (ffree & fincstp combined)
  void fpop();

  // pushes double TOS element of FPU stack on CPU stack; pops from FPU stack
  void push_fTOS();

  // pops double TOS element from CPU stack and pushes on FPU stack
  void pop_fTOS();

  void empty_FPU_stack();

  void push_IU_state();
  void pop_IU_state();

  void push_FPU_state();
  void pop_FPU_state();

  void push_CPU_state();
  void pop_CPU_state();

  // Round up to a power of two
  void round_to(Register reg, int modulus);

  // Callee saved registers handling
  void push_callee_saved_registers();
  void pop_callee_saved_registers();
*/
  // allocation
  void eden_allocate(
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Register t2,
    Label&   slow_case                 // continuation point if fast allocation fails
  );
  void tlab_allocate(
    Register obj,                      // result: pointer to object after successful allocation
    Register var_size_in_bytes,        // object size in bytes if unknown at compile time; invalid otherwise
    int      con_size_in_bytes,        // object size in bytes if   known at compile time
    Register t1,                       // temp register
    Register t2,                       // temp register
    Label&   slow_case                 // continuation point if fast allocation fails
    );
  void tlab_refill(Label& retry_tlab, Label& try_eden, Label& slow_case);

  //----
  //  void set_word_if_not_zero(Register reg); // sets reg to 1 if not zero, otherwise 0


  // Debugging

  // only if +VerifyOops
  void verify_oop(Register reg, const char* s = "broken oop");
  void verify_oop_addr(Address addr, const char * s = "broken oop addr");
  void verify_oop_subroutine();
  // TODO: verify method and klass metadata (compare against vptr?)
  void _verify_method_ptr(Register reg, const char * msg, const char * file, int line) {}
  void _verify_klass_ptr(Register reg, const char * msg, const char * file, int line){}
  
  #define verify_method_ptr(reg) _verify_method_ptr(reg, "broken method " #reg, __FILE__, __LINE__)
  #define verify_klass_ptr(reg) _verify_klass_ptr(reg, "broken klass " #reg, __FILE__, __LINE__)
 
  // only if +VerifyFPU
  void verify_FPU(int stack_depth, const char* s = "illegal FPU state");

  // prints msg, dumps registers and stops execution
  void stop(const char* msg);

  // prints msg and continues
  void warn(const char* msg);

  static void debug(char* msg/*, RegistersForDebugging* regs*/);
  static void debug32(int rdi, int rsi, int rbp, int rsp, int rbx, int rdx, int rcx, int rax, int eip, char* msg);
  static void debug64(char* msg, int64_t pc, int64_t regs[]);

  void print_reg(Register reg);
  void print_reg(FloatRegister reg);
  //void os_breakpoint();

  void untested()                                { stop("untested"); }

  void unimplemented(const char* what = "")      { char* b = new char[1024];  jio_snprintf(b, sizeof(b), "unimplemented: %s", what);  stop(b); }

  void should_not_reach_here()                   { stop("should not reach here"); }

  void print_CPU_state();

  // Stack overflow checking
  void bang_stack_with_offset(int offset) {
    // stack grows down, caller passes positive offset
    assert(offset > 0, "must bang with negative offset");
    if (offset <= 32768) {
      sw(A0, SP, -offset);
    } else {
#ifdef _LP64
      li(AT, offset);
      dsub(AT, SP, AT);
#else
      move(AT, offset);
      sub(AT, SP, AT);
#endif
      sw(A0, AT, 0);
    }
  }

  	// Writes to stack successive pages until offset reached to check for
  	// stack overflow + shadow pages.  Also, clobbers tmp
  	void bang_stack_size(Register size, Register tmp);
        virtual RegisterOrConstant delayed_value_impl(intptr_t* delayed_value_addr,
                                                       Register tmp,
                                                       int offset);

  	// Support for serializing memory accesses between threads
  	void serialize_memory(Register thread, Register tmp);

  	//void verify_tlab();
  	void verify_tlab(Register t1, Register t2);

  	// Biased locking support
  	// lock_reg and obj_reg must be loaded up with the appropriate values.
  	// swap_reg must be rax, and is killed.
  	// tmp_reg is optional. If it is supplied (i.e., != noreg) it will
  	// be killed; if not supplied, push/pop will be used internally to
  	// allocate a temporary (inefficient, avoid if possible).
  	// Optional slow case is for implementations (interpreter and C1) which branch to
  	// slow case directly. Leaves condition codes set for C2's Fast_Lock node.
  	// Returns offset of first potentially-faulting instruction for null
  	// check info (currently consumed only by C1). If
  	// swap_reg_contains_mark is true then returns -1 as it is assumed
  	// the calling code has already passed any potential faults.
  	int biased_locking_enter(Register lock_reg, Register obj_reg,
			  Register swap_reg, Register tmp_reg,
			  bool swap_reg_contains_mark,
			  Label& done, Label* slow_case = NULL,
			  BiasedLockingCounters* counters = NULL);
	void biased_locking_exit (Register obj_reg, Register temp_reg, Label& done);


  // Calls

	void call(address entry);
	void call(address entry, relocInfo::relocType rtype); 
	void call(address entry, RelocationHolder& rh); 
	// Emit the CompiledIC call idiom
	void ic_call(address entry);

	void jmp(address entry);
	void jmp(address entry, relocInfo::relocType rtype);

	// Argument ops
	/*inline void store_int_argument(Register s, Argument& a);
	inline void store_long_argument(Register s, Argument& a);
	inline void store_float_argument(FloatRegister s, Argument& a);
	inline void store_double_argument(FloatRegister s, Argument& a);
	inline void store_ptr_argument(Register s, Argument& a);*/
inline void store_int_argument(Register s, Argument &a) {
        if(a.is_Register()) {
                move(a.as_Register(), s);
        } else {
                sw(s, a.as_caller_address());
        }
}

inline void store_long_argument(Register s, Argument &a) {
        Argument a1 = a.successor();
        if(a.is_Register() && a1.is_Register()) {
                move(a.as_Register(), s);
                move(a.as_Register(), s);
        } else {
                sd(s, a.as_caller_address());
        }
}

inline void store_float_argument(FloatRegister s, Argument &a) {
        if(a.is_Register()) {
                mov_s(a.as_FloatRegister(), s);
        } else {
                swc1(s, a.as_caller_address());
        }
}
inline void store_double_argument(FloatRegister s, Argument &a) {
        if(a.is_Register()) {
                mov_d(a.as_FloatRegister(), s);
        } else {
                sdc1(s, a.as_caller_address());
        }
}

inline void store_ptr_argument(Register s, Argument &a) {
  if(a.is_Register()) {
    move(a.as_Register(), s);
  } else {
    st_ptr(s, a.as_caller_address());
  }
}

  // Load and store values by size and signed-ness
  void load_sized_value(Register dst, Address src, size_t size_in_bytes, bool is_signed, Register dst2 = noreg);
  void store_sized_value(Address dst, Register src, size_t size_in_bytes, Register src2 = noreg);

  // interface method calling
  void lookup_interface_method(Register recv_klass,
                               Register intf_klass,
                               RegisterOrConstant itable_index,
                               Register method_result,
                               Register scan_temp,
                               Label& no_such_interface);
  // virtual method calling
  void lookup_virtual_method(Register recv_klass,
                             RegisterOrConstant vtable_index,
                             Register method_result);

	// ld_ptr will perform lw for 32 bit VMs and ld for 64 bit VMs
	// st_ptr will perform sw for 32 bit VMs and sd for 64 bit VMs
	inline void ld_ptr(Register rt, Address a){
         #ifdef _LP64
         ld(rt, a.base(), a.disp());
         #else
         lw(rt, a.base(), a.disp());
         #endif
        }
	inline void ld_ptr(Register rt, Register base, int offset16){
         #ifdef _LP64
         ld(rt, base, offset16);
         #else
         lw(rt, base, offset16);
         #endif

        }
	inline void st_ptr(Register rt, Address a){
        #ifdef _LP64
         sd(rt, a.base(), a.disp());
        #else
         sw(rt, a.base(), a.disp());
        #endif
        }
	inline void st_ptr(Register rt, Register base, int offset16) {
        #ifdef _LP64
          sd(rt, base, offset16);
        #else
          sw(rt, base, offset16);
        #endif

        }

	void ld_ptr(Register rt, Register offset, Register base);
	void st_ptr(Register rt, Register offset, Register base);

	// ld_long will perform lw for 32 bit VMs and ld for 64 bit VMs
	// st_long will perform sw for 32 bit VMs and sd for 64 bit VMs
	inline void ld_long(Register rt, Register base, int offset16);
	inline void st_long(Register rt, Register base, int offset16);
	inline void ld_long(Register rt, Address a);
	inline void st_long(Register rt, Address a);


	void ld_long(Register rt, Register offset, Register base);
	void st_long(Register rt, Register offset, Register base);
	// Regular vs. d* versions
	inline void addu_long(Register rd, Register rs, Register rt) {
        #ifdef _LP64
         daddu(rd, rs, rt);
        #else
         addu(rd, rs, rt);
        #endif
        }
	inline void addu_long(Register rd, Register rs, long imm32_64) {
        #ifdef _LP64
         daddiu(rd, rs, imm32_64);
        #else
         addiu(rd, rs, imm32_64);
        #endif

        }

	// Floating
 public:
	// swap the two byte of the low 16-bit halfword
	// this directive will use AT, be sure the high 16-bit of reg is zero
	// by yjl 6/28/2005
	void hswap(Register reg);
  	void huswap(Register reg);

	// convert big endian integer to little endian integer
  // by yjl 6/29/2005
  	void swap(Register reg);
 
  // implement the x86 instruction semantic
  // if c_reg == *dest then *dest <= x_reg
	// else c_reg <= *dest
	// the AT indicate if xchg occurred, 1 for xchged, else  0
	// by yjl 6/28/2005
	void cmpxchg(Register x_reg, Address dest, Register c_reg);
#ifdef _LP64
	void dcmpxchg(Register x_reg, Address dest, Register c_reg);
#endif
	
	void round_to(Register reg, int modulus) {
		assert_different_registers(reg, AT);
		increment(reg, modulus - 1);
		move(AT, - modulus);
		andr(reg, reg, AT);
	}

	//pop & push, added by aoqi
#ifdef _LP64
	void extend_sign(Register rh, Register rl) { stop("extend_sign"); }
	void neg(Register reg) { dsubu(reg, R0, reg); }
	void push (Register reg)      { sd  (reg, SP, -8); daddi(SP, SP, -8); }
	void push (FloatRegister reg) { sdc1(reg, SP, -8); daddi(SP, SP, -8); }
	void pop  (Register reg)      { ld  (reg, SP, 0);  daddi(SP, SP, 8); }
	void pop  (FloatRegister reg) { ldc1(reg, SP, 0);  daddi(SP, SP, 8); }
	void pop  ()                  { daddi(SP, SP, 8); }
	void pop2 ()                  { daddi(SP, SP, 16); }
#else
	void extend_sign(Register rh, Register rl) { sra(rh, rl, 31); }
	void neg(Register reg) { subu(reg, R0, reg); }
	void push (Register reg)      { sw  (reg, SP, -4); addi(SP, SP, -4); }
	void push (FloatRegister reg) { swc1(reg, SP, -4); addi(SP, SP, -4); }
	void pop  (Register reg)      { lw  (reg, SP, 0);  addi(SP, SP, 4); }
	void pop  (FloatRegister reg) { lwc1(reg, SP, 0);  addi(SP, SP, 4); }
	void pop  ()                  { addi(SP, SP, 4); }
	void pop2 ()                  { addi(SP, SP, 8); }
#endif
	void push2(Register reg1, Register reg2);
	void pop2 (Register reg1, Register reg2);
	void dpush (Register reg)     { sd  (reg, SP, -8); daddi(SP, SP, -8); }
	void dpop  (Register reg)     { ld  (reg, SP, 0);  daddi(SP, SP, 8); }

	/* branches may exceed 16-bit offset */
	void b_far(address entry);
	void b_far(Label& L);
	
	void bne_far    (Register rs, Register rt, address entry);
	void bne_far    (Register rs, Register rt, Label& L);

	void beq_far    (Register rs, Register rt, address entry);
	void beq_far    (Register rs, Register rt, Label& L);

	//move an 32-bit immediate to Register
	void move(Register reg, int imm32)  { li32(reg, imm32); }
	void li	(Register rd, long imm);
	void li	(Register rd, address addr) { li(rd, (long)addr); }
	//replace move(Register reg, int imm)
	void li32(Register rd, int imm32); // sign-extends to 64 bits on mips64
#ifdef _LP64
	void dli(Register rd, long imm) { li(rd, imm); }
	void li64(Register rd, long imm);
	void li48(Register rd, long imm);
#endif

#ifdef _LP64
	void move(Register rd, Register rs)   { dadd(rd, rs, R0); }
	void move_u32(Register rd, Register rs)   { addu(rd, rs, R0); }
#else
	void move(Register rd, Register rs)   { add(rd, rs, R0); }
#endif
	void dmove(Register rd, Register rs)  { dadd(rd, rs, R0); }

#ifdef _LP64
  	void shl(Register reg, int sa)        { dsll(reg, reg, sa); }
  	void shr(Register reg, int sa)        { dsrl(reg, reg, sa); }
  	void sar(Register reg, int sa)        { dsra(reg, reg, sa); }
#else
  	void shl(Register reg, int sa)        { sll(reg, reg, sa); }
  	void shr(Register reg, int sa)        { srl(reg, reg, sa); }
  	void sar(Register reg, int sa)        { sra(reg, reg, sa); }
#endif

#ifndef PRODUCT
  static void pd_print_patched_instruction(address branch) {
  jint stub_inst = *(jint*) branch;
  print_instruction(stub_inst);
  ::tty->print("%s", " (unresolved)");

  }
#endif

	// the follow two might use AT register, be sure you have no meanful data in AT before you call them
	// by yjl 6/23/2005
	void increment(Register reg, int imm);
	void decrement(Register reg, int imm);

	//FIXME 
  	void empty_FPU_stack(){/*need implemented*/};

//we need 2 fun to save and resotre general register
	void pushad();
	void popad();

  // Test sub_klass against super_klass, with fast and slow paths.

  // The fast path produces a tri-state answer: yes / no / maybe-slow.
  // One of the three labels can be NULL, meaning take the fall-through.
  // If super_check_offset is -1, the value is loaded up from super_klass.
  // No registers are killed, except temp_reg.
  void check_klass_subtype_fast_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp_reg,
                                     Label* L_success,
                                     Label* L_failure,
                                     Label* L_slow_path,
                RegisterOrConstant super_check_offset = RegisterOrConstant(-1));

  // The rest of the type check; must be wired to a corresponding fast path.
  // It does not repeat the fast path logic, so don't use it standalone.
  // The temp_reg and temp2_reg can be noreg, if no temps are available.
  // Updates the sub's secondary super cache as necessary.
  // If set_cond_codes, condition codes will be Z on success, NZ on failure.
  void check_klass_subtype_slow_path(Register sub_klass,
                                     Register super_klass,
                                     Register temp_reg,
                                     Register temp2_reg,
                                     Label* L_success,
                                     Label* L_failure,
                                     bool set_cond_codes = false);

   // Simplified, combined version, good for typical uses.
   // Falls through on failure.
   void check_klass_subtype(Register sub_klass,
                            Register super_klass,
                            Register temp_reg,
                            Label& L_success);

  // method handles (JSR 292)
  Address argument_address(RegisterOrConstant arg_slot, int extra_slot_offset = 0);

  void get_vm_result  (Register oop_result, Register thread);
  void get_vm_result_2(Register metadata_result, Register thread);
#undef VIRTUAL
  void atomic_inc32(address counter_addr, int inc, Register tmp_reg1, Register tmp_reg2);

  void fast_lock(Register obj, Register box, Register tmp, Register scr);
  void fast_unlock(Register obj, Register box, Register tmp);
};


/**
 * class SkipIfEqual:
 *
 * Instantiating this class will result in assembly code being output that will
 * jump around any code emitted between the creation of the instance and it's
 * automatic destruction at the end of a scope block, depending on the value of
 * the flag passed to the constructor, which will be checked at run-time.
 */
class SkipIfEqual {
 private:
  MacroAssembler* _masm;
  Label _label;

 public:
   SkipIfEqual(MacroAssembler*, const bool* flag_addr, bool value);
   ~SkipIfEqual();
};

#ifdef ASSERT
inline bool AbstractAssembler::pd_check_instruction_mark() { return true; }
#endif



#endif // CPU_MIPS_VM_MACROASSEMBLER_MIPS_HPP
