/*
 * Copyright (c) 2006, 2012, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_MIPS_VM_VMREG_MIPS_INLINE_HPP
#define CPU_MIPS_VM_VMREG_MIPS_INLINE_HPP

inline VMReg RegisterImpl::as_VMReg() {
  if( this==noreg ) return VMRegImpl::Bad();
#ifdef _LP64
  //FIXME why encoding << 1? what is the meaning of the VMReg's value
  return VMRegImpl::as_VMReg(encoding() << 1 );
#else
  return VMRegImpl::as_VMReg(encoding() );
#endif // _LP64
}

inline VMReg FloatRegisterImpl::as_VMReg() {
#ifdef _LP64
  return VMRegImpl::as_VMReg((encoding() << 1) + ConcreteRegisterImpl::max_gpr);
#else
  return VMRegImpl::as_VMReg((encoding()) + ConcreteRegisterImpl::max_gpr);
#endif // _LP64
}

inline bool VMRegImpl::is_Register() {
  return (unsigned int) value() < (unsigned int) ConcreteRegisterImpl::max_gpr;
}

inline bool VMRegImpl::is_FloatRegister() {
  return value() >= ConcreteRegisterImpl::max_gpr && value() < ConcreteRegisterImpl::max_fpr;
}

inline Register VMRegImpl::as_Register() {

  assert( is_Register(), "must be");
  // Yuk
#ifdef _LP64
  return ::as_Register(value() >> 1);
#else
  return ::as_Register(value());
#endif // _LP64
}

inline FloatRegister VMRegImpl::as_FloatRegister() {
  assert( is_FloatRegister(), "must be" );
  // Yuk
#ifdef _LP64
  assert( is_even(value()), "must be" );
  return ::as_FloatRegister((value() - ConcreteRegisterImpl::max_gpr) >> 1);
#else
  return ::as_FloatRegister((value() - ConcreteRegisterImpl::max_gpr));
#endif // _LP64
}

inline   bool VMRegImpl::is_concrete() {
  assert(is_reg(), "must be");
  if(is_Register()) return true;
  if(is_FloatRegister()) return true;
  assert(false, "what register?");
  return false;
}

#endif // CPU_MIPS_VM_VMREG_MIPS_INLINE_HPP

