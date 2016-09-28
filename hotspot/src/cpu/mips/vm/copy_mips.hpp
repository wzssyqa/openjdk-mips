/*
 * Copyright (c) 2003, 2010, Oracle and/or its affiliates. All rights reserved.
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

#ifndef CPU_MIPS_VM_COPY_MIPS_HPP
#define CPU_MIPS_VM_COPY_MIPS_HPP

// Inline functions for memory copy and fill.
//
// // Contains inline asm implementations
#ifdef TARGET_OS_ARCH_linux_mips
# include "copy_linux_mips.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_solaris_mips
# include "copy_solaris_mips.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_windows_mips
# include "copy_windows_mips.inline.hpp"
#endif
#ifdef TARGET_OS_ARCH_bsd_mips
# include "copy_bsd_mips.inline.hpp"
#endif
// Inline functions for memory copy and fill.

// Contains inline asm implementations

static void pd_fill_to_words(HeapWord* tohw, size_t count, juint value) {
  juint* to = (juint*)tohw;
  count *= HeapWordSize / BytesPerInt;
  while (count-- > 0) {
    *to++ = value;
  }
}

static void pd_fill_to_aligned_words(HeapWord* tohw, size_t count, juint value) {
  pd_fill_to_words(tohw, count, value);
}

static void pd_fill_to_bytes(void* to, size_t count, jubyte value) {
  (void)memset(to, value, count);
}

static void pd_zero_to_words(HeapWord* tohw, size_t count) {
  pd_fill_to_words(tohw, count, 0);
}

static void pd_zero_to_bytes(void* to, size_t count) {
  (void)memset(to, 0, count);
}

#endif //CPU_MIPS_VM_COPY_MIPS_HPP
