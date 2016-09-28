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

// Template for atomic, element-wise copy.
template <class T>
static void copy_conjoint_atomic(T* from, T* to, size_t count) {
  if (from > to) {
    while (count-- > 0) {
      // Copy forwards
      *to++ = *from++;
    }
  } else {
    from += count - 1;
    to   += count - 1;
    while (count-- > 0) {
      // Copy backwards
      *to-- = *from--;
    }
  }
}

static void pd_conjoint_words(HeapWord* from, HeapWord* to, size_t count) {
  (void)memmove(to, from, count * HeapWordSize);
}

static void pd_disjoint_words(HeapWord* from, HeapWord* to, size_t count) {
  switch (count) {
  case 8:  to[7] = from[7];
  case 7:  to[6] = from[6];
  case 6:  to[5] = from[5];
  case 5:  to[4] = from[4];
  case 4:  to[3] = from[3];
  case 3:  to[2] = from[2];
  case 2:  to[1] = from[1];
  case 1:  to[0] = from[0];
  case 0:  break;
  default:
    (void)memcpy(to, from, count * HeapWordSize);
    break;
  }
}

static void pd_disjoint_words_atomic(HeapWord* from, HeapWord* to, size_t count) {
  // pd_disjoint_words is word-atomic in this implementation.
  pd_disjoint_words(from, to, count);
}

static void pd_aligned_conjoint_words(HeapWord* from, HeapWord* to, size_t count) {
  (void)memmove(to, from, count * HeapWordSize);
}

static void pd_aligned_disjoint_words(HeapWord* from, HeapWord* to, size_t count) {
  pd_disjoint_words(from, to, count);
}

static void pd_conjoint_bytes(void* from, void* to, size_t count) {
  (void)memmove(to, from, count);
}

static void pd_conjoint_bytes_atomic(void* from, void* to, size_t count) {
  pd_conjoint_bytes(from, to, count);
}

static void pd_conjoint_jshorts_atomic(jshort* from, jshort* to, size_t count) {
//	(void)memmove(to, from, count << LogBytesPerShort);
        copy_conjoint_atomic<jshort>( from, to, count);
}

static void pd_conjoint_jints_atomic(jint* from, jint* to, size_t count) {
#ifdef _LP64
  // 2012/9/19 Jin: jint is counted by 4 bytes, while pd_conjoint_words handles 8 bytes as an unit.
  // Error: pd_conjoint_words((HeapWord*)from, (HeapWord*)to, count);
  // We found this bug when debugging UseCompressedOops.
//  memmove(to, from, count * sizeof(jint));
  copy_conjoint_atomic<jint>(from, to, count);
#else
  assert(HeapWordSize == BytesPerInt, "heapwords and jints must be the same size");
  // pd_conjoint_words is word-atomic in this implementation.
  pd_conjoint_words((HeapWord*)from, (HeapWord*)to, count);
#endif
}

//use set mips3 directive, gs2 is 64 cpu actually
//by yjl 4/27/2005
static void pd_conjoint_jlongs_atomic(jlong* from, jlong* to, size_t count) {
  if (count <= 0)
    return;

  jint tmp;
  jlong *_from = from, *_to = to;
  size_t _count= count;
#ifdef _LP64
  if (from > to) {
    __asm__ __volatile__ (
      "	.set push\n"
      " .set mips64\n"
      "	.set noreorder\n"

      "1:	ld 	%[__tmp], 0(%[__src])\n"
      "	daddi	%[__cnt], %[__cnt], -1\n"
      "	sd	%[__tmp], 0(%[__dst])\n"
      "	daddiu	%[__src], %[__src], 8\n"
      " bne	%[__cnt], $0, 1b\n"
      "	daddiu	%[__dst], %[__dst], 8\n"
      "	.set pop\n"
      : [__src] "=&r"(_from), 
      [__dst] "=&r"(_to), 
      [__cnt] "=&r"(_count),
      [__tmp] "=&r" (tmp)
      : "[__src]" (_from), "[__dst]" (_to), "[__cnt]" (_count) 
      : "memory"
      );
  } else {
    _from += count - 1; 
    _to += count - 1; 

    __asm__ __volatile__ (
      "	.set push\n"
      " .set mips64\n"
      "	.set noreorder\n"

      "1:	ld 	%[__tmp], 0(%[__src])\n"
      "	daddi	%[__cnt], %[__cnt], -1\n"
      "	sd	%[__tmp], 0(%[__dst])\n"
      "	daddiu	%[__src], %[__src], -8\n"
      " bne	%[__cnt], $0, 1b\n"
      "	daddiu	%[__dst], %[__dst], -8\n"
      "	.set pop\n"
      : [__src] "=&r"(_from), 
      [__dst] "=&r"(_to), 
      [__cnt] "=&r"(_count),
      [__tmp] "=&r" (tmp)
      : "[__src]" (_from), "[__dst]" (_to), "[__cnt]" (_count) 
      : "memory"
      );
  }
#else
  __asm__ __volatile__ (
      "	.set push\n"
      " .set mips64\n"
      "	.set noreorder\n"

      "1:	ld 	%[__tmp], 0(%[__src])\n"
      "	addi	%[__cnt], %[__cnt], -1\n"
      "	sd	%[__tmp], 0(%[__dst])\n"
      "	addiu	%[__src], %[__src], 8\n"
      " bne	%[__cnt], $0, 1b\n"
      "	addiu	%[__dst], %[__dst], 8\n"
      "	.set pop\n"
      : [__src] "=&r"(_from), 
      [__dst] "=&r"(_to), 
      [__cnt] "=&r"(_count),
      [__tmp] "=&r" (tmp)
      : "[__src]" (_from), "[__dst]" (_to), "[__cnt]" (_count) 
      : "memory"
      );
#endif
/*
  tty->print_cr("old mark %x, old klass: %p, new mark %x, new klass %p", ((void**)from)[0], ((void**)from)[1], 
    ((void**)to)[0], ((void**)to)[1]);
*/
}

static void pd_conjoint_oops_atomic(oop* from, oop* to, size_t count) {
  assert(HeapWordSize == BytesPerOop, "heapwords and oops must be the same size");
  // pd_conjoint_words is word-atomic in this implementation.
  pd_conjoint_words((HeapWord*)from, (HeapWord*)to, count);
}

static void pd_arrayof_conjoint_bytes(HeapWord* from, HeapWord* to, size_t count) {
  pd_conjoint_bytes(from, to, count);
}

static void pd_arrayof_conjoint_jshorts(HeapWord* from, HeapWord* to, size_t count) {
	pd_conjoint_jshorts_atomic((jshort*)from, (jshort*)to, count);
}

static void pd_arrayof_conjoint_jints(HeapWord* from, HeapWord* to, size_t count) {
  pd_conjoint_jints_atomic((jint*)from, (jint*)to, count);
}

static void pd_arrayof_conjoint_jlongs(HeapWord* from, HeapWord* to, size_t count) {
  pd_conjoint_jlongs_atomic((jlong*)from, (jlong*)to, count);
}

static void pd_arrayof_conjoint_oops(HeapWord* from, HeapWord* to, size_t count) {
  pd_conjoint_oops_atomic((oop*)from, (oop*)to, count);
}
