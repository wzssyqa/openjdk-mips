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

#ifndef CPU_MIPS_VM_VM_VERSION_MIPS_HPP
#define CPU_MIPS_VM_VM_VERSION_MIPS_HPP
 
#include "runtime/globals_extension.hpp"
#include "runtime/vm_version.hpp"


class VM_Version: public Abstract_VM_Version {
protected:
	 enum Feature_Flag {
		 with_l2_cache = 0,
		 spt_16k_page = 1,
		 //////////////////////add some other feature here//////////////////
	 };

	 enum Feature_Flag_Set {
		 unknown_m	  = 0,
		 all_features_m	  = -1,
		 with_l2_cache_m  = 1 << with_l2_cache,
		 spt_16k_page_m   = 1 << spt_16k_page,

		 //////////////////////add some other feature here//////////////////
	 };
		 
	static int  _features;
	static const char* _features_str;

	static void print_features();
	static int  determine_features();

public:
	// Initialization
	static void initialize();
	
	//mips has no such instructions, use ll/sc instead
	static bool supports_compare_and_exchange() { return false; }

	static bool has_l2_cache() { return _features & with_l2_cache_m; }
	static bool has_16k_page() { return _features & spt_16k_page_m; }
		
	//////////////////////add some other feature here//////////////////

	static const char* cpu_features() { return _features_str; }
	  
	// Assembler testing
	static void allow_all();
	static void revert();		
};

#endif // CPU_MIPS_VM_VM_VERSION_MIPS_HPP
