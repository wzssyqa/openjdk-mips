/*
 * Copyright (c) 1997, 2014, Oracle and/or its affiliates. All rights reserved.
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
#include "memory/resourceArea.hpp"
#include "runtime/java.hpp"
#include "runtime/stubCodeGenerator.hpp"
#include "vm_version_mips.hpp"
#ifdef TARGET_OS_FAMILY_linux
# include "os_linux.inline.hpp"
#endif
#ifdef TARGET_OS_FAMILY_solaris
# include "os_solaris.inline.hpp"
#endif
#ifdef TARGET_OS_FAMILY_windows
# include "os_windows.inline.hpp"
#endif
#ifdef TARGET_OS_FAMILY_bsd
# include "os_bsd.inline.hpp"
#endif
/*
int VM_Version::_cpu;
int VM_Version::_model;
int VM_Version::_stepping;
int VM_Version::_cpuFeatures;
const char*           VM_Version::_features_str = "";
VM_Version::CpuidInfo VM_Version::_cpuid_info   = { 0, };

static BufferBlob* stub_blob;
static const int stub_size = 300;

extern "C" {
  typedef void (*getPsrInfo_stub_t)(void*);
}
static getPsrInfo_stub_t getPsrInfo_stub = NULL;
*/
int VM_Version::_features = VM_Version::unknown_m;
const char* VM_Version::_features_str = "";
/*
class VM_Version_StubGenerator: public StubCodeGenerator {
 public:

  VM_Version_StubGenerator(CodeBuffer *c) : StubCodeGenerator(c) {}

  address generate_getPsrInfo() {
  };
};


void VM_Version::get_processor_features() {
}
*/
void VM_Version::initialize() {
	_features = determine_features();
	//no need, Abstract_VM_Version already define it as false
	_supports_cx8 = true;

	char buf[256];
	jio_snprintf(buf, sizeof(buf), "%s, %s"
#ifdef OPT_RANGECHECK
			", optimized range check"
#endif
#ifdef OPT_PHI_1
			", optimized phi"
#endif
#ifdef OPT_MERGE
			", optimized merge"
#endif
			,	(has_l2_cache() ? "has_l2_cache" : ""), (has_16k_page() ? "has_16k_page" : "")
	);
	//////////////////////add some other feature here//////////////////
	
	// buf is started with ", " or is empty
	_features_str = strdup(buf);
	NOT_PRODUCT( if (PrintMiscellaneous && Verbose) print_features(); );
}

void VM_Version::print_features() {
	tty->print_cr("Version:%s", cpu_features());
}

int VM_Version::determine_features() {
	//////////////////////add some other feature here//////////////////
	return spt_16k_page_m; 
}

static int saved_features = 0;

void VM_Version::allow_all() {
	saved_features = _features;
	_features     = all_features_m;
}

void VM_Version::revert() {
	_features = saved_features;
}
