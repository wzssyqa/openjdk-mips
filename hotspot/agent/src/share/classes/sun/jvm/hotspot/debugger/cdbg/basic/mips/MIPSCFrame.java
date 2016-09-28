/*
 * Copyright (c) 2001, 2012, Oracle and/or its affiliates. All rights reserved.
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

package sun.jvm.hotspot.debugger.cdbg.basic.mips;

import sun.jvm.hotspot.debugger.*;
import sun.jvm.hotspot.debugger.mips.*;
import sun.jvm.hotspot.debugger.cdbg.*;
import sun.jvm.hotspot.debugger.cdbg.basic.*;

/** Basic MIPS frame functionality providing sender() functionality. */

public class MIPSCFrame extends BasicCFrame {
  private Address ebp;
  private Address pc;

  private static final int ADDRESS_SIZE = 4;

  /** Constructor for topmost frame */
  public MIPSCFrame(CDebugger dbg, Address ebp, Address pc) {
    super(dbg);
    this.ebp = ebp;
    this.pc  = pc;
  }

  public CFrame sender(ThreadProxy thread) {
    MIPSThreadContext context = (MIPSThreadContext) thread.getContext();
    Address esp = context.getRegisterAsAddress(MIPSThreadContext.SP);

    if ( (ebp == null) || ebp.lessThan(esp) ) {
      return null;
    }

    Address nextEBP = ebp.getAddressAt( 0 * ADDRESS_SIZE);
    if (nextEBP == null) {
      return null;
    }
    Address nextPC  = ebp.getAddressAt( 1 * ADDRESS_SIZE);
    if (nextPC == null) {
      return null;
    }
    return new MIPSCFrame(dbg(), nextEBP, nextPC);
  }

  public Address pc() {
    return pc;
  }

  public Address localVariableBase() {
    return ebp;
  }
}
