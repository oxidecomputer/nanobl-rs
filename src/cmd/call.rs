/*
 * Copyright 2025 Oxide Computer Company
 */

use super::api::*;
use crate::pagetable_impl;
use core::arch::naked_asm;
use nanobl_util::mem_regions;
use nanobl_util::region_builder::RegionBuilder;

pub(super) static CALL_DEFN: LcmdDefn = LcmdDefn {
	name: "call",
	help: LcmdHelp {
		synopsis: Some("addr::call [args...]"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Transfers control to foreign code at <addr>.  This can be a one-way\n",
	"or two-way transfer of control; System V ABI calling and return\n",
	"conventions are observed.  Up to 6 64-bit integer arguments may be\n",
	"passed, which will be placed in registers prior to transferring\n",
	"control.  If the called code returns, the value in %rax will be\n",
	"displayed and the bootloader will resume execution.  For this to\n",
	"work, the called code must not modify the contents of memory\n",
	"between %rsp and _end_bss inclusive, nor unmap that memory, and\n",
	"must return with the control registers and UART state intact.\n",
	"Otherwise the requirements are the same as for any called function\n",
	"observing the amd64 System V ABI.\n\n",
	"Output: the return value of the called function is output as the\n",
	"address parameter.  There is no count output.\n\n",
	"<addr> must be a valid identity-mapped address and must not reside\n",
	"in the MMIO region.  It may reside within the text or data of the\n",
	"bootloader, but if it does, the called function must not return.")),
		..LcmdHelp::new(&CALL_DEFN, "transfer program control")
	},
	exec: call,
};

fn call(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let target = RegionBuilder::new(inst.addr(), inst.count())
		.forbid_count(Some("count parameter is not permitted"))
		.forbid_address_in(
			&mem_regions::MMIO_RANGE,
			Some("call target lies within the MMIO region"),
		)
		.bound_region_with(pagetable_impl::require_mapped_bytes)
		.into_addr()?;

	let mut ca: [u64; 6] = [0; 6];

	for (count, maybe_u64) in inst
		.args()
		.split_whitespace()
		.fuse()
		.map(|s| parse_integer::<u64>(s.trim()))
		.enumerate()
	{
		if (count >= ca.len()) {
			return (Err(LcmdError::ExtraArgs(
				"at most 6 extra arguments are allowed",
			)));
		}
		ca[count] = maybe_u64?;
	}

	/*
	 * This is a bit tricky.  While the most common use case of this command
	 * is a one-way transfer of control to a loaded program, it's also
	 * useful to allow two-way transfers to things that agree to look like
	 * ABI-compliant functions.  For example, this allows observing the
	 * effects of a particular entry point in some loaded program without
	 * the complexity of enabling exceptions and writing a real debugger.
	 * To accomplish this, we'll create a naked function and use the
	 * normal Rust compiler to generate its stack frame for us.  We could
	 * do this with regular inline assembly instead, but we'd have to
	 * explicitly explain the ABI in the form of clobbers and options,
	 * which is rather tedious.  An example of this may be found in the
	 * unstable book's section on symbol operands.
	 *
	 * The target is last so that it will be on the stack, preserving the
	 * other arguments in registers just as they should be.
	 *
	 * We prefer allow(unused) here to prepending an _ to each parameter
	 * because these are not in fact unused.
	 */
	#[cfg(target_pointer_width = "64")]
	#[unsafe(naked)]
	#[allow(unused)]
	unsafe extern "system" fn foreign_call(
		arg0: u64,
		arg1: u64,
		arg2: u64,
		arg3: u64,
		arg4: u64,
		arg5: u64,
		target: u64,
	) -> u64 {
		naked_asm!(
			"movq 8(%rsp), %rax",
			"jmp *%rax",
			options(att_syntax)
		);
	}

	/*
	 * Safety:
	 *
	 * The user has asserted that the target of this call is mapped and
	 * contains code.  The user has also asserted either that the target
	 * does not reside within this program or that the call will never
	 * return.  See manual at the top of this file.
	 */
	let ret: u64 = unsafe {
		foreign_call(ca[0], ca[1], ca[2], ca[3], ca[4], ca[5], target)
	};

	/*
	 * We don't expect to get back here; if we do, our return is the return
	 * value of the function that was called.
	 */
	Ok(LcmdValue {
		addr: Some(ret),
		count: None,
	})
}
