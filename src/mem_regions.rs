/*
 * Copyright 2024 Oxide Computer Company
 */

use core::arch::asm;
use core::ops::{Range, RangeInclusive};
use x86::bits64::paging;
use x86::bits64::registers;

/*
 * These are architecturally defined and cannot change (at least, not in a way
 * that would make them more restrictive).
 */
pub const PHYS_RANGE: RangeInclusive<u64> =
	(0 ..= (1u64 << paging::MAXPHYADDR) - 1);
pub const IDENTITY_RANGE: RangeInclusive<u64> = (0 ..= (1u64 << 47) - 1);
pub const VIRT_RANGE_LOW: RangeInclusive<u64> = IDENTITY_RANGE;
pub const VIRT_RANGE_HIGH: RangeInclusive<u64> =
	(0xFFFF_8000_0000_0000 ..= 0xFFFF_FFFF_FFFF_FFFF);

/*
 * This is less well-defined and could change; for current machines the
 * default seems to be B000_0000, but it can be made to start as low as
 * 8000_0000.  More to the point, it's possible both to set this and to figure
 * out where it's been set; our objectives do not include setting it, and the
 * upper 1 GB contains all of this machine type's permanently mapped devices.
 * This also corresponds to the huge pages we mark uncacheable in the
 * pagetable.  There is almost certainly no good reason to change this for the
 * kinds of programs with which this is meant to be used, but if you need to
 * do so, be sure to change the stub pagetables to match.
 */
pub const MMIO_RANGE: RangeInclusive<u64> = (0x8000_0000 ..= 0xFFFF_FFFF);
pub const UART_RANGE: RangeInclusive<u64> = (0xFEDC_9000 ..= 0xFEDC_AFFF);

/*
 * Returns the address of the last byte that is (or may be) part of the BSS.
 * Tortuous to allow the BSS to end right at 0x8000_0000 without making this
 * induce creation of a GOT.
 */
fn get_ebss() -> usize {
	let v: usize;
	unsafe {
		asm!(
			"movq	$(__ebss - 1), {}",
			out(reg) v,
			options(nomem, preserves_flags, nostack, att_syntax)
		);
	}

	v + 1
}

/*
 * This function is safe; however, relying on its result in unsafe ways is
 * still unsafe.  Additionally, the stack pointer can and will move, meaning
 * that what is safe to write to at this moment in time may not be safe later.
 * The user of the return value is responsible for ensuring the safety of its
 * usage of the range.  It would be possible to make this safe, but doing so
 * requires a language feature we don't have: an object representing the value
 * of the stack pointer whose ownership remains valid only so long as the stack
 * pointer has not been decremented (or, perhaps for some uses, changed at all,
 * but that requires even more language support).  For now, this...
 *
 * To provide real safety here requires the proper use of privilege levels and
 * the MMU.
 */
pub fn program_range() -> Range<u64> {
	let rsp = registers::rsp();
	let ebss = get_ebss();

	/*
	 * Safety: Guaranteed to be set correctly by our link-editing script.
	 */
	(rsp .. ebss as u64)
}
