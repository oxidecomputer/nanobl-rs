/*
 * Copyright 2024 Oxide Computer Company
 */

use crate::println;
use core::fmt::Write;
use core::panic::PanicInfo;
use core::sync::atomic::{AtomicBool, Ordering};
use core::write;
use x86::{self, io, irq};

const POSTCODE_PORT: u16 = 0x80;
const PANIC_DEFAULT_CODE: u16 = 0xDEAD;
const PANIC_DEFAULT_VAL: u16 = 0x1111;

extern "C" {
	// Do not resuscitate.
	pub fn dnr() -> !;
}

pub fn halt() -> ! {
	unsafe {
		dnr();
	}
}

pub fn postcode(v: u32) {
	/*
	 * Safe because this crate is for use only at CPL = 0.
	 */
	unsafe {
		io::outl(POSTCODE_PORT, v);
	}
}

/*
 * As a special case, we support panicking with a 16-bit code and a 16-bit
 * value that will be sent to legacy I/O port 0x80 as a 32-bit value; this
 * allows panics before the UART driver is available, or in the UART driver
 * itself, to leave a trace that can be seen on the bench or via APML if
 * anything is listening on it.  To use this, the panicker must supply a
 * message whose first 10 bytes are exactly "HHHH,HHHH ", where H is the ASCII
 * code for a valid hex digit.  The first HHHH is the code and the second the
 * value.  Most machines have LED displays capable of showing only the code
 * portion (and many only the low byte), but the full 32-bit value is
 * accessible to a BMC or SP via SB-RMI.
 *
 * Normal panics initiated by core library don't contain such codes, but are
 * still fully supported and output to the console if possible.
 *
 * Now for the gritty part.  The no_std panic interface is borderline useless
 * because there is neither a fixed built-in panic buffer nor the ability to
 * allocate one on the fly.  This is great for very resource-constrained
 * environments but not so great for getting panic metadata out or otherwise
 * interpreting it in any way.  For this reason, the panic handler's payload
 * and message arguments cannot be directly interpreted; in practice, payload
 * is always empty and message contains an Arguments instance (which itself
 * requires use of an unstable feature to access, because of course it does).
 * This works fine for splatting a debug interpretation to the console (if we
 * have one) but not for interpreting anything in that payload.  So we hack
 * around this with our own panic buffer that looks at the first 10 bytes of
 * output that the payload would turn into and extracts the actual payload
 * metadata we want if it's present.  We do this by being core::fmt::Write.
 * Because we aren't guaranteed anything at all about how we'll be written to
 * through this interface, we simply accept all the data we're given and
 * discard anything beyond the part we might care about, then allow the caller
 * to query us for what we saw.  Importantly, our write_str method may be
 * called any number of times by the single underlying write operation.  The
 * caller (the panic handler) will extract the metadata, then write the payload
 * again to the console, which is fine even though write_str consumes the
 * Arguments, because Arguments is Copy.
 */
struct PanicBuf {
	buf: [u8; 10],
	pos: usize,
}

fn ascii_nybble_val(ascii: u8) -> u8 {
	match (ascii) {
		b'0' ..= b'9' => ascii - b'0',
		b'A' ..= b'F' => ascii - b'A' + 0xA,
		b'a' ..= b'f' => ascii - b'a' + 0xa,
		/*
		 * Normally we would panic here; however, the sole caller of
		 * this function is the panic handler, and in any case that
		 * code has already guaranteed this is unreachable.
		 */
		_ => 0xFF,
	}
}

impl PanicBuf {
	pub const fn new() -> Self {
		Self {
			buf: [0; 10],
			pos: 0,
		}
	}

	pub fn get_code(&self) -> u32 {
		let mut valid = self.pos == 10;
		let (mut code, mut val) = (0u16, 0u16);
		const DEFAULT: u32 = (PANIC_DEFAULT_VAL as u32) << 16 |
			PANIC_DEFAULT_CODE as u32;

		for (i, byte) in self.buf.iter().enumerate() {
			match (i, *byte) {
				(0 ..= 3, b'0' ..= b'9') |
				(0 ..= 3, b'A' ..= b'F') |
				(0 ..= 3, b'a' ..= b'f') => {
					code <<= 4;
					code |= u16::from(ascii_nybble_val(
						*byte,
					));
				}
				(0 ..= 3, _) => valid = false,

				(4, b',') => (),
				(4, b'_') => valid = false,

				(5 ..= 8, b'0' ..= b'9') |
				(5 ..= 8, b'A' ..= b'F') |
				(5 ..= 8, b'a' ..= b'f') => {
					val <<= 4;
					val |= u16::from(ascii_nybble_val(
						*byte,
					));
				}
				(5 ..= 8, _) => valid = false,

				(9, b' ') => (),
				(9, _) => valid = false,
				(_, _) => valid = false,
			};
			if (!valid) {
				return (DEFAULT);
			}
		}

		((val as u32) << 16) | code as u32
	}
}

impl core::fmt::Write for PanicBuf {
	/*
	 * No matter what, we must not panic, because we're already doing so.
	 * This is therefore written in the most conservative way imaginable.
	 */
	fn write_str(&mut self, s: &str) -> core::fmt::Result {
		let strbytes = s.as_bytes();
		let strbytelen = strbytes.len();

		let selfspaceleft = self.buf.len().saturating_sub(self.pos);
		let copylen = core::cmp::min(strbytelen, selfspaceleft);
		let selfbound = self.pos.checked_add(copylen).unwrap_or(0);

		if (selfbound <= self.pos || selfbound > self.buf.len()) {
			return (Ok(()));
		}

		let selfslice = &mut self.buf[self.pos .. selfbound];
		let strslice = &strbytes[0 .. copylen];

		selfslice.copy_from_slice(strslice);
		self.pos =
			self.pos.checked_add(copylen).unwrap_or(self.buf.len());

		Ok(())
	}
}

/*
 * This panic handler is concurrency-safe but not reentrant.  Thus we disable
 * interrupts prior to trying for the lock.  It could be made reentrant by
 * replacing the boolean lock with a byte containing the APIC ID of the lock
 * owner; however, there is no guarantee that the APIC IDs have been set up
 * (in fact, it's pretty well certain they haven't been, because this library
 * is for use either in something that would do that or something that would
 * load the thing that would do that), and in any case we don't do anything
 * here that would benefit from having interrupts enabled -- because we also
 * don't bother to set up an IDT.
 *
 * Therefore the requirements here are simple:
 *
 * - Like all code in this library, we must not induce an exception, because
 * the processor will shut down if we do, and
 * - We must take great pains to avoid panicking, because if we do we will
 * end up wedged forever in the CAS loop and whatever caused the original panic
 * will be lost.
 *
 * Asserting an NMI or SMI while we're here is UB, but that's just tough
 * cookies, ain't it?
 */
#[panic_handler]
pub fn panic(pi: &PanicInfo<'_>) -> ! {
	static mut lock: AtomicBool = AtomicBool::new(true);
	static mut pbuf: PanicBuf = PanicBuf::new();

	/*
	 * Safe because this crate is for use only at CPL = 0.
	 */
	unsafe {
		irq::disable();
	}

	/*
	 * Safe because it's atomic.
	 */
	if (unsafe {
		lock.compare_exchange(
			true,
			false,
			Ordering::Acquire,
			Ordering::Relaxed,
		)
	}
	.unwrap_or(false))
	{
		if let Some(msg) = pi.message() {
			/*
			 * Safe because we are protected by lock.
			 */
			unsafe {
				write!(&mut pbuf, "{msg}").unwrap_or(());
			}
		}

		/*
		 * Safe because we are protected by lock.
		 */
		postcode(unsafe { pbuf.get_code() });

		/*
		 * If a panic occurs during an XMODEM transfer or within the
		 * UART code itself, printing to the console might not work.
		 * To help in such cases, there's the `panic_console` feature
		 * that allows specifying an alternate console where we can
		 * print the panic message. If said feature is enabled, let's
		 * write to the panic console first.
		 */
		#[cfg(feature = "panic_console")]
		crate::pprintln!("panic: {:?}", pi);

		println!("panic: {:?}", pi);
	}

	halt();
}
