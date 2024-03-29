/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use crate::pagetable_impl::{self, MappingDescriptor, MappingOp, PageMapFlags};
use nanobl_util::delay;
use nanobl_util::pagetable::{self, PageSize, SizeEnum};
use nanobl_util::region_builder::{self, RegionBuilder};
use nanobl_util::{console, huashan_hal::RxError, mem_regions, prelude::*};
use xmodem::Xmodem;

pub(super) static RECV_DEFN: LcmdDefn = LcmdDefn {
	name: "recv",
	help: LcmdHelp {
		synopsis: Some("addr[,count]::recv [-d N | -r] [-m]"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Receives data from a transmitter on this console device into the\n",
	"processor's normal memory space.  By default, the Xmodem protocol\n",
	"is used; the transmit program must support CRC16 and may use either\n",
	"128-byte or 1024-byte blocks.  The sx(1) program is suitable for\n",
	"this purpose.  The received data will be located at <addr>.  If\n",
	"<count> is supplied, the resulting behaviour depends on which\n",
	"protocol is in use, but in no event will more than <count> bytes be\n",
	"written into memory.  With neither -r nor -m, the default count is\n",
	"the number of bytes in contiguously mapped memory beginning at\n",
	"<addr>; with -m and without -r, the default count is 128 MiB.\n\n",
	"-d N      Delay N units of time, where each unit is 2**32 TSC ticks\n",
	"          or typically a bit less than 2 seconds.  This delay is\n",
	"          necessary to allow your Xmodem program time to start\n",
	"          before the receiver initiates the transfer.  The default\n",
	"          is 4 units or around 7 seconds.  This option is not valid\n",
	"          with raw mode.  No one knows why the authors of Xmodem\n",
	"          thought this is a good idea.\n",
	"-m        Automatically map memory.  Physical memory at the address\n",
	"          supplied will be identity-mapped prior to receiving data.\n",
	"          Memory will be mapped read/write using the largest page\n",
	"          size that can map the region exactly.  Existing mappings,\n",
	"          if any, are clobbered.\n",
	"-r        Use raw receive.  No CRC checking is performed, and\n",
	"          <count> must be specified; the loader will continue\n",
	"          receiving until <count> bytes are received or a receive\n",
	"          error occurs.  Raw mode receive can be aborted by sending\n",
	"          BREAK signals from the transmitter.\n\n",
	"Output: The supplied address and number of bytes actually received\n",
	"form the output address and count parameters, respectively.\n\n",
	"Safety: This command will fail if the region defined by <addr> and\n",
	"<count> is unmappable or part of the bootloader.  In addition, an\n",
	"attempt to receive via Xmodem will fail if the region overlaps the\n",
	"MMIO region (this is allowed for raw receives to accommodate manual\n",
	"register modification by sending a small number of bytes)")),
		..LcmdHelp::new(
			&RECV_DEFN,
			"receive data into processor memory space",
		)
	},
	exec: recv,
};

fn recv(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	/*
	 * Xmodem is designed so that the sender is started first and waits for
	 * the receiver to initiate the transfer by sending a byte.  This makes
	 * it very difficult for our use case, where we would need to start the
	 * send in the remote terminal with something like sx(1) attached to
	 * the remote UART, then somehow send the command to start the receiver
	 * and atomically, without taking any characters out of the receive
	 * buffer, restore the channel to the sending process.  Basically this
	 * protocol was designed for downloading filez from a BBS, not really
	 * for uploading.  Xmodem, the choice of leeches!
	 *
	 * One can certainly write a program that does that, but that's not how
	 * Unix terminal software like picocom or minicom actually works today
	 * and having to write special clients to interact with this would be
	 * blah.  We can make this slightly better by allowing the user to set
	 * the timeout and/or by doing some kind of timeout with the read
	 * routine and then trying to start receive in a loop (which is what
	 * rx(1) does).  Why anyone ever thought this was a good idea is beyond
	 * me.  So we implement a crude delay without timers.
	 *
	 * Units are 32-bit rollovers of the TSC, typically a little less than
	 * 2 seconds on the kind of machines this was written for.  Can be
	 * overridden with -d.
	 */
	const DEFAULT_DELAY: u64 = 4;
	const DEFAULT_XMODEM_RECV_SIZE: u64 = 128 * 1024 * 1024;

	let mut use_xmodem = true;
	let mut do_map = false;
	let mut delay = DEFAULT_DELAY;
	let mut in_delay = false;

	if (!inst.args().is_empty()) {
		let strargs = inst.args().split_whitespace();

		for s in strargs {
			match (s) {
				"-d" => in_delay = true,
				"-m" => do_map = true,
				"-r" => use_xmodem = false,
				_ => {
					if (in_delay) {
						delay = parse_integer::<u32>(
							s.trim(),
						)? as u64;
						in_delay = false;
					} else {
						return (Err(LcmdError::BadArgs(
							"unknown argument",
						)));
					}
				}
			}
		}
	};

	/*
	 * We have lots of problems figuring out where to stop.  Raw receive
	 * provides neither any in-band size nor any way to know when we're
	 * finished, so the user must provide a byte count.  Xmodem has a way
	 * to indicate end of transmission and some limited set of errors, but
	 * also has no way to know how large the file will be until then.
	 * Worse, Xmodem pads out the last block, which may be 128 or 1k bytes.
	 *
	 * For Xmodem, this is not a trivial concern: the user can supply a
	 * count, and we'll use it.  If it's too small, then Xmodem will get an
	 * error when it reaches the end and writing into it (via io::Write)
	 * fails.  This is annoying, but it does at least give the user a way
	 * to limit where we'll write, and the failure mode is reasonable.  If
	 * the user hasn't supplied a maximum, we'll supply something plausible
	 * and fail if it doesn't fit.
	 */
	let rb = RegionBuilder::new(inst.addr(), inst.count()).forbid_count_in(
		&(0u64 ..= 0u64),
		Some("nothing to do (count is 0)"),
	);

	let sz = if (do_map) {
		DEFAULT_XMODEM_RECV_SIZE
	} else if let Some(addr) = inst.addr() {
		let off = addr & PageSize::Size4K.as_offset_mask();
		let sz = pagetable_impl::find_size_of_mapped_region(addr.into())
			as u64;

		sz.saturating_sub(off)
	} else {
		1
	};

	let rb = if (use_xmodem) {
		rb.default_count(sz).forbid_region_overlap(
			&mem_regions::MMIO_RANGE,
			Some("destination overlaps with the MMIO region"),
		)
	} else {
		rb.default_count(1)
	};

	/*
	 * XXX This isn't correct if the user-supplied address and count are
	 * misaligned.  For example, consider (fff, 2).  We must in fact map
	 * 2 pages here but we will instead map only one, then fault when we
	 * write the second byte to 1000.  Fix the RegionBuilder.
	 */
	let mut region = if (do_map) {
		rb.align_address_down_to(PageSize::Size4K.as_size() as u64)
			.align_count_up_to(PageSize::Size4K.as_size() as u64)
	} else {
		rb.bound_region_with(pagetable_impl::require_mapped_bytes)
	}
	.forbid_region_overlap(
		&mem_regions::program_range(),
		Some("destination overlaps with the bootloader"),
	)
	.into_native_region()?;

	if (do_map) {
		let range = region.as_range();
		let page_size = pagetable::get_page_size(&range);
		let phys_base = *range.start() as u64;
		let phys_size = region.count as u64;
		let phys_range = (phys_base ..= phys_base + (phys_size - 1));
		let flags = if (region_builder::ranges_overlap(
			&mem_regions::MMIO_RANGE,
			&phys_range,
		)) {
			PageMapFlags::PCD | PageMapFlags::RW | PageMapFlags::P
		} else {
			PageMapFlags::RW | PageMapFlags::P
		};

		let md = MappingDescriptor {
			op: MappingOp::Iterate,
			size: page_size,
			base: phys_base.into(),
			flags,
		};
		pagetable_impl::map_region(&region, &md)?;
	}

	let mut dest =
		&mut unsafe { region.as_mut_slice() }[region.addr_off ..];

	let mut condev = console::console();

	if (!use_xmodem) {
		condev.read_exact_checked(dest).map_err(|e| {
			let off;

			match (e) {
				RxError::Break(n) => {
					off = n;
					print!("break error");
				}
				RxError::Framing(n) => {
					off = n;
					print!("framing error");
				}
				RxError::Parity(n) => {
					off = n;
					print!("parity error");
				}
				_ => {
					off = 0;
					print!("unknown error")
				}
			};
			println!(" at byte {}({:#x})", off, off);
			LcmdError::Propagated(
				"receive aborted due to streaming input error",
			)
		})?;

		return (Ok(LcmdValue {
			addr: Some(region.addr as u64),
			count: Some(region.count as u64),
		}));
	}

	let mut xmctx = Xmodem::new();

	delay::delay(delay << 32);

	let bytes = xmctx
		.recv(&mut condev, &mut dest, xmodem::Checksum::CRC16)
		.or(
			/* XXX */
			Err(LcmdError::Propagated("xmodem error")),
		)?;

	/*
	 * We auto-mapped space and what we received didn't fill it.  We now
	 * wish to unmap the pages contained completed within
	 * [addr + bytes, end).  Remember that the first byte we wrote
	 * may not have been aligned on a page boundary!
	 */
	if (do_map && bytes < region.count) {
		let range = region.as_range();
		let page_size = pagetable::get_page_size(&range);

		let start = region.addr + region.addr_off;
		let unmap_start = (((start + bytes) +
			(page_size.as_size() - 1)) &
			page_size.as_base_mask()) as u64;
		let unmap_end = (region.addr + region.count) as u64;

		let region = RegionBuilder::new(
			Some(unmap_start),
			Some(unmap_end - unmap_start),
		)
		.align_address_up_to(page_size.as_size() as u64)
		.align_count_down_to(PageSize::Size4K.as_size() as u64)
		.into_native_region()
		.unwrap();

		pagetable_impl::unmap_region(&region);
	}

	Ok(LcmdValue {
		addr: Some(region.addr as u64),
		count: Some(bytes as u64),
	})
}
