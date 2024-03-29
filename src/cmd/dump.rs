/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use crate::pagetable_impl;
use nanobl_util::prelude::*;
use nanobl_util::region_builder::{NativeRegion, RegionBuilder};

pub(super) static DUMP_DEFN: LcmdDefn = LcmdDefn {
	name: "dump",
	help: LcmdHelp {
		synopsis: Some("addr[,count]::dump"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Displays the contents of the processor's normal memory space.\n",
	"The contents are displayed bytewise in 4 groups of 4, 16 bytes per\n",
	"line of output.  The output is guaranteed to include at least\n",
	"<count> bytes beginning with and including <addr>.  If <count> is\n",
	"not supplied, only the group including <addr> is displayed.  All\n",
	"contents are displayed as hex bytes; in addition, bytes that can be\n",
	"interpreted as printable ASCII characters are displayed as such;\n",
	"others are displayed as '.'.  This is generally similar to xxd(1)\n",
	"and mdb(1)'s _dump_ output; however, useful additions like -e and\n",
	"-g are not available.\n\n",
	"Output: None.\n\n",
	"Safety: It is possible (and useful) to dump regions containing MMIO\n",
	"registers; however, you must ensure that the side effects, if any,\n",
	"of doing so are safe.  You must also ensure that the region you\n",
	"dump is mapped (by default, the first 8 GB of physical address\n",
	"space is identity-mapped) and readable.")),
		..LcmdHelp::new(
			&DUMP_DEFN,
			"dump processor memory space contents",
		)
	},
	exec: dump,
};

fn dump(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let region: NativeRegion<u8> =
		RegionBuilder::new(inst.addr(), inst.count())
			.default_count(1)
			.forbid_count_in(
				&(0u64 ..= 0u64),
				Some("nothing to do (count is 0)"),
			)
			.align_address_down_to(16u64)
			.align_count_up_to(16u64)
			.bound_region_with(pagetable_impl::require_mapped_bytes)
			.into_native_region()?;

	print!("{:<18}", "");
	for i in (0 .. 16) {
		let width = if (i % 4 == 0) { 3 } else { 2 };
		if (i == region.addr_off) {
			print!("{:>width$}", "\\/", width = width);
		} else {
			print!("{:>width$x}", i, width = width);
		}
	}

	print!("  ");
	for i in (0 .. 16) {
		if (i == region.addr_off) {
			print!("v");
		} else {
			print!("{:x}", i);
		}
	}
	println!();

	for chunk in unsafe { region.as_slice() }.chunks(16) {
		print!("{:>16x}:  ", chunk.as_ptr() as usize);
		for (j, byte) in chunk.iter().enumerate() {
			print!("{:02x}", byte);
			if (j % 4 == 3) {
				print!(" ");
			}
		}

		print!(" ");

		for byte in chunk {
			let ch = if (byte.is_ascii_graphic()) {
				*byte as char
			} else {
				'.'
			};
			print!("{}", ch);
		}
		println!();
	}

	Ok(LcmdValue::NONE)
}
