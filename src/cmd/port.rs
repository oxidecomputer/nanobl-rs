/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use nanobl_util::prelude::*;
use nanobl_util::region_builder::RegionBuilder;

pub(super) static PORT_READ_DEFN: LcmdDefn = LcmdDefn {
	name: "in",
	help: LcmdHelp {
		synopsis: Some("addr,[count]::in"),
		desc: Some(concat!(
	"Read <count> bytes from the I/O port at <addr>.  The read length\n",
	"must be either 1, 2 or 4 bytes and the address must have the same\n",
	"alignment.  If not specified, <count> defaults to 4 bytes.\n\n",
	"The value read is displayed as a <count>-byte native-endian\n",
	"integer.\n\n",
	"Output: None.\n\n",
	"Safety: You must ensure that <addr> is valid and safe to read.")),
		..LcmdHelp::new(&PORT_READ_DEFN, "read I/O port")
	},
	exec: port_read,
};

pub(super) static PORT_WRITE_DEFN: LcmdDefn = LcmdDefn {
	name: "out",
	help: LcmdHelp {
		synopsis: Some("addr,[count]::out VALUE"),
		desc: Some(concat!(
	"Write <VALUE> as a [count]-byte value to the I/O port at <addr>.\n",
	"The write length must be either 1, 2 or 4 bytes and the address\n",
	"must have the same alignment.  If not specified, <count> defaults\n",
	"to 4 bytes.  <VALUE> is intepreted as a <count>-byte native-endian\n",
	"integer.\n\n",
	"Output: None.\n\n",
	"Safety: You must ensure that <addr> is valid and safe to write.")),
		..LcmdHelp::new(&PORT_WRITE_DEFN, "write I/O port")
	},
	exec: port_write,
};

fn addr_count(inst: &LcmdInstance<'_>) -> Result<(u16, u64), LcmdError> {
	let (addr, _, count, _) = RegionBuilder::new(inst.addr(), inst.count())
		.default_count(4)
		.bound_count_with(|c| match c {
			Some(1) | Some(2) | Some(4) => Ok(()),
			_ => Err("count must be 1, 2 or 4"),
		})
		.bound_region_in(
			&(0 ..= 0xFFFF),
			Some("ports are limited to 16 bits"),
		)
		.into_tuple()?;
	if (addr % count) != 0 {
		return Err(LcmdError::BadArgs(
			"invalid port addr alignment for count",
		));
	}
	Ok((addr as u16, count))
}

fn port_read(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let (port, count) = addr_count(&inst)?;

	// Safe because the user has guaranteed this port is readable.
	let val = unsafe {
		match count {
			1 => x86::io::inb(port) as u32,
			2 => x86::io::inw(port) as u32,
			4 => x86::io::inl(port),
			_ => unreachable!(),
		}
	};

	println!("PORT    VALUE\n{port:>4x} {val:>8x}");

	Ok(LcmdValue::NONE)
}

fn port_write(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let (port, count) = addr_count(&inst)?;

	let val = if inst.args().is_empty() {
		return (Err(LcmdError::MissingArgs(
			"value argument is required",
		)));
	} else {
		parse_integer::<u32>(inst.args().trim())?
	};

	// Safe because the user has guaranteed this port is writable
	// and the value being written is valid.
	if count == 1 {
		if val > u8::MAX as u32 {
			return Err(LcmdError::BadArgs(
				"value too large for count",
			));
		}
		unsafe { x86::io::outb(port, val as u8) }
	} else if count == 2 {
		if val > u16::MAX as u32 {
			return Err(LcmdError::BadArgs(
				"value too large for count",
			));
		}
		unsafe { x86::io::outw(port, val as u16) }
	} else if count == 4 {
		unsafe { x86::io::outl(port, val) }
	}

	Ok(LcmdValue::NONE)
}
