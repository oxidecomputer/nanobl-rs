/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use core::convert::TryInto;
use nanobl_util::prelude::*;
use nanobl_util::region_builder::{self, Region, RegionBuilder};
use x86::msr;

pub(super) static RD_MSR_DEFN: LcmdDefn = LcmdDefn {
	name: "rdmsr",
	help: LcmdHelp {
		synopsis: Some("regno[,count]::rdmsr"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Reads the contents of a model-specific register (MSR) or a\n",
	"contiguous sequence of <count> MSRs beginning at <regno>.  The\n",
	"values of each MSR are displayed as 64-bit native endian.\n",
	"integers.\n\n",
	"Output: None.\n\n",
	"Safety: You must ensure that <regno> and, if <count> is supplied,\n",
	"all MSRs in [<addr>, <addr> + <count>), are valid and safe to read.")),
		..LcmdHelp::new(&RD_MSR_DEFN, "read model-specific register(s)")
	},
	exec: read,
};

pub(super) static WR_MSR_DEFN: LcmdDefn = LcmdDefn {
	name: "wrmsr",
	help: LcmdHelp {
		synopsis: Some("regno[,count]::wrmsr VALUE"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Writes <VALUE> into the model-specific register (MSR) <regno> and,\n",
	"if <count> is supplied, additionally into each of the <count> - 1\n",
	"successive MSRs.  <VALUE> is interpreted as a 64-bit native-endian\n",
	"integer.\n\n",
	"Output: None.\n\n",
	"Safety: You must ensure that <regno> and, if <count> is supplied,\n",
	"all MSRs in [<addr>, <addr> + <count>), are valid and may safely be\n",
	"written with <VALUE>.")),
		..LcmdHelp::new(
			&WR_MSR_DEFN,
			"write model-specific register(s)",
		)
	},
	exec: write,
};

fn get_region(
	inst: &LcmdInstance<'_>,
) -> Result<Region, region_builder::Error> {
	RegionBuilder::new(inst.addr(), inst.count())
		.default_count(1)
		.forbid_count_in(&(0 ..= 0), Some("nothing to do (count is 0)"))
		.bound_region_in(
			&(0 ..= 0xFFFF_FFFF),
			Some("MSR numbers are limited to 32 bits"),
		)
		.into_region()
}

fn for_region_with<F>(r: &Region, f: F)
where
	F: Fn(u32),
{
	/*
	 * These are guaranteed never to panic because their invariants were
	 * satisfied at the time the Region was created.
	 */
	let addr: u32 = r.addr.try_into().unwrap();
	let count: u32 = r.count.try_into().unwrap();

	for msr in (addr ..= addr + (count - 1)) {
		f(msr);
	}
}

fn read(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let region = get_region(&inst)?;

	if (!inst.args().is_empty()) {
		return (Err(LcmdError::ExtraArgs(
			"no additional arguments are accepted",
		)));
	}

	println!("{:>8} {:>16}", "MSR", "VALUE");

	for_region_with(&region, |msr| {
		/*
		 * Safe because:
		 * 1. This crate is for use only with CPL = 0.
		 * 2. The user has guaranteed this MSR is readable.
		 */
		println!(
			"{msr:>08x} {val:>016x}",
			msr = msr,
			val = unsafe { msr::rdmsr(msr) }
		);
	});

	Ok(LcmdValue::NONE)
}

fn write(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let region = get_region(&inst)?;

	let v = if (inst.args().is_empty()) {
		return (Err(LcmdError::MissingArgs(
			"value argument is required",
		)));
	} else {
		parse_integer::<u64>(inst.args().trim())?
	};

	for_region_with(&region, |msr| {
		/*
		 * Safe because:
		 * 1. This crate is for use only with CPL = 0.
		 * 2. The user has guaranteed this MSR is writable.
		 * 3. The user has guaranteed the value to be written
		 * is allowed for this MSR.
		 */
		unsafe {
			msr::wrmsr(msr, v);
		}
	});

	Ok(LcmdValue::NONE)
}
