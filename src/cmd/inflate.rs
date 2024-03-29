/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use crate::pagetable_impl::{self, MappingDescriptor, MappingOp, PageMapFlags};
use miniz_oxide::inflate::core::decompress;
use miniz_oxide::inflate::core::inflate_flags::TINFL_FLAG_PARSE_ZLIB_HEADER;
use miniz_oxide::inflate::core::DecompressorOxide;
use miniz_oxide::inflate::TINFLStatus;
use nanobl_util::mem_regions;
use nanobl_util::pagetable::{self, PageSize, SizeEnum};
use nanobl_util::region_builder::RegionBuilder;

pub(super) static INFLATE_DEFN: LcmdDefn = LcmdDefn {
	name: "inflate",
	help: LcmdHelp {
		synopsis: Some("src,len::inflate dst"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Inflates compressed data at some region to some destination.\n",
	"Output: The decompressed image will be written into a newly\n",
	"mapped region.  ZLIB is used.\n",
	"\n",
	"Safety: You must ensure that [src..src+len] contains a ZLIB\n",
	"compressed image that is valid and safe to read, and that\n",
	"dst is an address pointing to enough RAM to contain the\n",
	"decompressed image.")),
		..LcmdHelp::new(&INFLATE_DEFN, "decompress data")
	},
	exec: inflate,
};

fn inflate(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	const DEFAULT_INFLATE_DST_SIZE: u64 = 128 * 1024 * 1024;

	let mapped = if let Some(addr) = inst.addr() {
		let off = addr & PageSize::Size4K.as_offset_mask();
		let sz = pagetable_impl::find_size_of_mapped_region(addr.into())
			as u64;

		sz.saturating_sub(off)
	} else {
		0
	};

	let sr = RegionBuilder::new(inst.addr(), inst.count())
		.default_count(mapped)
		.forbid_count_in(&(0u64 ..= 0u64), Some("source is empty"))
		.forbid_region_overlap(
			&mem_regions::MMIO_RANGE,
			Some("source overlaps with the MMIO region"),
		)
		.forbid_region_overlap(
			&mem_regions::program_range(),
			Some("source overlaps with the bootloader"),
		)
		.bound_region_with(pagetable_impl::require_mapped_bytes)
		.into_native_region()?;
	let src = &unsafe { sr.as_slice() }[sr.addr_off ..];
	let srange = sr.as_range();
	let srange = (*srange.start() as u64) ..= (*srange.end() as u64);

	let paddr = parse_integer(inst.args().trim())?;

	let mut dr = RegionBuilder::new(Some(paddr), None)
		.default_count(DEFAULT_INFLATE_DST_SIZE)
		.align_address_down_to(PageSize::Size4K.as_size() as u64)
		.align_count_up_to(PageSize::Size4K.as_size() as u64)
		.forbid_region_overlap(
			&mem_regions::MMIO_RANGE,
			Some("destination overlaps with the MMIO region"),
		)
		.forbid_region_overlap(
			&mem_regions::program_range(),
			Some("destination overlaps with the bootloader"),
		)
		.forbid_region_overlap(
			&srange,
			Some("destination overlaps source"),
		)
		.into_native_region()?;

	let range = dr.as_range();
	let page_size = pagetable::get_page_size(&range);
	let phys_base = *range.start() as u64;
	let flags = PageMapFlags::RW | PageMapFlags::P;

	let md = MappingDescriptor {
		op: MappingOp::Iterate,
		size: page_size,
		base: phys_base.into(),
		flags,
	};
	pagetable_impl::map_region(&dr, &md)?;

	let dst = &mut unsafe { dr.as_mut_slice() }[dr.addr_off ..];

	let mut r = DecompressorOxide::new();
	let flags = TINFL_FLAG_PARSE_ZLIB_HEADER;
	let (s, _, o) = decompress(&mut r, src, dst, 0, flags);
	if s != TINFLStatus::Done {
		return Err(LcmdError::BadObj("bad compressed image"));
	}

	/*
	 * TODO: We could unmap the source region and any slop at the end of the
	 * destination region, but honestly I can't see much of a point.
	 */

	Ok(LcmdValue {
		addr: Some(paddr),
		count: Some(o as u64),
	})
}
