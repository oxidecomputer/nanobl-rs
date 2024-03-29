/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use crate::pagetable_impl::{self, MappingDescriptor, MappingOp, PageMapFlags};
use nanobl_util::region_builder::{NativeRegion, RegionBuilder};
use nanobl_util::{mem_regions, pagetable::*, prelude::*};
use x86::bits64::paging::PAddr;

pub(super) static MAP_DEFN: LcmdDefn = LcmdDefn {
	name: "map",
	help: LcmdHelp {
		synopsis: Some(
			"vaddr[,count]::map [-1|-2|-3] [-F] [-f ...] paddr",
		),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Create <count> mappings for the virtual region beginning at\n",
	"<vaddr>.  The target of the mapping at <vaddr> + P * S is:\n\n",
	"	<paddr> + P * S, if -F is not supplied (the default)\n",
	"	<paddr>, if -F is supplied\n\n",
	"for all P in [0, count), where S is the page size in bytes.\n",
	"<count> specifies the number of *pages* to map; if elided, 1 is\n",
	"assumed.  The size S of each mapping is specified by the optional\n",
	"flags -1, -2, or -3, which are mutually exclusive and specify the\n",
	"level of the pagetable at which to create the mapping.  -1, the\n",
	"default, creates 4 KiB mappings, -2 2 MiB mappings, and -3 1 GiB\n",
	"mappings.\n\n",
	"AMD64 architectural limits are enforced: the entire range\n",
	"[vaddr, vaddr + count * S must be canonically addressable, every\n",
	"target physical address must have bits 63:52 clear, and both\n",
	"<vaddr> and <paddr> must lie on the requested page size boundary.\n",
	"All flag bits and reserved bits must be clear in <paddr>.\n\n",
	"Mapping flags may be set by supplying the -f option, which must be\n",
	"followed by a comma-separated list of tokens representing flags to\n",
	"set or clear.  The tokens are: rw, us, pwt, pcd, a, d, g, pat, nx,\n",
	"!p, and the integers 9 through 11 and 52 through 62 representing\n",
	"the software bits at those bit positions in all table entries.\n",
	"Unless !p is supplied, the present (P) bit will be set; see also\n",
	"the ::unmap command for removing entire mappings.  PS will be set\n",
	"according to the specified size and cannot otherwise be changed.\n",
	"The PCD bit will always be set if the target of the mapping lies\n",
	"within MMIO space, and cannot be turned off.\n\n",
	"Higher-level pagetables are constructed for you as needed and\n",
	"linked into the existing collection.  If a page must be allocated\n",
	"to store a new pagetable, it will be taken from a fixed-size arena\n",
	"the size and location of which may be inspected using ::mappings.\n",
	"If no pagetable pages are available in this region, this command\n",
	"will fail; if you need more pagetables, load a program and have it\n",
	"construct them for you.\n\n",
	"Output: None.\n\n",
	"Safety: Mappings covering the bootloader, its stack, the console\n",
	"UART registers, and the pagetables themselves cannot be unmapped or\n",
	"replaced.  Mappings are not checked for aliasing; if you create a\n",
	"mapping that aliases the region in which the bootloader is stored,\n",
	"your access to the alias pages is subject to the same restrictions\n",
	"as the identity mapping; however, other lcmds cannot enforce them.\n",
	"Successful creation or modification of any mapping is guaranteed to\n",
	"be followed by the execution of appropriate TLB invalidation\n",
	"instructions.")),
		..LcmdHelp::new(&MAP_DEFN, "create or alter memory mappings")
	},
	exec: map,
};

pub(super) static UNMAP_DEFN: LcmdDefn = LcmdDefn {
	name: "unmap",
	help: LcmdHelp {
		synopsis: Some("vaddr[,count]::unmap [-1|-2|-3]"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Removes mappings created by ::map for the virtual region beginning\n",
	"at <vaddr>.  <count> mappings of the same page size are\n",
	"torched; if elided, 1 is assumed.  The optional arguments -1, -2,\n",
	"and -3 are interpreted as by ::map.  If the mapping consists of a\n",
	"pointer to a lower-level pagetable, the entire region is unmapped;\n",
	"that is, large page splitting is not implemented.  If any part of\n",
	"the region consists of page(s) larger than the size specified, the\n",
	"entire pagetable(s) mapping the smaller pages are destroyed.\n",
	"Mappings are destroyed by clearing their entire contents to 0.\n",
	"Pagetables consisting entirely of 0 entries will be unliked and\n",
	"freed into the pagetable arena.\n\n",
	"Output: None.\n\n",
	"Safety: See notes for ::map.")),
		..LcmdHelp::new(&UNMAP_DEFN, "remove memory mappings")
	},
	exec: unmap,
};

pub(super) static MAPPINGS_DEFN: LcmdDefn = LcmdDefn {
	name: "mappings",
	help: LcmdHelp {
		synopsis: Some("::mappings [-a]"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"With no options, displays the current pagetables, including the\n",
	"default identity mappings and any that have subsequently been\n",
	"created. With -a, instead displays information about the loader's\n",
	"internal pagetable arena management.  Neither operation modifies\n",
	"the pagetables nor the TLB.\n\n",
	"Output: None.\n\n",
	"Safety: This command is safe.")),
		..LcmdHelp::new(&MAPPINGS_DEFN, "display memory mappings")
	},
	exec: show,
};

pub(super) static IS_MAPPED_DEFN: LcmdDefn = LcmdDefn {
	name: "ismapped",
	help: LcmdHelp {
		synopsis: Some("vaddr[,count]::ismapped [-l]"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"For the region containing <count> bytes (default 1) beginning at\n",
	"<vaddr>, indicates whether every byte of the region is mapped.\n",
	"If the optional -l flag is supplied, <count> is ignored and instead\n",
	"the length of the mapped region beginning at the base of the page\n",
	"containing <vaddr> is displayed.\n\n",
	"Output: with -l, the supplied address and number of bytes in the\n",
	"contiguously-mapped region are the output address and count.\n\n",
	"Safety: This command is safe.")),
		..LcmdHelp::new(&IS_MAPPED_DEFN, "diagnose memory mappings")
	},
	exec: is_mapped,
};

fn bit_to_flag(bitnum: u8) -> Result<PageMapFlags, LcmdError> {
	match (bitnum) {
		9 ..= 11 => Ok(PageMapFlags::from_bits(
			1u64 << (bitnum - 9 + PageMapFlags::SW_LOW_FIRSTBIT),
		)
		.unwrap()),
		52 ..= 62 => Ok(PageMapFlags::from_bits(
			1u64 << (bitnum - 52 + PageMapFlags::SW_HIGH_FIRSTBIT),
		)
		.unwrap()),
		_ => Err(LcmdError::BadArgs(
			"integer flag out of range [9,11] or [52,62]",
		)),
	}
}

/*
 * Translate a string representing a comma-separated list of page mapping
 * flags into the set of flags representing them.  To support multiple -f
 * arguments, we want to be able to OR the resulting flags together to produce
 * the final output; to that end, because P is set by default, we *set* P in
 * our return value if *!p* is in the string.  The caller will combine all
 * arguments and then toggle P before using the flags.
 */
fn xlate_flags(s: &str) -> Result<PageMapFlags, LcmdError> {
	let mut out = PageMapFlags::empty();

	for sf in s.split(',') {
		match (sf.trim()) {
			"!p" => out.insert(PageMapFlags::P),
			"rw" => out.insert(PageMapFlags::RW),
			"us" => out.insert(PageMapFlags::US),
			"pwt" => out.insert(PageMapFlags::PWT),
			"pcd" => out.insert(PageMapFlags::PCD),
			"a" => out.insert(PageMapFlags::A),
			"d" => out.insert(PageMapFlags::D),
			"g" => out.insert(PageMapFlags::G),
			"pat" => out.insert(PageMapFlags::PAT),
			"nx" => out.insert(PageMapFlags::NX),
			bit => {
				if let Ok(bitnum) = bit.parse::<u8>() {
					out.insert(bit_to_flag(bitnum)?);
				} else {
					return (Err(LcmdError::BadArgs(
						"unrecognised flag string",
					)));
				}
			}
		}
	}

	Ok(out)
}

fn map(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let mut size = PageSize::Size4K;
	let mut pa_arg = None;
	let mut flags = PageMapFlags::empty();
	let mut in_opt_f = false;
	let mut opt_F = false;

	for arg in inst.args().split_whitespace().fuse() {
		match (arg.trim()) {
			"-1" => size = PageSize::Size4K,
			"-2" => size = PageSize::Size2M,
			"-3" => size = PageSize::Size1G,
			"-f" => in_opt_f = true,
			"-F" => opt_F = true,
			s => {
				if (in_opt_f) {
					flags |= xlate_flags(s)?;
					in_opt_f = false;
				} else if (pa_arg.is_some()) {
					return (Err(LcmdError::ExtraArgs(
					"extraneous or invalid argument")));
				} else {
					pa_arg = Some(parse_integer::<u64>(s)?);
				}
			}
		}
	}

	let pbase = if let Some(x) = pa_arg {
		/* XXX this should be self-discoverable */
		if ((x & size.as_offset_mask()) != 0 ||
			(x & 0xFFF0_0000_0000_0000) != 0)
		{
			return (Err(LcmdError::BadArgs(
				"paddr contains invalid flags or address bits",
			)));
		} else {
			PAddr(x)
		}
	} else {
		return (Err(LcmdError::MissingArgs(
			"the paddr argument is required",
		)));
	};

	flags.toggle(PageMapFlags::P);

	let multiplier = size.as_size();
	let region: NativeRegion<u8> = RegionBuilder::new(
		inst.addr(),
		inst.count(),
	)
	.default_count(1)
	.multiply_count_by(
		multiplier as u64,
		Some("pages can't fit in the available address space"),
	)
	.bound_address_p2align(
		multiplier as u64,
		Some("virtual address is not aligned to a page boundary"),
	)
	.bound_region_in_any(
		&[&mem_regions::VIRT_RANGE_LOW, &mem_regions::VIRT_RANGE_HIGH],
		Some("virtual address out of bounds"),
	)
	.forbid_region_overlap(
		&mem_regions::program_range(),
		Some("virtual address overlaps the bootloader"),
	)
	.into_native_region()?;

	/*
	 * Let's recap:
	 *
	 * - region represents the virtual region to be mapped
	 * - region is guaranteed to fit, to be properly aligned, and not to
	 *   overlap with the loader, but hasn't been checked for overlap with
	 *   any existing mappings of a larger size
	 * - entry contains the start of the physical region and the flags
	 * - the address portion of entry is guaranteed to fit
	 * - the flags in entry have been set (mostly by the user, some by us)
	 * - size is the page size to use
	 *
	 * This satisfies all the prerequisites, so we call into the pagetable
	 * library to map the region.
	 */

	let md = MappingDescriptor {
		op: if (opt_F) {
			MappingOp::Fixed
		} else {
			MappingOp::Iterate
		},
		size,
		base: pbase,
		flags,
	};

	pagetable_impl::map_region(&region, &md)?;

	Ok(LcmdValue::NONE)
}

fn unmap(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let mut size = PageSize::Size4K;

	for arg in inst.args().split_whitespace().fuse() {
		match (arg.trim()) {
			"-1" => size = PageSize::Size4K,
			"-2" => size = PageSize::Size2M,
			"-3" => size = PageSize::Size1G,
			_ => {
				return (Err(LcmdError::ExtraArgs(
					"extraneous or invalid argument",
				)));
			}
		}
	}

	let multiplier = size.as_size();
	let region: NativeRegion<u8> = RegionBuilder::new(
		inst.addr(),
		inst.count(),
	)
	.default_count(1)
	.multiply_count_by(
		multiplier as u64,
		Some("pages can't fit in the available address space"),
	)
	.bound_address_p2align(
		multiplier as u64,
		Some("virtual address is not aligned to a page boundary"),
	)
	.bound_region_in_any(
		&[&mem_regions::VIRT_RANGE_LOW, &mem_regions::VIRT_RANGE_HIGH],
		Some("virtual address out of bounds"),
	)
	.forbid_region_overlap(
		&mem_regions::program_range(),
		Some("virtual address overlaps the bootloader"),
	)
	.forbid_region_overlap(
		&mem_regions::UART_RANGE,
		Some("virtual address overlaps essential UART registers"),
	)
	.into_native_region()?;

	pagetable_impl::unmap_region(&region);

	Ok(LcmdValue::NONE)
}

fn show(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let mut show_arena = false;

	if (inst.addr().is_some() || inst.count().is_some()) {
		return (Err(LcmdError::ExtraArgs(
			"address and count are not accepted by this command",
		)));
	}

	for arg in inst.args().split_whitespace().fuse() {
		match (arg.trim()) {
			"-a" => show_arena = true,
			_ => {
				return (Err(LcmdError::BadArgs(
					"unknown or extraneous argument",
				)));
			}
		}
	}

	if (show_arena) {
		pagetable_impl::dump_arena();
	} else {
		pagetable_impl::dump_mappings();
	}

	Ok(LcmdValue::NONE)
}

fn is_mapped(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let mut show_length = false;

	for arg in inst.args().split_whitespace().fuse() {
		match (arg.trim()) {
			"-l" => show_length = true,
			_ => {
				return (Err(LcmdError::BadArgs(
					"unknown or extraneous argument",
				)));
			}
		}
	}

	if (show_length) {
		if let Some(addr) = inst.addr() {
			let sz = pagetable_impl::find_size_of_mapped_region(
				addr.into(),
			);
			return (Ok(LcmdValue {
				addr: Some(addr),
				count: Some(sz as u64),
			}));
		} else {
			return (Err(LcmdError::MissingArgs(
				"address is required with -l",
			)));
		}
	}

	let region: NativeRegion<u8> = RegionBuilder::new(
		inst.addr(),
		inst.count(),
	)
	.default_count(1)
	.forbid_count_in(&(0u64 ..= 0u64), Some("nothing to do (count is 0)"))
	.bound_region_in_any(
		&[&mem_regions::VIRT_RANGE_LOW, &mem_regions::VIRT_RANGE_HIGH],
		Some("region contains noncanonical virtual addresses"),
	)
	.into_native_region()?;

	if (pagetable_impl::is_range_mapped(&region.as_range())) {
		println!("true");
	} else {
		println!("false");
	}

	Ok(LcmdValue::NONE)
}
