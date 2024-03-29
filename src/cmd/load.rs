/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use crate::pagetable_impl::{self, MappingDescriptor, MappingOp, PageMapFlags};
use core::convert::TryInto;
use core::mem;
use goblin::elf64::header as hdr;
use goblin::elf64::header::Header as EHdr;
use goblin::elf64::program_header as phdr;
use goblin::elf64::program_header::ProgramHeader as PHdr;
use goblin::elf64::section_header as shdr;
use nanobl_util::region_builder::{NativeRegion, RegionBuilder};
use nanobl_util::{mem_regions, pagetable::*};

pub(super) static LOAD_DEFN: LcmdDefn = LcmdDefn {
	name: "load",
	help: LcmdHelp {
		synopsis: Some("addr[,count]::load"),
		#[rustfmt::skip]
		desc: Some(concat!(
	"Interprets a 64-bit ELF image in memory at VA <addr>, treating at\n",
	"most <count> bytes as part of that image.  If <count> is not\n",
	"supplied, the default value is the number of bytes in the\n",
	"contiguous mappings beginning at <addr>.  The ELF object must be of\n",
	"the correct type, version, machine, and so forth for execution in a\n",
	"standalone environment on this machine.  All loadable segments will\n",
	"be loaded as specified by the ph_vaddr field or, if ph_vaddr is 0,\n",
	"as specified by ph_paddr.  Object images containing program headers\n",
	"that specify 0 for both ph_paddr and ph_vaddr are invalid and will\n",
	"not be loaded.  Otherwise, mappings from from ph_vaddr to ph_paddr\n",
	"are created as needed.  Control is not transferred; instead, the\n",
	"entry point from e_entry is displayed (if it lies within a loaded\n",
	"segment) and control returned to the bootloader.\n\n",
	"Output: the entry point, if one exists, is the output address\n",
	"parameter.  There is no output count.\n\n",
	"You may invoke the loaded program by passing the entry point to the\n",
	"_call_ lcmd.  Note that because _recv_, _load_, and _call_ are\n",
	"independent commands, you may place any number of objects in\n",
	"memory, optionally interpret and inspect them, and only then\n",
	"transfer control to one of them, optionally passing pointer(s) to\n",
	"the other(s) to your program.  This allows a great deal of\n",
	"flexibility in debugging both your software and this loader.\n\n",
	"Safety: The contents of the ELF headers are checked strictly;\n",
	"however, the destination addresses of loadable segments are checked\n",
	"only to ensure they do not overlap with the bootloader or the MMIO\n",
	"region.")),
		..LcmdHelp::new(
			&LOAD_DEFN,
			"interpret and load an ELF64 object",
		)
	},
	exec: load,
};

fn bound_offset(image: &NativeRegion<u8>, off: usize) -> Result<(), LcmdError> {
	if (off >= image.count) {
		Err(LcmdError::Range(
			"ELF64 image contains pointer outside its bounds",
		))
	} else {
		Ok(())
	}
}

/*
 * For things like arrays of headers, we want to make sure the last byte in the
 * group is within the image.  This collects the logic associated with doing so
 * while checking all intermediate calculations for overflow.
 */
fn bound_end_offset(
	image: &NativeRegion<u8>,
	start: usize,
	elemsz: usize,
	nelem: usize,
) -> Result<(), LcmdError> {
	if (nelem == 0 || elemsz == 0) {
		return (Ok(()));
	}

	let len = nelem
		.checked_mul(elemsz)
		.ok_or(LcmdError::Range("size of element group overflows"))?;
	let last_off = len.checked_sub(1).unwrap();
	let end = start
		.checked_add(last_off)
		.ok_or(LcmdError::Range("end of element group overflows"))?;

	bound_offset(image, end)
}

unsafe fn parse_header(image: &NativeRegion<u8>) -> Result<&EHdr, LcmdError> {
	let ehdr = EHdr::from_bytes(
		image.as_slice()[0 .. hdr::SIZEOF_EHDR].try_into().unwrap(),
	);

	if (&ehdr.e_ident[0 .. hdr::SELFMAG] != hdr::ELFMAG) {
		return (Err(LcmdError::BadObj("Bad ELF magic")));
	}

	if (ehdr.e_ident[hdr::EI_CLASS] != hdr::ELFCLASS64) {
		return (Err(LcmdError::BadObj("ELF image is not 64-bit")));
	}

	if (ehdr.e_ident[hdr::EI_DATA] != hdr::ELFDATA2LSB) {
		return (Err(LcmdError::BadObj(
			"ELF image is not little-endian",
		)));
	}

	if (ehdr.e_ident[hdr::EI_VERSION] != hdr::EV_CURRENT) {
		return (Err(LcmdError::BadObj("ELF ehdr version mismatch")));
	}

	/*
	 * XXX Current illumos tools build unix with ELFOSABI_SOLARIS, which
	 * is pretty definitely wrong.  We just ignore this instead for now.
	 *
	if (ehdr.e_ident[hdr::EI_OSABI] != hdr::ELFOSABI_NONE) {
		return (Err(LcmdError::BadObj(
			"ELF image specifies an OS ABI",
		)));
	}
	*/

	if (ehdr.e_type != hdr::ET_EXEC) {
		return (Err(LcmdError::BadObj(
			"ELF image is not of PT_EXEC type",
		)));
	}

	if (ehdr.e_machine != hdr::EM_X86_64) {
		/* EM_AMD64 */
		return (Err(LcmdError::BadObj(
			"ELF image machine type is not AMD64",
		)));
	}

	if (ehdr.e_version != hdr::EV_CURRENT.into()) {
		return (Err(LcmdError::BadObj("ELF image version mismatch")));
	}

	/* Ignore e_flags */

	if (usize::from(ehdr.e_ehsize) != hdr::SIZEOF_EHDR) {
		return (Err(LcmdError::BadObj(
			"ELF ehdr has incorrect ehdr size field",
		)));
	}

	if (usize::from(ehdr.e_phentsize) != phdr::SIZEOF_PHDR) {
		return (Err(LcmdError::BadObj(
			"ELF ehdr has incorrect phdr size field",
		)));
	}

	if (usize::from(ehdr.e_shentsize) != shdr::SIZEOF_SHDR) {
		return (Err(LcmdError::BadObj(
			"ELF ehdr has incorrect shdr size field",
		)));
	}

	if (ehdr.e_shstrndx >= ehdr.e_shnum) {
		return (Err(LcmdError::BadObj(
			"ELF ehdr strndx field out of bounds",
		)));
	}

	bound_offset(image, ehdr.e_shoff as usize)?;
	bound_end_offset(
		image,
		ehdr.e_shoff as usize,
		shdr::SIZEOF_SHDR,
		ehdr.e_shnum.into(),
	)
	.map_err(|_| {
		LcmdError::Range("excessive section headers in ELF image")
	})?;

	if (ehdr.e_phnum == 0) {
		return (Err(LcmdError::BadObj(
			"ELF image contains no phdrs; nothing to load",
		)));
	}

	bound_offset(image, ehdr.e_phoff as usize)?;
	bound_end_offset(
		image,
		ehdr.e_phoff as usize,
		phdr::SIZEOF_PHDR,
		ehdr.e_phnum.into(),
	)
	.map_err(|_| {
		LcmdError::Range("excessive program headers in ELF image")
	})?;

	Ok(ehdr)
}

unsafe fn get_seg_slice<'a>(
	image: &'a NativeRegion<u8>,
	ph: &PHdr,
) -> Result<&'a [u8], LcmdError> {
	bound_offset(image, ph.p_offset as usize)?;
	bound_end_offset(image, ph.p_offset as usize, 1, ph.p_filesz as usize)
		.map_err(|_| {
			LcmdError::Range("ELF program header size overflow")
		})?;

	let srcsize = core::cmp::min(ph.p_memsz, ph.p_filesz) as usize;

	Ok(image.as_slice()
		.split_at(ph.p_offset as usize)
		.1
		.split_at(srcsize)
		.0)
}

fn map_segment(
	ph: &PHdr,
	respect_flags: bool,
) -> Result<NativeRegion<u8>, LcmdError> {
	let vbase = if (ph.p_vaddr == 0) {
		ph.p_paddr
	} else {
		ph.p_vaddr
	};
	let pbase = ph.p_paddr;

	if (vbase == 0) {
		return (Err(LcmdError::Range(
			"ELF image contains program header with no \
			valid destination address",
		)));
	}

	let lpage_mask = PageSize::Size2M.as_offset_mask();

	let page_size = if ((ph.p_vaddr & lpage_mask) != 0 ||
		(ph.p_paddr & lpage_mask) != 0)
	{
		PageSize::Size4K
	} else {
		PageSize::Size2M
	};

	let virt_region = RegionBuilder::new(
		Some(ph.p_vaddr),
		Some(ph.p_memsz),
	)
	.align_count_up_to(page_size.as_size() as u64)
	.bound_region_in_any(
		&[&mem_regions::VIRT_RANGE_LOW, &mem_regions::VIRT_RANGE_HIGH],
		Some("loadable segment destination virtual address is \
		non-canonical"),
	)
	.bound_address_p2align(
		page_size.as_size() as u64,
		Some("virtual address is not aligned to a page boundary"),
	)
	.forbid_region_overlap(
		&mem_regions::MMIO_RANGE,
		Some("loadable segment virtual destination overlaps with \
		     the MMIO region"),
	)
	.forbid_region_overlap(
		&mem_regions::program_range(),
		Some("loadable segment virtual destination overlaps with \
		     the bootloader"),
	)
	.into_native_region()?;

	let _ = RegionBuilder::new(
		Some(ph.p_paddr),
		Some(ph.p_memsz),
	).bound_region_in(
		&mem_regions::PHYS_RANGE,
		Some("physical address is out of range")
	).bound_address_p2align(
		page_size.as_size() as u64,
		Some("physical address is not aligned to a page boundary"),
	).forbid_region_overlap(
		&mem_regions::MMIO_RANGE,
		Some("loadable segment physical destination overlaps with \
			the MMIO region")
	).forbid_region_overlap(
		&mem_regions::program_range(),
		Some("loadavle segment physical destination overlaps with \
			the bootloader")
	).into_region()?;

	let flags = if (respect_flags) {
		const RWX: u32 = phdr::PF_X | phdr::PF_W | phdr::PF_R;
		const RW: u32 = phdr::PF_W | phdr::PF_R;
		const RX: u32 = phdr::PF_X | phdr::PF_R;
		const WX: u32 = phdr::PF_X | phdr::PF_W;

		PageMapFlags::G |
			PageMapFlags::SW_LOW_2 | match (ph.p_flags &
			(phdr::PF_X | phdr::PF_W | phdr::PF_R))
		{
			RWX | WX => PageMapFlags::P | PageMapFlags::RW,
			RX | phdr::PF_X => PageMapFlags::P,
			RW | phdr::PF_W => {
				PageMapFlags::P |
					PageMapFlags::RW | PageMapFlags::NX
			}
			phdr::PF_R => PageMapFlags::P | PageMapFlags::NX,
			_ => PageMapFlags::NX,
		}
	} else {
		PageMapFlags::P | PageMapFlags::RW
	};

	let md = MappingDescriptor {
		op: MappingOp::Iterate,
		size: page_size,
		base: pbase.into(),
		flags,
	};
	pagetable_impl::map_region(&virt_region, &md)?;

	Ok(virt_region)
}

fn load(inst: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	let mut entry: Option<u64> = None;

	let mapped = if let Some(addr) = inst.addr() {
		let off = addr & PageSize::Size4K.as_offset_mask();
		let sz = pagetable_impl::find_size_of_mapped_region(addr.into())
			as u64;

		sz.saturating_sub(off)
	} else {
		0
	};

	let image: NativeRegion<u8> = RegionBuilder::new(
		inst.addr(),
		inst.count(),
	)
	.default_count(mapped)
	.forbid_count_in(
		&(0u64 .. hdr::SIZEOF_EHDR as u64),
		Some("image region is too small to contain an ELF header"),
	)
	.forbid_region_overlap(
		&mem_regions::MMIO_RANGE,
		Some("image region overlaps with the MMIO region"),
	)
	.forbid_region_overlap(
		&mem_regions::program_range(),
		Some("image region overlaps with the bootloader"),
	)
	.bound_region_with(pagetable_impl::require_mapped_bytes)
	.bound_address_p2align(
		mem::align_of::<EHdr>() as u64,
		Some("image region is insufficiently aligned"),
	)
	.into_native_region()?;

	let ehdr = unsafe { parse_header(&image)? };
	let ph_bytes =
		unsafe { image.as_slice().split_at(ehdr.e_phoff as usize).1 };
	let phdrs = unsafe {
		core::slice::from_raw_parts(
			ph_bytes.as_ptr() as *const PHdr,
			core::cmp::min(
				ph_bytes.len() / phdr::SIZEOF_PHDR,
				ehdr.e_phnum as usize,
			),
		)
	};

	for ph in phdrs {
		if (ph.p_type != phdr::PT_LOAD) {
			continue;
		}

		/*
		 * There's nothing to do if memsz is 0.  Not so if filesz is
		 * 0, because then we're going to clear the range.
		 */
		if (ph.p_memsz == 0) {
			continue;
		}

		/*
		 * Safe because of our construction of image, and the
		 * guarantees made about it by the user.
		 */
		let src_bytes = unsafe { get_seg_slice(&image, ph)? };

		let mut dst_region = map_segment(ph, false)?;

		/*
		 * map_segment() guarantees that this region is mapped, which
		 * also guarantees dereferenceability and alignment of the
		 * virtual address.  Use of the physical region it's mapped to
		 * is safe only if the image supplied by the user can run on
		 * this computer.  We have no further means of verifying that.
		 */
		let dst_bytes = unsafe { dst_region.as_mut_slice() };
		dst_bytes[0 .. src_bytes.len()].copy_from_slice(src_bytes);

		if (ph.p_filesz < ph.p_memsz) {
			/*
			 * Safety: No.  See previous notes.
			 */
			let seg_zero =
				dst_bytes.split_at_mut(src_bytes.len()).1;
			seg_zero.fill(0);
		}

		map_segment(ph, true)?;

		if (dst_region.as_range().contains(&(ehdr.e_entry as usize))) {
			entry = Some(ehdr.e_entry);
		}
	}

	if let Some(eaddr) = entry {
		Ok(LcmdValue {
			addr: Some(eaddr),
			count: None,
		})
	} else {
		Err(LcmdError::Range(
			"ELF image entry point does not fall \
				     within any loaded segment",
		))
	}
}
