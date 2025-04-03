/*
 * Copyright 2025 Oxide Computer Company
 */

use bitflags::bitflags;
use core::arch::asm;
use core::cmp;
use core::ops::RangeInclusive;
use core::ptr;
use nanobl_util::mem_regions;
use nanobl_util::pagetable::*;
use nanobl_util::prelude::*;
use nanobl_util::region_builder::NativeRegion;
use nanobl_util::tedium64::*;
use x86::bits64::paging::{
	pd_index, pdpt_index, pml4_index, pt_index, PAddr, VAddr,
};

/*
 * This file contains pagetable manipulation code that is specific to the
 * loader.  See pagetable.rs for generic amd64 pagetable functionality.
 *
 * The layout of the loader's pagetable space is established in start.s along
 * with the link-editor configuration.  This code relies on symbols provided by
 * that code but not their values; this allows the pagetable link address,
 * consumed size, and available space to change without needing this code to
 * change, provided the meanings of these three symbols remain the same.
 *
 * __PAGETABLE_SPACE is the total number of bytes in the embedded pagetable
 * region.  We do not assume anything about it; it may be zero or something not
 * a multiple of the pagetable page size.
 *
 * __spagetable is the address of the first embedded pagetable page.  It is
 * guaranteed to be non-NULL, properly aligned, and identity-mapped.  Note that
 * this page, and possibly some number of subsequent pages, is generally
 * expected to be in use by skeleton pagetables providing identity mappings,
 * though we do not assume so (those could reside elsewhere).
 *
 * __epagetable is the base address of the first page following the last _used_
 * pagetable in the embedded pagetable space.  There is no guarantee that it is
 * a lower address than __spagetable + __PAGETABLE_SPACE, but it cannot be
 * higher; the minimal case is __spagetable == __epagetable with
 * __PAGETABLE_SPACE == 0.
 *
 * Thus, the region between __epagetable and __spagetable + __PAGETABLE_SPACE is
 * available for us to use for additional pagetable pages.  We do not rely on
 * the size of that region, which may be zero, only on it being identity-mapped
 * as part of the preconditions of this program's execution.
 */

#[derive(Debug)]
pub enum Error {
	NoMemory,
	LargerMappingExists,
}

const RESERVED_PAGETABLES: usize = 2;
static mut SPAGETABLE: usize = 0;
static mut EPAGETABLE: usize = 0;
static mut PAGETABLE_SPACE: usize = 0;
static mut FREEMAP: Mask64 = Mask64::NONE;

unsafe fn init_pagetables() {
	let root = RootPtr::current();
	let spagetable: usize;
	let epagetable: usize;
	let space: usize;
	const PAGE: PageSize = PageSize::Size4K;

	asm!(
		"movq	$__spagetable, {}",
		"movq	$__epagetable, {}",
		"movq	$__PAGETABLE_SPACE, {}",
		out(reg) spagetable,
		out(reg) epagetable,
		out(reg) space,
		options(nomem, preserves_flags, nostack, att_syntax)
	);

	/*
	 * See block comment above.  The first two of these assertions should
	 * be guaranteed by the link-editor script at build time.  The third
	 * is part of our contract with the bootblock; it cannot be checked
	 * statically and if it fails we are all but certain to lock up the
	 * processor very soon so we may as well blow up now.
	 */
	assert_eq!(spagetable & PAGE.as_offset_mask(), 0);
	assert!(spagetable + space >= epagetable);
	assert_eq!(spagetable, root.child_paddr().unwrap().0 as usize);

	SPAGETABLE = spagetable;
	EPAGETABLE = epagetable;
	PAGETABLE_SPACE = space;

	let freebase =
		(epagetable + (PAGE.as_size() - 1)) & PAGE.as_base_mask();
	let end = (spagetable + space) & PAGE.as_base_mask();

	/*
	 * There aren't any free pagetable pages available.  This should never
	 * happen and we won't be able to load anything, but we can at least
	 * keep going so the user can see what's wrong.
	 */
	if (end <= freebase) {
		FREEMAP = Mask64::NONE;
		return;
	}

	/*
	 * The first 2 free pages are reserved for use by the program we load,
	 * allowing it either to create a VA window to map and modify its own
	 * pagetables or to map some register space it wants to play with.
	 */
	let pages = cmp::min((end - spagetable) as u64 >> PAGE.as_shift(), 64);
	let usedpages = ((freebase - spagetable) as u64 >> PAGE.as_shift()) +
		RESERVED_PAGETABLES as u64;

	if (usedpages >= pages) {
		FREEMAP = Mask64::NONE;
		return;
	}

	FREEMAP = Mask64::bits_range(
		&(Shift64::new(usedpages) ..= Shift64::new(pages - 1)),
	);
}

/*
 * XXX pagetable pages must be pinned
 */
fn alloc_pagetable_page<T: PageTable>() -> Result<*mut T, Error>
where
	T::Entry: Sized,
{
	/*
	 * This entire evolution is safe because:
	 *
	 * 1. we are single-threaded and
	 *
	 * 2. the entire pagetable region lies within the program's own
	 *    identity-mapped block of memory and
	 *
	 * 3. these extern symbols exist and have the values described above
	 *    and enforced by assertions
	 */
	assert_eq!(core::mem::size_of::<T>(), PageSize::Size4K.as_size());

	unsafe {
		if (SPAGETABLE == 0) {
			init_pagetables();
		}

		let map = FREEMAP;
		if let Some(freeidx) = map.lowest_bit() {
			FREEMAP &= !Mask64::bit(freeidx);
			let pageptr = (SPAGETABLE +
				((freeidx.count() as usize) <<
					PageSize::Size4K.as_shift())) as *mut T;
			ptr::write_bytes(pageptr, 0, 1);
			Ok(pageptr.as_mut().unwrap())
		} else {
			Err(Error::NoMemory)
		}
	}
}

/*
 * XXX This isn't right at all, really.  We want to impl Drop instead.
 */
fn free_pagetable_page<T: PageTable>(page: *mut T)
where
	T::Entry: Sized,
{
	let va = page as usize;
	let idx;
	let sz = PageSize::Size4K;

	unsafe {
		assert!(va >= SPAGETABLE);
		assert!(va <= SPAGETABLE + PAGETABLE_SPACE);
		assert_eq!(va & sz.as_offset_mask(), 0);
		idx = (va - SPAGETABLE) >> sz.as_shift();

		let freemask = Mask64::bit(idx);
		assert_eq!(FREEMAP & freemask, Mask64::NONE);
		FREEMAP |= freemask;
	}
}

fn invlpg(vaddr: *const u8) {
	/*
	 * Safe because CPL = 0.
	 */
	unsafe {
		asm!("invlpg ({})",
		in(reg) vaddr,
		options(nostack, preserves_flags, att_syntax));
	}
}

/*
 * Why not impl Deref instead?  We'd like to!  But the dereferencing operation
 * is implementation-dependent, while the pagetable structures themselves are
 * generic and live in a different crate.  The orphan rule prohibits us from
 * doing it, and the implementation-specific nature of pagetable mapping makes
 * it incorrect to implement in generic code.  Instead, this.  A fourth (or
 * fifth) look at x86_64's Mapper is probably in order.
 */
unsafe fn pte_deref<T: PageTableEntry>(entry: &T) -> &T::Target {
	assert!(is_safe(entry));

	let paddr = entry.child_paddr().unwrap().as_usize();
	(paddr as *const T::Target).as_ref().unwrap()
}

unsafe fn pte_deref_mut<T: PageTableEntry>(entry: &mut T) -> &mut T::Target {
	assert!(is_safe(entry));

	let paddr = entry.child_paddr().unwrap().as_usize();
	(paddr as *mut T::Target).as_mut().unwrap()
}

fn is_safe<T: PageTableEntry>(entry: &T) -> bool {
	entry.is_page() ||
		mem_regions::program_range()
			.contains(&entry.child_paddr().unwrap().into())
}

/*
 * If entry points to a pagetable, recursively remove that pagetable and all
 * its children.  Then, regardless, set the value of this mapping to 0.
 */
fn clear_mapping<T: PageTableEntry>(entry: &mut T, va: VAddr) {
	if (!entry.is_page() && !is_safe(entry)) {
		return;
	}

	if (!entry.is_page()) {
		let child = unsafe { pte_deref_mut(entry) };
		let iter = child.iter_mut();

		for child_entry in iter {
			clear_mapping(
				child_entry,
				va + <T::Target as PageTable>::Entry::MAP_SIZE
					.as_size(),
			);
		}

		free_pagetable_page(child as *mut T::Target);
	}

	*entry = T::new(PAddr(0), T::Flags::zero());
	invlpg(va.as_usize() as *const u8);
}

/*
 * These definitions are the union of all the things one can ask for on a page
 * (not a child pagetable, necessarily) of one size or another.  Their values
 * are chosen deliberately not to match any real location in pagetable entries;
 * this is an abstraction only and never goes anywhere near hardware.
 */
bitflags! {
	pub struct PageMapFlags: u64 {
		const P = 0x2000;
		const RW = 0x4000;
		const US = 0x8000;
		const PWT = 0x1_0000;
		const PCD = 0x2_0000;
		const A = 0x4_0000;
		const D = 0x8_0000;
		const G = 0x10_0000;
		const PAT = 0x20_0000;
		const SW_LOW_0 = 0x40_0000;
		const SW_LOW_1 = 0x80_0000;
		const SW_LOW_2 = 0x100_0000;
		const SW_HIGH_0 = 0x200_0000;
		const SW_HIGH_1 = 0x400_0000;
		const SW_HIGH_2 = 0x800_0000;
		const SW_HIGH_3 = 0x1000_0000;
		const SW_HIGH_4 = 0x2000_0000;
		const SW_HIGH_5 = 0x4000_0000;
		const SW_HIGH_6 = 0x8000_0000;
		const SW_HIGH_7 = 0x1_0000_0000;
		const SW_HIGH_8 = 0x2_0000_0000;
		const SW_HIGH_9 = 0x4_0000_0000;
		const SW_HIGH_10 = 0x8_0000_0000;
		const NX = 0x10_0000_0000;

		const SW_LOW = 0x1C0_0000;
		const SW_HIGH = 0xF_FE00_0000;
	}
}

impl PageMapFlags {
	pub const SW_LOW_FIRSTBIT: u8 = 22;
	pub const SW_HIGH_FIRSTBIT: u8 = 25;
}

macro_rules! map_flag {
	($sf:ident, $src:expr, $df:ident, $dst:expr) => {
		if ($src.contains(PageMapFlags::$sf)) {
			$dst.insert(Self::$df);
		}
	};
}

macro_rules! identity_map_flag {
	($f:ident, $src:expr, $dst:expr) => {
		map_flag!($f, $src, $f, $dst);
	};
}

#[rustfmt::skip]
macro_rules! impl_flags_from {
	($t:ty) => {
		impl From<PageMapFlags> for $t {
			fn from(v: PageMapFlags) -> Self {
				assert_eq!(
					(v & PageMapFlags::all()).bits(),
					v.bits()
				);

				let mut out = Self::empty();

				identity_map_flag!(P, v, out);
				identity_map_flag!(RW, v, out);
				identity_map_flag!(US, v, out);
				identity_map_flag!(PWT, v, out);
				identity_map_flag!(PCD, v, out);
				identity_map_flag!(A, v, out);
				identity_map_flag!(D, v, out);
				identity_map_flag!(G, v, out);
				identity_map_flag!(PAT, v, out);
				identity_map_flag!(NX, v, out);

				let sw_low = (v & PageMapFlags::SW_LOW)
					.bits() >>
					PageMapFlags::SW_LOW_FIRSTBIT;
				let sw_high = (v & PageMapFlags::SW_HIGH)
					.bits() >>
					PageMapFlags::SW_HIGH_FIRSTBIT;

				/*
				 * These "magic" constants should perhaps be
				 * discoverable, but they are architecturally
				 * guaranteed for the upstream hardware we
				 * support.
				 */
				out |= Self::from_bits(sw_low << 9).unwrap();
				out |= Self::from_bits(sw_high << 52).unwrap();

				assert_eq!(
					(out & Self::all()).bits(),
					out.bits()
				);

				out
			}
		}
	};
}

impl_flags_from!(PdpeFlags);
impl_flags_from!(PdeFlags);
impl_flags_from!(PteFlags);

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub enum MappingOp {
	Fixed,
	Iterate,
}

#[derive(Debug)]
pub struct MappingDescriptor {
	pub op: MappingOp,
	pub size: PageSize,
	pub base: PAddr,
	pub flags: PageMapFlags,
}

pub fn map_region(
	r: &NativeRegion,
	md: &MappingDescriptor,
) -> Result<(), Error> {
	/*
	 * Both the physical and virtual base addresses must be properly
	 * aligned with the page size requested.  To do otherwise is a
	 * programming error.
	 */
	assert_eq!((md.base & md.size.as_offset_mask()), 0u64.into());
	assert_eq!((r.addr & md.size.as_offset_mask()), 0usize);
	assert_eq!((r.count & md.size.as_offset_mask()), 0usize);

	let page_shift = md.size.as_shift();

	/*
	 * Safe because this crate is for use only at CPL = 0.
	 */
	let mut root = unsafe { RootPtr::current() };
	let page_count = r.count >> page_shift;
	for page_idx in (0 .. page_count) {
		let vbase =
			VAddr::from_usize(r.addr + (page_idx << page_shift));
		let pbase = if (md.op == MappingOp::Iterate) {
			md.base + (page_idx << page_shift)
		} else {
			md.base
		};

		let v4idx = pml4_index(vbase);
		let pml4 = unsafe { pte_deref_mut(&mut root) };
		let pml4e = &mut pml4[v4idx];
		if (!pml4e.is_present()) {
			let table = alloc_pagetable_page::<L3PT>()?;
			let tpage = table as u64;
			*pml4e = L4Entry::new(
				tpage.into(),
				Pml4eFlags::US | Pml4eFlags::RW | Pml4eFlags::P,
			);
		}

		let pdpt = unsafe { pte_deref_mut(pml4e) };
		let v3idx = pdpt_index(vbase);
		let pdpe = &mut pdpt[v3idx];
		if (md.size == PageSize::Size1G) {
			if (pdpe.is_present() && !pdpe.is_page()) {
				println!(
					"WARNING: replacing smaller \
					 mappings for {:x}",
					vbase
				);
				clear_mapping(pdpe, vbase);
			}

			*pdpe = L3Entry::new(
				pbase,
				PdpeFlags::from(md.flags) | PdpeFlags::PS,
			);
			invlpg(vbase.as_usize() as *const u8);
			continue;
		}
		if (!pdpe.is_present()) {
			let table = alloc_pagetable_page::<L2PT>()?;
			let tpage = table as u64;
			*pdpe = L3Entry::new(
				tpage.into(),
				PdpeFlags::RW | PdpeFlags::P,
			);
		}
		if (pdpe.is_page()) {
			return (Err(Error::LargerMappingExists));
		}

		let pd = unsafe { pte_deref_mut(pdpe) };
		let v2idx = pd_index(vbase);
		let pde = &mut pd[v2idx];
		if (md.size == PageSize::Size2M) {
			if (pde.is_present() && !pde.is_page()) {
				println!(
					"WARNING: replacing smaller \
					 mappings for {:x}",
					vbase
				);
				clear_mapping(pde, vbase);
			}

			*pde = L2Entry::new(
				pbase,
				PdeFlags::from(md.flags) | PdeFlags::PS,
			);
			invlpg(vbase.as_usize() as *const u8);
			continue;
		}
		if (!pde.is_present()) {
			let table = alloc_pagetable_page::<L1PT>()?;
			let tpage = table as u64;
			*pde = L2Entry::new(
				tpage.into(),
				PdeFlags::RW | PdeFlags::P,
			);
		}
		if (pde.is_page()) {
			return (Err(Error::LargerMappingExists));
		}

		let pt = unsafe { pte_deref_mut(pde) };
		let v1idx = pt_index(vbase);
		let pte = &mut pt[v1idx];
		assert_eq!(md.size, PageSize::Size4K);

		*pte = L1Entry::new(pbase, PteFlags::from(md.flags));
		invlpg(vbase.as_usize() as *const u8);
	}

	Ok(())
}

pub fn unmap_region(r: &NativeRegion) {
	let size = get_page_size(&r.as_range());
	let page_shift = size.as_shift();

	/*
	 * Safe because this crate is for use only at CPL = 0.
	 */
	let mut root = unsafe { RootPtr::current() };
	let page_count = r.count >> page_shift;
	for page_idx in (0 .. page_count) {
		let vbase =
			VAddr::from_usize(r.addr + (page_idx << page_shift));
		let pml4 = unsafe { pte_deref_mut(&mut root) };
		let v4idx = pml4_index(vbase);
		let pml4e = &mut pml4[v4idx];
		if (!pml4e.is_present()) {
			continue;
		}

		let pdpt = unsafe { pte_deref_mut(pml4e) };
		let v3idx = pdpt_index(vbase);
		let pdpe = &mut pdpt[v3idx];
		if (size == PageSize::Size1G) {
			clear_mapping(pdpe, vbase);
			continue;
		}

		let pd = unsafe { pte_deref_mut(pdpe) };
		let v2idx = pd_index(vbase);
		let pde = &mut pd[v2idx];
		if (size == PageSize::Size2M) {
			clear_mapping(pde, vbase);
			continue;
		}

		let pt = unsafe { pte_deref_mut(pde) };
		let v1idx = pt_index(vbase);
		let pte = &mut pt[v1idx];
		assert_eq!(size, PageSize::Size4K);
		clear_mapping(pte, vbase);
		invlpg(vbase.as_usize() as *const u8);
	}
}

pub fn is_range_canonical(range: &RangeInclusive<usize>) -> bool {
	let start = *range.start() as u64;
	let end = *range.end() as u64;

	(mem_regions::VIRT_RANGE_LOW.contains(&start) &&
		mem_regions::VIRT_RANGE_LOW.contains(&end)) ||
		(mem_regions::VIRT_RANGE_HIGH.contains(&start) &&
			mem_regions::VIRT_RANGE_HIGH.contains(&end))
}

pub fn is_range_mapped(range: &RangeInclusive<usize>) -> bool {
	if (!is_range_canonical(range)) {
		return (false);
	}

	/*
	 * Safe because CPL = 0.
	 */
	let root = unsafe { RootPtr::current() };
	assert!(is_safe(&root));

	let mut addr = *range.start();
	let end = *range.end();
	let mut len = end - addr + 1;

	loop {
		let v4idx = pml4_index(addr.into());
		/*
		 * Safe because we previously asserted that this page is
		 * within the program itself.
		 */
		let pml4 = unsafe { pte_deref(&root) };
		if (!is_safe(&pml4[v4idx]) || !pml4[v4idx].is_present()) {
			return (false);
		}

		/*
		 * Safe because we returned previously if this page is not
		 * within the program itself.
		 */
		let pdpt = unsafe { pte_deref(&pml4[v4idx]) };
		let v3idx = pdpt_index(addr.into());
		let pdpe = &pdpt[v3idx];
		if (!pdpe.is_present()) {
			return (false);
		}
		if (pdpe.is_page()) {
			let mask = L3Entry::MAP_SIZE.as_base_mask();
			let size = L3Entry::MAP_SIZE.as_size();

			len = len.saturating_sub(size - (addr & !mask));
			if (len == 0) {
				break;
			}
			addr &= mask;
			addr = addr.saturating_add(size);
			continue;
		}

		if (!is_safe(pdpe)) {
			return (false);
		}
		let pd = unsafe { pte_deref(pdpe) };
		let v2idx = pd_index(addr.into());
		let pde = &pd[v2idx];
		if (!pde.is_present()) {
			return (false);
		}
		if (pde.is_page()) {
			let mask = L2Entry::MAP_SIZE.as_base_mask();
			let size = L2Entry::MAP_SIZE.as_size();

			len = len.saturating_sub(size - (addr & !mask));
			if (len == 0) {
				break;
			}
			addr &= mask;
			addr = addr.saturating_add(size);
			continue;
		}

		if (!is_safe(pde)) {
			return (false);
		}
		let pt = unsafe { pte_deref(pde) };
		let v1idx = pt_index(addr.into());
		let pte = &pt[v1idx];
		if (!pte.is_present()) {
			return (false);
		}

		let mask = L1Entry::MAP_SIZE.as_base_mask();
		let size = L1Entry::MAP_SIZE.as_size();

		len = len.saturating_sub(size - (addr & !mask));
		if (len == 0) {
			break;
		}
		addr &= mask;
		addr = addr.saturating_add(size);
	}

	true
}

pub fn find_size_of_mapped_region(base: VAddr) -> usize {
	let mut page_size = PageSize::Size1G;
	let mut start = PageSize::Size4K.align_down_size(base.as_usize());
	let mut total = 0usize;

	loop {
		let larger = page_size.larger();
		let smaller = page_size.smaller();
		let next = page_size.align_up_size(start.saturating_add(1));
		let range = (start ..= next - 1);

		if (next == start || !is_range_mapped(&range)) {
			if let Some(nps) = smaller {
				page_size = nps;
				continue;
			}
			break;
		}

		total += next - start;
		start = page_size.align_down_size(start) + page_size.as_size();

		while let Some(nps) = larger {
			if ((start & nps.as_offset_mask()) == 0) {
				page_size = nps;
			} else {
				break;
			}
		}
	}

	total
}

pub fn require_mapped_bytes(
	addr: Option<u64>,
	count: Option<u64>,
) -> Result<(), &'static str> {
	let addr = match (addr) {
		None => return (Err("nonexistent address is unmapped")),
		Some(x) => x as usize,
	};

	let count = match (count) {
		None | Some(0) => 1,
		Some(x) => x as usize,
	};

	let end = addr
		.checked_add(count - 1)
		.ok_or("64-bit address overflow")?;

	let range = (addr ..= end);
	if (is_range_mapped(&range)) {
		Ok(())
	} else {
		Err("virtual address range is not mapped")
	}
}

pub fn dump_arena() {
	const PAGE: PageSize = PageSize::Size4K;

	if (unsafe { SPAGETABLE } == 0) {
		unsafe {
			init_pagetables();
		}
	}

	let rootphys = unsafe { RootPtr::current() }.child_paddr().unwrap().0;
	let start = unsafe { SPAGETABLE };
	let initend = unsafe { EPAGETABLE };
	let space = unsafe { PAGETABLE_SPACE };
	let freemap = unsafe { FREEMAP };

	println!(
		"Pagetable arena is at [{:x}, {:x}) [{} pages]",
		start,
		start + space,
		space >> PAGE.as_shift()
	);
	println!(
		"Bootblock pagetables at [{:x}, {:x}) [{} pages]",
		start,
		initend,
		(initend - start) >> PAGE.as_shift()
	);
	println!(
		"Reservation for loaded programs at [{:x}, {:x}) [{} pages]",
		initend,
		initend + (RESERVED_PAGETABLES << PAGE.as_shift()),
		RESERVED_PAGETABLES
	);
	println!("Root pagetable is at {:x}", rootphys);
	println!("Freemap: {:x?} === {:b}", freemap, freemap);
}

pub fn dump_mappings() {
	/*
	 * Safe because this crate is for use only at CPL = 0.
	 */
	let root = unsafe { RootPtr::current() };
	println!("{}", root);

	/*
	 * Safety: The fact that we're running implies the validity of the
	 * pagetable root address we got from %cr3.  Its validity as a virtual
	 * address is checked by pml4(), as much as possible.
	 */
	let pml4 = unsafe { pte_deref(&root) };

	for (v4idx, pml4e) in pml4.iter().enumerate() {
		if (!pml4e.is_present()) {
			continue;
		}
		let v4base = if (v4idx as u64 & (1u64 << 47) != 0) {
			v4idx << 39 | 0xFFFF_0000_0000_0000
		} else {
			v4idx << 39
		};
		println!("  {:7x}______/25 {}", v4base >> 36, pml4e);

		/*
		 * Safety is guaranteed only by pagetable construction.
		 */
		let pdpt = unsafe { pte_deref(pml4e) };
		for (v3idx, pdpe) in pdpt.iter().enumerate() {
			if (!pdpe.is_present()) {
				continue;
			}

			let v3base = v4base | v3idx << 30;
			println!("  {:9x}____/34 {}", v3base >> 28, *pdpe);
			if (pdpe.is_page()) {
				continue;
			}

			/*
			 * Safety is guaranteed only by pagetable construction.
			 */
			let pd = unsafe { pte_deref(pdpe) };
			for (v2idx, pde) in pd.iter().enumerate() {
				if (!pde.is_present()) {
					continue;
				}

				let v2base = v3base | v2idx << 21;
				println!("  {:11x}__/43 {}", v2base >> 20, pde);
				if (pde.is_page()) {
					continue;
				}

				/*
				 * Safety is guaranteed only by pagetable
				 * construction.
				 */
				let pt = unsafe { pte_deref(pde) };
				for (v1idx, pte) in pt.iter().enumerate() {
					if (!pte.is_present()) {
						continue;
					}

					let v1base = v2base | v1idx << 12;
					println!(
						"  {:13x}/52 {}",
						v1base >> 12,
						pte
					);
				}
			}
		}
	}
}
