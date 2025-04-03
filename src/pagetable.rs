/*
 * Copyright 2025 Oxide Computer Company
 */

use crate::prelude::*;
use crate::tedium64::*;
use bitflags::bitflags;
use core::fmt::{self, Display};
use core::ops::{Index, IndexMut, RangeInclusive};
use void::Void;
use x86::bits64::paging::PAddr;
use x86::controlregs;

pub trait SizeEnum: Copy + Sized {
	fn as_shift(&self) -> Shift64;

	fn as_offset_mask(&self) -> Mask64 {
		Mask64::bits_to(self.as_shift())
	}

	fn as_base_mask(&self) -> Mask64 {
		Mask64::bits_from(self.as_shift())
	}

	fn as_size(&self) -> usize {
		self.as_shift().size()
	}

	fn align_up_size(&self, v: usize) -> usize {
		v.saturating_add(self.as_size() - 1) & self.as_base_mask()
	}

	fn align_up_u64(&self, v: u64) -> u64 {
		v.saturating_add(self.as_size() as u64 - 1) &
			self.as_base_mask()
	}

	fn align_down_size(&self, v: usize) -> usize {
		v & self.as_base_mask()
	}

	fn align_down_u64(&self, v: u64) -> u64 {
		v & self.as_base_mask()
	}

	fn smaller(&self) -> Option<Self>;
	fn larger(&self) -> Option<Self>;
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum MappingSize {
	Size4K,
	Size2M,
	Size1G,
	Size512G,
	Size256T,
}

impl SizeEnum for MappingSize {
	#[inline(always)]
	fn as_shift(&self) -> Shift64 {
		match (self) {
			Self::Size4K => shift64!(12),
			Self::Size2M => shift64!(21),
			Self::Size1G => shift64!(30),
			Self::Size512G => shift64!(39),
			Self::Size256T => shift64!(48),
		}
	}

	fn smaller(&self) -> Option<Self> {
		match (self) {
			Self::Size4K => None,
			Self::Size2M => Some(Self::Size4K),
			Self::Size1G => Some(Self::Size2M),
			Self::Size512G => Some(Self::Size1G),
			Self::Size256T => Some(Self::Size512G),
		}
	}

	fn larger(&self) -> Option<Self> {
		match (self) {
			Self::Size4K => Some(Self::Size2M),
			Self::Size2M => Some(Self::Size1G),
			Self::Size1G => Some(Self::Size512G),
			Self::Size512G => Some(Self::Size256T),
			Self::Size256T => None,
		}
	}
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum PageSize {
	Size4K,
	Size2M,
	Size1G,
}

impl SizeEnum for PageSize {
	#[inline(always)]
	fn as_shift(&self) -> Shift64 {
		match (self) {
			Self::Size4K => shift64!(12),
			Self::Size2M => shift64!(21),
			Self::Size1G => shift64!(30),
		}
	}

	fn smaller(&self) -> Option<Self> {
		match (self) {
			Self::Size4K => None,
			Self::Size2M => Some(Self::Size4K),
			Self::Size1G => Some(Self::Size2M),
		}
	}

	fn larger(&self) -> Option<Self> {
		match (self) {
			Self::Size4K => Some(Self::Size2M),
			Self::Size2M => Some(Self::Size1G),
			Self::Size1G => None,
		}
	}
}

pub fn get_page_size(r: &RangeInclusive<usize>) -> PageSize {
	let size = PageSize::Size4K;

	if (r.is_empty()) {
		return (size);
	}

	let base = *r.start() & size.as_base_mask();
	let len =
		((*r.end() - base) - 1 + size.as_size()) & size.as_base_mask();

	let size = PageSize::Size1G;
	if ((base & size.as_offset_mask()) == 0 &&
		(len & size.as_offset_mask() == 0))
	{
		return (size);
	}

	let size = PageSize::Size2M;
	if ((base & size.as_offset_mask()) == 0 &&
		(len & size.as_offset_mask() == 0))
	{
		return (size);
	}

	PageSize::Size4K
}

pub trait PageTable: Index<usize> + IndexMut<usize> + Sized {
	type Entry: PageTableEntry + Sized;

	fn pidx(&self, index: usize) -> &Self::Entry;
	fn pidx_mut(&mut self, index: usize) -> &mut Self::Entry;

	#[allow(clippy::needless_lifetimes)]
	fn iter<'a>(&'a self) -> core::slice::Iter<'a, Self::Entry>;
	#[allow(clippy::needless_lifetimes)]
	fn iter_mut<'a>(&'a mut self) -> core::slice::IterMut<'a, Self::Entry>;
}

pub trait PageTableEntryFlags: Copy {
	fn zero() -> Self;
}

pub trait PageTableEntry: Sized {
	type Flags: PageTableEntryFlags;
	type Target: PageTable + Sized;

	const MAP_SIZE: MappingSize;

	fn new(addr: PAddr, flags: Self::Flags) -> Self;
	fn page_paddr(&self) -> Option<PAddr>;
	fn child_paddr(&self) -> Option<PAddr>;
	fn flags(&self) -> Self::Flags;
	fn is_present(&self) -> bool;
	fn is_page_flags(f: Self::Flags) -> bool;

	fn is_page(&self) -> bool {
		Self::is_page_flags(self.flags())
	}
}

macro_rules! impl_pagetable {
	($pt:ident, $et:ty) => {
		#[repr(transparent)]
		pub struct $pt([$et; 512]);

		impl PageTable for $pt {
			type Entry = $et;

			fn pidx(&self, index: usize) -> &Self::Entry {
				&self.0[index]
			}

			fn pidx_mut(
				&mut self,
				index: usize,
			) -> &mut Self::Entry {
				&mut self.0[index]
			}

			fn iter<'a>(
				&'a self,
			) -> core::slice::Iter<'a, Self::Entry> {
				self.0.iter()
			}

			fn iter_mut<'a>(
				&'a mut self,
			) -> core::slice::IterMut<'a, Self::Entry> {
				self.0.iter_mut()
			}
		}

		impl Index<usize> for $pt {
			type Output = $et;

			fn index(&self, index: usize) -> &Self::Output {
				self.pidx(index)
			}
		}

		impl IndexMut<usize> for $pt {
			fn index_mut(
				&mut self,
				index: usize,
			) -> &mut Self::Output {
				self.pidx_mut(index)
			}
		}
	};
}

impl_pagetable!(L4PT, L4Entry);
impl_pagetable!(L3PT, L3Entry);
impl_pagetable!(L2PT, L2Entry);
impl_pagetable!(L1PT, L1Entry);

#[allow(dead_code)]
pub struct Terminator(u8);

impl Index<usize> for Terminator {
	type Output = Void;

	fn index(&self, _: usize) -> &Self::Output {
		unreachable!()
	}
}

impl IndexMut<usize> for Terminator {
	fn index_mut(&mut self, _: usize) -> &mut Self::Output {
		unreachable!()
	}
}

impl PageTable for Terminator {
	type Entry = Void;

	fn pidx(&self, _: usize) -> &Self::Entry {
		unreachable!()
	}

	fn pidx_mut(&mut self, _: usize) -> &mut Self::Entry {
		unreachable!()
	}

	#[allow(clippy::needless_lifetimes)]
	fn iter<'a>(
		&'a self,
	) -> core::slice::Iter<'a, <Self as Index<usize>>::Output> {
		unreachable!()
	}

	#[allow(clippy::needless_lifetimes)]
	fn iter_mut<'a>(
		&'a mut self,
	) -> core::slice::IterMut<'a, <Self as Index<usize>>::Output> {
		unreachable!()
	}
}

macro_rules! impl_page_table_entry_flags {
	($t:ident) => {
		impl PageTableEntryFlags for $t {
			fn zero() -> Self {
				Self::empty()
			}
		}
	};
}

/*
 * x86 has no implementation for %cr3 at all.
 */
bitflags! {
	pub struct Cr3Flags: u64 {
		const PWT = mask64_bits!(3).as_u64();
		const PCD = mask64_bits!(4).as_u64();
		const PCID = mask64_bits!(0 ..= 11).as_u64();
	}
}

impl Cr3Flags {
	pub fn pcid_bits(self) -> u64 {
		(self & Self::PCID).bits()
	}
}

impl_page_table_entry_flags!(Cr3Flags);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct RootPtr(u64);

impl RootPtr {
	///
	/// # Safety
	///
	/// This method may be called only at CPL = 0.
	///
	pub unsafe fn current() -> Self {
		Self(controlregs::cr3())
	}
}

impl PageTableEntry for RootPtr {
	type Flags = Cr3Flags;
	type Target = L4PT;

	const MAP_SIZE: MappingSize = MappingSize::Size256T;

	/*
	 * XXX We don't have any obvious way to tell whether the caller wants
	 * to build an entry with a PCID or with flags.  These probably need
	 * different types instead; with our current flags definition it is
	 * possible to construct invalid values that will induce an exception
	 * if stuffed into %cr3.
	 */
	fn new(childaddr: PAddr, flags: Self::Flags) -> Self {
		assert_eq!(
			childaddr & PageSize::Size4K.as_offset_mask(),
			0.into()
		);
		assert_eq!(flags & Cr3Flags::all(), flags);

		Self(childaddr | flags.bits())
	}

	fn page_paddr(&self) -> Option<PAddr> {
		None
	}

	fn child_paddr(&self) -> Option<PAddr> {
		Some((self.0 & mask64_bits!(12 ..= 51)).into())
	}

	fn flags(&self) -> Self::Flags {
		Cr3Flags::from_bits_truncate(self.0)
	}

	fn is_present(&self) -> bool {
		true
	}

	fn is_page_flags(_f: Self::Flags) -> bool {
		false
	}
}

impl Display for RootPtr {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let root_pwt = self.flags().contains(Cr3Flags::PWT);
		let root_pcd = self.flags().contains(Cr3Flags::PCD);

		let root_flags = match (root_pcd, root_pwt) {
			(false, false) => "--",
			(false, true) => "-*",
			(true, false) => "*-",
			(true, true) => "**",
		};

		/*
		 * XXX This probably belongs in fmt() of a pagetable type
		 * that doesn't exist.
		 */
		writeln!(f, "  {:>13}    NSSSS A  CW", "")?;
		writeln!(
			f,
			"  {:>13}/SZ XHHHLGTDADTUWP | {:>13}",
			"VIRTUAL", "PFN"
		)?;

		write!(
			f,
			"  XXXX_________/16 _________{}___ v {:13x}",
			root_flags,
			self.child_paddr().unwrap().0 >> shift64!(12)
		)?;

		Ok(())
	}
}

/*
 * x86 Pml4eFlags has the wrong name for NX and lacks definitions for all the
 * software bits.
 */
bitflags! {
	pub struct Pml4eFlags: u64 {
		const P = mask64_bits!(0).as_u64();
		const RW = mask64_bits!(1).as_u64();
		const US = mask64_bits!(2).as_u64();
		const PWT = mask64_bits!(3).as_u64();
		const PCD = mask64_bits!(4).as_u64();
		const A = mask64_bits!(5).as_u64();
		const AVL9 = mask64_bits!(9).as_u64();
		const AVL10 = mask64_bits!(10).as_u64();
		const AVL11 = mask64_bits!(11).as_u64();
		const AVL52 = mask64_bits!(52).as_u64();
		const AVL53 = mask64_bits!(53).as_u64();
		const AVL54 = mask64_bits!(54).as_u64();
		const AVL55 = mask64_bits!(55).as_u64();
		const AVL56 = mask64_bits!(56).as_u64();
		const AVL57 = mask64_bits!(57).as_u64();
		const AVL58 = mask64_bits!(58).as_u64();
		const AVL59 = mask64_bits!(59).as_u64();
		const AVL60 = mask64_bits!(60).as_u64();
		const AVL61 = mask64_bits!(61).as_u64();
		const AVL62 = mask64_bits!(62).as_u64();
		const NX = mask64_bits!(63).as_u64();

		const SW_LOW = mask64_bits!(9 ..= 11).as_u64();
		const SW_HIGH = mask64_bits!(52 ..= 62).as_u64();
	}
}

impl Pml4eFlags {
	pub fn sw_bits_low(self) -> u64 {
		(self & Self::SW_LOW).bits() >> 9
	}

	pub fn sw_bits_high(self) -> u64 {
		(self & Self::SW_HIGH).bits() >> 52
	}
}

impl_page_table_entry_flags!(Pml4eFlags);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct L4Entry(u64);

impl PageTableEntry for L4Entry {
	type Flags = Pml4eFlags;
	type Target = L3PT;

	const MAP_SIZE: MappingSize = MappingSize::Size512G;

	fn new(childaddr: PAddr, flags: Self::Flags) -> Self {
		assert_eq!(
			childaddr & PageSize::Size4K.as_offset_mask(),
			0.into()
		);
		assert_eq!(flags & Pml4eFlags::all(), flags);

		Self(childaddr | flags.bits())
	}
	fn page_paddr(&self) -> Option<PAddr> {
		None
	}

	fn child_paddr(&self) -> Option<PAddr> {
		Some((self.0 & mask64_bits!(12 ..= 51)).into())
	}

	fn flags(&self) -> Self::Flags {
		Pml4eFlags::from_bits_truncate(self.0)
	}

	fn is_present(&self) -> bool {
		self.flags().contains(Pml4eFlags::P)
	}

	fn is_page_flags(_f: Self::Flags) -> bool {
		false
	}
}

macro_rules! write_bits {
	($f:expr, $b:expr, $($flag:expr),*) => {
		$(write!($f, "{}",
			 if ($b.contains($flag)) { "*" } else { "-" })?;)*
	};
}

impl Display for L4Entry {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let flags = self.flags();

		write_bits!(f, flags, Pml4eFlags::NX);
		write!(f, "{:03x}", flags.sw_bits_high())?;
		write!(f, "{:01x}___", flags.sw_bits_low())?;
		write_bits!(
			f,
			flags,
			Pml4eFlags::A,
			Pml4eFlags::PCD,
			Pml4eFlags::PWT,
			Pml4eFlags::US,
			Pml4eFlags::RW,
			Pml4eFlags::P
		);
		write!(
			f,
			" v {:13x}",
			self.child_paddr().unwrap().as_usize() >>
				PageSize::Size4K.as_shift()
		)?;

		Ok(())
	}
}

/*
 * x86 PDPEFlags has the wrong name for NX, lacks definitions of all the
 * software bits, and the associated entry types do not handle the effect of
 * PS's value on the interpretation of PAT.
 */
bitflags! {
	pub struct PdpeFlags: u64 {
		const P = mask64_bits!(0).as_u64();
		const RW = mask64_bits!(1).as_u64();
		const US = mask64_bits!(2).as_u64();
		const PWT = mask64_bits!(3).as_u64();
		const PCD = mask64_bits!(4).as_u64();
		const A = mask64_bits!(5).as_u64();
		const D = mask64_bits!(6).as_u64();
		const PS = mask64_bits!(7).as_u64();
		const G = mask64_bits!(8).as_u64();
		const AVL9 = mask64_bits!(9).as_u64();
		const AVL10 = mask64_bits!(10).as_u64();
		const AVL11 = mask64_bits!(11).as_u64();
		const PAT = mask64_bits!(12).as_u64();
		const AVL52 = mask64_bits!(52).as_u64();
		const AVL53 = mask64_bits!(53).as_u64();
		const AVL54 = mask64_bits!(54).as_u64();
		const AVL55 = mask64_bits!(55).as_u64();
		const AVL56 = mask64_bits!(56).as_u64();
		const AVL57 = mask64_bits!(57).as_u64();
		const AVL58 = mask64_bits!(58).as_u64();
		const AVL59 = mask64_bits!(59).as_u64();
		const AVL60 = mask64_bits!(60).as_u64();
		const AVL61 = mask64_bits!(61).as_u64();
		const AVL62 = mask64_bits!(62).as_u64();
		const NX = mask64_bits!(63).as_u64();

		const SW_LOW = mask64_bits!(9 ..= 11).as_u64();
		const SW_HIGH = mask64_bits!(52 ..= 62).as_u64();
	}
}

impl PdpeFlags {
	pub fn sw_bits_low(self) -> u64 {
		(self & Self::SW_LOW).bits() >> 9
	}

	pub fn sw_bits_high(self) -> u64 {
		(self & Self::SW_HIGH).bits() >> 52
	}
}

impl_page_table_entry_flags!(PdpeFlags);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct L3Entry(u64);

impl PageTableEntry for L3Entry {
	type Flags = PdpeFlags;
	type Target = L2PT;

	const MAP_SIZE: MappingSize = MappingSize::Size1G;

	fn new(addr: PAddr, flags: Self::Flags) -> Self {
		let allowed_flags;
		let phys_mask;

		if (Self::is_page_flags(flags)) {
			allowed_flags = PdpeFlags::all();
			phys_mask = Self::MAP_SIZE.as_offset_mask();
		} else {
			allowed_flags = PdpeFlags::all() -
				(PdpeFlags::PS | PdpeFlags::PAT);
			phys_mask = PageSize::Size4K.as_offset_mask();
		}

		assert_eq!(addr & phys_mask, 0.into());
		assert_eq!(flags & allowed_flags, flags);

		Self(addr | flags.bits())
	}

	fn page_paddr(&self) -> Option<PAddr> {
		if (self.flags().contains(PdpeFlags::PS)) {
			/* XXX MAP_SIZE */
			Some((self.0 & mask64_bits!(30 ..= 51)).into())
		} else {
			None
		}
	}

	fn child_paddr(&self) -> Option<PAddr> {
		if (self.flags().contains(PdpeFlags::PS)) {
			None
		} else {
			Some((self.0 & mask64_bits!(12 ..= 51)).into())
		}
	}

	fn flags(&self) -> Self::Flags {
		PdpeFlags::from_bits_truncate(self.0)
	}

	fn is_present(&self) -> bool {
		self.flags().contains(PdpeFlags::P)
	}

	fn is_page_flags(f: Self::Flags) -> bool {
		f.contains(PdpeFlags::PS)
	}
}

impl Display for L3Entry {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let flags = self.flags();
		let is_page = self.is_page();

		write_bits!(f, flags, PdpeFlags::NX);
		write!(f, "{:03x}", flags.sw_bits_high())?;
		write!(f, "{:01x}", flags.sw_bits_low())?;

		if (is_page) {
			write_bits!(
				f,
				flags,
				PdpeFlags::G,
				PdpeFlags::PAT,
				PdpeFlags::D,
				PdpeFlags::A,
				PdpeFlags::PCD,
				PdpeFlags::PWT,
				PdpeFlags::US,
				PdpeFlags::RW,
				PdpeFlags::P
			);
			write!(
				f,
				" > {:9x}____",
				self.page_paddr().unwrap().as_usize() >> 28
			)?;
		} else {
			write!(f, "___")?;
			write_bits!(
				f,
				flags,
				PdpeFlags::A,
				PdpeFlags::PCD,
				PdpeFlags::PWT,
				PdpeFlags::US,
				PdpeFlags::RW,
				PdpeFlags::P
			);
			write!(
				f,
				" v {:13x}",
				self.child_paddr().unwrap().as_usize() >>
					PageSize::Size4K.as_shift()
			)?;
		}

		Ok(())
	}
}

/*
 * x86 PDEFlags has the wrong name for NX, lacks definitions of all the
 * software bits, and the associated entry types do not handle the effect of
 * PS's value on the interpretation of PAT.
 */
bitflags! {
	pub struct PdeFlags: u64 {
		const P = mask64_bits!(0).as_u64();
		const RW = mask64_bits!(1).as_u64();
		const US = mask64_bits!(2).as_u64();
		const PWT = mask64_bits!(3).as_u64();
		const PCD = mask64_bits!(4).as_u64();
		const A = mask64_bits!(5).as_u64();
		const D = mask64_bits!(6).as_u64();
		const PS = mask64_bits!(7).as_u64();
		const G = mask64_bits!(8).as_u64();
		const AVL9 = mask64_bits!(9).as_u64();
		const AVL10 = mask64_bits!(10).as_u64();
		const AVL11 = mask64_bits!(11).as_u64();
		const PAT = mask64_bits!(12).as_u64();
		const AVL52 = mask64_bits!(52).as_u64();
		const AVL53 = mask64_bits!(53).as_u64();
		const AVL54 = mask64_bits!(54).as_u64();
		const AVL55 = mask64_bits!(55).as_u64();
		const AVL56 = mask64_bits!(56).as_u64();
		const AVL57 = mask64_bits!(57).as_u64();
		const AVL58 = mask64_bits!(58).as_u64();
		const AVL59 = mask64_bits!(59).as_u64();
		const AVL60 = mask64_bits!(60).as_u64();
		const AVL61 = mask64_bits!(61).as_u64();
		const AVL62 = mask64_bits!(62).as_u64();
		const NX = mask64_bits!(63).as_u64();

		const SW_LOW = mask64_bits!(9 ..= 11).as_u64();
		const SW_HIGH = mask64_bits!(52 ..= 62).as_u64();
	}
}

impl PdeFlags {
	pub fn sw_bits_low(self) -> u64 {
		(self & Self::SW_LOW).bits() >> 9
	}

	pub fn sw_bits_high(self) -> u64 {
		(self & Self::SW_HIGH).bits() >> 52
	}
}

impl_page_table_entry_flags!(PdeFlags);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct L2Entry(u64);

impl PageTableEntry for L2Entry {
	type Flags = PdeFlags;
	type Target = L1PT;

	const MAP_SIZE: MappingSize = MappingSize::Size2M;

	fn new(addr: PAddr, flags: Self::Flags) -> Self {
		let allowed_flags;
		let phys_mask;

		if (Self::is_page_flags(flags)) {
			allowed_flags = PdeFlags::all();
			phys_mask = Self::MAP_SIZE.as_offset_mask();
		} else {
			allowed_flags = PdeFlags::all() -
				(PdeFlags::PS | PdeFlags::PAT);
			phys_mask = PageSize::Size4K.as_offset_mask();
		}

		assert_eq!(addr & phys_mask, 0.into());
		assert_eq!(flags & allowed_flags, flags);

		Self(addr | flags.bits())
	}

	fn page_paddr(&self) -> Option<PAddr> {
		if (self.flags().contains(PdeFlags::PS)) {
			Some((self.0 & mask64_bits!(21 ..= 51)).into())
		} else {
			None
		}
	}

	fn child_paddr(&self) -> Option<PAddr> {
		if (self.flags().contains(PdeFlags::PS)) {
			None
		} else {
			Some((self.0 & mask64_bits!(12 ..= 51)).into())
		}
	}

	fn flags(&self) -> Self::Flags {
		PdeFlags::from_bits_truncate(self.0)
	}

	fn is_present(&self) -> bool {
		self.flags().contains(PdeFlags::P)
	}

	fn is_page_flags(f: Self::Flags) -> bool {
		f.contains(PdeFlags::PS)
	}
}

impl Display for L2Entry {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let flags = self.flags();
		let is_page = self.is_page();

		write_bits!(f, flags, PdeFlags::NX);
		write!(f, "{:03x}", flags.sw_bits_high())?;
		write!(f, "{:01x}", flags.sw_bits_low())?;

		if (is_page) {
			write_bits!(
				f,
				flags,
				PdeFlags::G,
				PdeFlags::PAT,
				PdeFlags::D,
				PdeFlags::A,
				PdeFlags::PCD,
				PdeFlags::PWT,
				PdeFlags::US,
				PdeFlags::RW,
				PdeFlags::P
			);
			write!(
				f,
				" > {:11x}__",
				self.page_paddr().unwrap().as_usize() >> 20
			)?;
		} else {
			write!(f, "___")?;
			write_bits!(
				f,
				flags,
				PdeFlags::A,
				PdeFlags::PCD,
				PdeFlags::PWT,
				PdeFlags::US,
				PdeFlags::RW,
				PdeFlags::P
			);
			write!(
				f,
				" v {:13x}",
				self.child_paddr().unwrap().as_usize() >>
					PageSize::Size4K.as_shift()
			)?;
		}

		Ok(())
	}
}

/*
 * x86 PTFlags has the wrong name for NX, lacks definitions of all the
 * software bits, and is missing PAT entirely.
 */
bitflags! {
	pub struct PteFlags: u64 {
		const P = mask64_bits!(0).as_u64();
		const RW = mask64_bits!(1).as_u64();
		const US = mask64_bits!(2).as_u64();
		const PWT = mask64_bits!(3).as_u64();
		const PCD = mask64_bits!(4).as_u64();
		const A = mask64_bits!(5).as_u64();
		const D = mask64_bits!(6).as_u64();
		const PAT = mask64_bits!(7).as_u64();
		const G = mask64_bits!(8).as_u64();
		const AVL9 = mask64_bits!(9).as_u64();
		const AVL10 = mask64_bits!(10).as_u64();
		const AVL11 = mask64_bits!(11).as_u64();
		const AVL52 = mask64_bits!(52).as_u64();
		const AVL53 = mask64_bits!(53).as_u64();
		const AVL54 = mask64_bits!(54).as_u64();
		const AVL55 = mask64_bits!(55).as_u64();
		const AVL56 = mask64_bits!(56).as_u64();
		const AVL57 = mask64_bits!(57).as_u64();
		const AVL58 = mask64_bits!(58).as_u64();
		const AVL59 = mask64_bits!(59).as_u64();
		const AVL60 = mask64_bits!(60).as_u64();
		const AVL61 = mask64_bits!(61).as_u64();
		const AVL62 = mask64_bits!(62).as_u64();
		const NX = mask64_bits!(63).as_u64();

		const SW_LOW = mask64_bits!(9 ..= 11).as_u64();
		const SW_HIGH = mask64_bits!(52 ..= 62).as_u64();
	}
}

impl PteFlags {
	pub fn sw_bits_low(self) -> u64 {
		(self & Self::SW_LOW).bits() >> 9
	}

	pub fn sw_bits_high(self) -> u64 {
		(self & Self::SW_HIGH).bits() >> 52
	}
}

impl_page_table_entry_flags!(PteFlags);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
#[repr(transparent)]
pub struct L1Entry(u64);

impl PageTableEntry for L1Entry {
	type Flags = PteFlags;
	type Target = Terminator;

	const MAP_SIZE: MappingSize = MappingSize::Size4K;

	fn new(pageaddr: PAddr, flags: Self::Flags) -> Self {
		assert_eq!(
			pageaddr & Self::MAP_SIZE.as_offset_mask(),
			0.into()
		);
		assert_eq!(flags & PteFlags::all(), flags);

		Self(pageaddr | flags.bits())
	}

	fn page_paddr(&self) -> Option<PAddr> {
		Some((self.0 & mask64_bits!(12 ..= 51)).into())
	}

	fn child_paddr(&self) -> Option<PAddr> {
		None
	}

	fn flags(&self) -> Self::Flags {
		PteFlags::from_bits_truncate(self.0)
	}

	fn is_present(&self) -> bool {
		self.flags().contains(PteFlags::P)
	}

	fn is_page_flags(_f: Self::Flags) -> bool {
		true
	}
}

impl Display for L1Entry {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		let flags = self.flags();

		write_bits!(f, flags, PteFlags::NX);
		write!(f, "{:03x}", flags.sw_bits_high())?;
		write!(f, "{:01x}", flags.sw_bits_low())?;
		write_bits!(
			f,
			flags,
			PteFlags::G,
			PteFlags::PAT,
			PteFlags::D,
			PteFlags::A,
			PteFlags::PCD,
			PteFlags::PWT,
			PteFlags::US,
			PteFlags::RW,
			PteFlags::P
		);
		write!(
			f,
			" > {:13x}",
			self.page_paddr().unwrap().as_usize() >>
				Self::MAP_SIZE.as_shift()
		)?;

		Ok(())
	}
}

impl PageTableEntry for Void {
	type Flags = Void;
	type Target = Terminator;

	const MAP_SIZE: MappingSize = MappingSize::Size4K;

	fn new(_: PAddr, _: Self::Flags) -> Self {
		unreachable!()
	}
	fn page_paddr(&self) -> Option<PAddr> {
		unreachable!()
	}
	fn child_paddr(&self) -> Option<PAddr> {
		unreachable!()
	}
	fn flags(&self) -> Self::Flags {
		unreachable!()
	}
	fn is_present(&self) -> bool {
		unreachable!()
	}
	fn is_page_flags(_: Self::Flags) -> bool {
		unreachable!()
	}
	fn is_page(&self) -> bool {
		unreachable!()
	}
}

impl PageTableEntryFlags for Void {
	fn zero() -> Self {
		unreachable!()
	}
}
