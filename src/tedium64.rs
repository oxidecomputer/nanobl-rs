/*
 * Copyright 2024 Oxide Computer Company
 */

use crate::to_inclusive::{DiscreteFinite, ToInclusive};
use core::convert::TryFrom;
use core::fmt::{self, Binary, Formatter, LowerHex, Octal, UpperHex};
use core::ops::{Add, AddAssign, Sub, SubAssign};
use core::ops::{BitAnd, BitOr, BitXor, Not};
use core::ops::{BitAndAssign, BitOrAssign, BitXorAssign};
use core::ops::{Shl, ShlAssign, Shr, ShrAssign};

/*
 * XXX Write a proc macro instead.  We want something that will accept only
 * 0 ..= 63 and fail at compile time otherwise, and make bare literals into
 * u64s because otherwise the default type is signed and signed integers are
 * worse than useless for everything we want to do.
 */
#[macro_export]
macro_rules! u64_literal_helper {
	(0) => {
		(0u64)
	};
	(1) => {
		(1u64)
	};
	(2) => {
		(2u64)
	};
	(3) => {
		(3u64)
	};
	(4) => {
		(4u64)
	};
	(5) => {
		(5u64)
	};
	(6) => {
		(6u64)
	};
	(7) => {
		(7u64)
	};
	(8) => {
		(8u64)
	};
	(9) => {
		(9u64)
	};
	(10) => {
		(10u64)
	};
	(11) => {
		(11u64)
	};
	(12) => {
		(12u64)
	};
	(13) => {
		(13u64)
	};
	(14) => {
		(14u64)
	};
	(15) => {
		(15u64)
	};
	(16) => {
		(16u64)
	};
	(17) => {
		(17u64)
	};
	(18) => {
		(18u64)
	};
	(19) => {
		(19u64)
	};
	(20) => {
		(20u64)
	};
	(21) => {
		(21u64)
	};
	(22) => {
		(22u64)
	};
	(23) => {
		(23u64)
	};
	(24) => {
		(24u64)
	};
	(25) => {
		(25u64)
	};
	(26) => {
		(26u64)
	};
	(27) => {
		(27u64)
	};
	(28) => {
		(28u64)
	};
	(29) => {
		(29u64)
	};
	(30) => {
		(30u64)
	};
	(31) => {
		(31u64)
	};
	(32) => {
		(32u64)
	};
	(33) => {
		(33u64)
	};
	(34) => {
		(34u64)
	};
	(35) => {
		(35u64)
	};
	(36) => {
		(36u64)
	};
	(37) => {
		(37u64)
	};
	(38) => {
		(38u64)
	};
	(39) => {
		(39u64)
	};
	(40) => {
		(40u64)
	};
	(41) => {
		(41u64)
	};
	(42) => {
		(42u64)
	};
	(43) => {
		(43u64)
	};
	(44) => {
		(44u64)
	};
	(45) => {
		(45u64)
	};
	(46) => {
		(46u64)
	};
	(47) => {
		(47u64)
	};
	(48) => {
		(48u64)
	};
	(49) => {
		(49u64)
	};
	(50) => {
		(50u64)
	};
	(51) => {
		(51u64)
	};
	(52) => {
		(52u64)
	};
	(53) => {
		(53u64)
	};
	(54) => {
		(54u64)
	};
	(55) => {
		(55u64)
	};
	(56) => {
		(56u64)
	};
	(57) => {
		(57u64)
	};
	(58) => {
		(58u64)
	};
	(59) => {
		(59u64)
	};
	(60) => {
		(60u64)
	};
	(61) => {
		(61u64)
	};
	(62) => {
		(62u64)
	};
	(63) => {
		(63u64)
	};
}

macro_rules! decl_trns_primitive64 {
	($t:ident) => {
#[rustfmt::skip]
		#[derive(Debug, Copy, Clone, PartialOrd, Ord)]
		#[derive(PartialEq, Eq, Hash)]
		#[repr(transparent)]
		pub struct $t(u64);

		impl LowerHex for $t {
			fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
				fmt::LowerHex::fmt(&self.0, f)
			}
		}

		impl UpperHex for $t {
			fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
				fmt::UpperHex::fmt(&self.0, f)
			}
		}

		impl Binary for $t {
			fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
				fmt::Binary::fmt(&self.0, f)
			}
		}

		impl Octal for $t {
			fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
				fmt::Octal::fmt(&self.0, f)
			}
		}
	};
}

macro_rules! impl_commutative_from_prim {
	($t:ty, $($p:ty),+) => {
		$(impl From<$p> for $t {
			#[inline(always)]
			fn from(v: $p) -> Self {
				debug_assert!(v as u64 <= Self::MAX.0);
				Self(v as u64)
			}
		}
		impl From<$t> for $p {
			#[inline(always)]
			fn from(v: $t) -> Self {
				v.0 as Self
			}
		})+
	};
}

macro_rules! impl_from_prim {
	($t:ty, $($p:ty),+) => {
		$(impl From<$p> for $t {
			#[inline(always)]
			fn from(v: $p) -> Self {
				debug_assert!(v as u64 <= Self::MAX.0);
				Self(v as u64)
			}
		})+
	};
}

macro_rules! impl_to_prim {
	($t:ty, $($p:ty),+) => {
		$(impl From<$t> for $p {
			#[inline(always)]
			fn from(v: $t) -> Self {
				v.0 as $p
			}
		})+
	};
}

macro_rules! impl_binop_lhsprim {
	($ty:ty, $tr:ident, $f:ident, $op:tt, $($primitive:ty),+) => {
		$(impl $tr<$ty> for $primitive {
			type Output = Self;
			#[inline(always)]
			fn $f(self, rhs: $ty) -> Self {
				self $op rhs.0 as Self
			}
		})+
	};
}

macro_rules! impl_binop_rhsprim {
	($ty:ty, $tr:ident, $f:ident, $op:tt, $($primitive:ty),+) => {
		$(impl $tr<$primitive> for $ty {
			type Output = Self;
			#[inline(always)]
			fn $f(self, rhs: $primitive) -> Self {
				Self(self.0 $op rhs as u64)
			}
		})+
	};
}

macro_rules! impl_binop {
	($ty:ty, $tr:ident, $f:ident, $op:tt, $($other:ty),+) => {
		$(impl $tr<$other> for $ty {
			type Output = Self;
			#[inline(always)]
			fn $f(self, rhs: $other) -> Self {
				Self(self.0 $op rhs.0)
			}
		})+
	};
}

macro_rules! impl_unop {
	($tr:ident, $f:ident, $op:tt, $($ty:ty),+) => {
		$(impl $tr for $ty {
			type Output = Self;
			#[inline(always)]
			fn $f(self) -> Self::Output {
				Self($op self.0)
			}
		})+
	};
}

macro_rules! impl_binopassign_lhsprim {
	($ty:ty, $tr:ident, $f:ident, $opa:tt, $($primitive:ty),+) => {
		$(impl $tr<$ty> for $primitive {
			#[inline(always)]
			fn $f(&mut self, rhs: $ty) {
				*self $opa rhs.0 as Self;
			}
		})+
	}
}

macro_rules! impl_binopassign_rhsprim {
	($ty:ty, $tr:ident, $f:ident, $opa:tt, $($primitive:ty),+) => {
		$(impl $tr<$primitive> for $ty {
			#[inline(always)]
			fn $f(&mut self, rhs: $primitive) {
				self.0 $opa rhs as u64;
			}
		})+
	}
}

macro_rules! impl_binopassign {
	($ty:ty, $tr:ident, $f:ident, $op:tt, $($other:ty),+) => {
		$(impl $tr<$other> for $ty {
			#[inline(always)]
			fn $f(&mut self, rhs: $other) {
				self.0 $op rhs.0;
			}
		})+
	}
}

macro_rules! impl_commutative_prim {
	($trns:ty, $tr:ident, $f:ident, $op:tt, $($primitive:ty),+) => {
		impl_binop_lhsprim!($trns, $tr, $f, $op, $($primitive),+);
		impl_binop_rhsprim!($trns, $tr, $f, $op, $($primitive),+);
	};
}

macro_rules! impl_commutative_assign_prim {
	($trns:ty, $tr:ident, $f:ident, $op:tt, $($primitive:ty),+) => {
		impl_binopassign_lhsprim!($trns, $tr, $f, $op, $($primitive),+);
		impl_binopassign_rhsprim!($trns, $tr, $f, $op, $($primitive),+);
	};
}

decl_trns_primitive64!(Shift64);

impl Shift64 {
	///
	/// # Safety
	///
	/// `s` must be in the range (0 .. 64).
	///
	pub const unsafe fn new_unchecked_u64(s: u64) -> Self {
		Self(s)
	}

	pub fn new<T: Into<u64>>(s: T) -> Self {
		let s = Self(s.into());
		assert!(s <= Self::MAX);
		s
	}

	pub const fn count(self) -> u64 {
		self.0
	}

	pub const fn size(self) -> usize {
		1usize << self.0
	}
}

#[macro_export]
macro_rules! shift64 {
	($tt:tt) => {
		unsafe {
			Shift64::new_unchecked_u64($crate::u64_literal_helper!(
				$tt
			))
		}
	};
}

impl DiscreteFinite for Shift64 {
	const MIN: Self = Self(0);
	const MAX: Self = Self(63);

	#[inline(always)]
	fn next(&self) -> Self {
		Self(self.0.saturating_add(1).clamp(Self::MIN.0, Self::MAX.0))
	}

	#[inline(always)]
	fn prev(&self) -> Self {
		Self(self.0.saturating_sub(1))
	}
}

impl_commutative_from_prim!(Shift64, u8, u16, u32, u64, usize);
impl_to_prim!(Shift64, i8, i16, i32, i64, isize);

impl_commutative_prim!(Shift64, Add, add, +, u64, i64);
impl_commutative_prim!(Shift64, Sub, sub, -, u64, i64);
impl_binop_rhsprim!(Shift64, Add, add, +, u8, u16, u32, i8, i16, i32);
impl_binop_rhsprim!(Shift64, Sub, sub, -, u8, u16, u32, i8, i16, i32);
#[cfg(target_pointer_width = "64")]
impl_commutative_prim!(Shift64, Add, add, +, usize, isize);
#[cfg(target_pointer_width = "64")]
impl_commutative_prim!(Shift64, Sub, sub, -, usize, isize);
impl_binop!(Shift64, Add, add, +, Shift64);
impl_binop!(Shift64, Sub, sub, -, Shift64);

impl_commutative_assign_prim!(Shift64, AddAssign, add_assign, +=, u64, i64);
impl_commutative_assign_prim!(Shift64, SubAssign, sub_assign, -=, u64, i64);
impl_binopassign_rhsprim!(Shift64, AddAssign, add_assign, +=,
			  u8, u16, u32, i8, i16, i32);
impl_binopassign_rhsprim!(Shift64, SubAssign, sub_assign, -=,
			  u8, u16, u32, i8, i16, i32);
#[cfg(target_pointer_width = "64")]
impl_commutative_assign_prim!(Shift64, AddAssign, add_assign, +=, usize, isize);
#[cfg(target_pointer_width = "64")]
impl_commutative_assign_prim!(Shift64, SubAssign, sub_assign, -=, usize, isize);
impl_binopassign!(Shift64, AddAssign, add_assign, +=, Shift64);
impl_binopassign!(Shift64, SubAssign, sub_assign, -=, Shift64);

impl_binop_lhsprim!(Shift64, Shl, shl, <<, u64, i64);
impl_binop_lhsprim!(Shift64, Shr, shr, >>, u64, i64);
#[cfg(target_pointer_width = "64")]
impl_binop_lhsprim!(Shift64, Shl, shl, <<, usize, isize);
#[cfg(target_pointer_width = "64")]
impl_binop_lhsprim!(Shift64, Shr, shr, >>, usize, isize);

impl_binopassign_lhsprim!(Shift64, ShlAssign, shl_assign, <<=, u64, i64);
impl_binopassign_lhsprim!(Shift64, ShrAssign, shr_assign, >>=, u64, i64);
#[cfg(target_pointer_width = "64")]
impl_binopassign_lhsprim!(Shift64, ShlAssign, shl_assign, <<=, usize, isize);
#[cfg(target_pointer_width = "64")]
impl_binopassign_lhsprim!(Shift64, ShrAssign, shr_assign, >>=, usize, isize);

decl_trns_primitive64!(Mask64);
impl_commutative_from_prim!(Mask64, u64);
#[cfg(target_pointer_width = "64")]
impl_commutative_from_prim!(Mask64, usize);
impl_from_prim!(Mask64, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_from_prim!(Mask64, usize);

impl Mask64 {
	pub const NONE: Mask64 = Self(0u64);
	pub const FULL: Mask64 = Self(0xFFFF_FFFF_FFFF_FFFFu64);

	pub const fn new_u64(mask: u64) -> Self {
		Self(mask)
	}

	pub const fn bit_u64(idx: u64) -> Self {
		Self(1u64 << idx)
	}

	pub const fn bits_to_u64(idx: u64) -> Self {
		Self((1u64 << idx) - 1)
	}

	pub const fn bits_to_inclusive_u64(idx: u64) -> Self {
		Self(Self::bits_to_u64(idx).0 | (1u64 << idx))
	}

	pub const fn bits_from_u64(idx: u64) -> Self {
		Self(!((1u64 << idx) - 1))
	}

	pub const fn bits_range_u64(from: u64, to: u64) -> Self {
		Self(Self::bits_to_u64(to).0 & Self::bits_from_u64(from).0)
	}

	pub const fn bits_range_inclusive_u64(from: u64, to: u64) -> Self {
		Self(Self::bits_range_u64(from, to).0 | (1u64 << to))
	}

	pub const fn as_u64(self) -> u64 {
		self.0
	}

	pub fn bit<T: Into<Shift64> + Copy>(idx: T) -> Self {
		Self(1u64 << idx.into())
	}

	pub fn bits_to<T: Into<Shift64> + Copy>(idx: T) -> Self {
		Self((1u64 << idx.into()) - 1)
	}

	pub fn bits_to_inclusive<T: Into<Shift64> + Copy>(idx: T) -> Self {
		Self::bits_to(idx) | Self::bit(idx)
	}

	pub fn bits_from<T: Into<Shift64> + Copy>(idx: T) -> Self {
		Self(!((1u64 << idx.into().0) - 1))
	}

	pub fn bits_range<R: ToInclusive<Shift64>>(r: &R) -> Self {
		let ri = r.to_inclusive();
		Self::bits_to_inclusive(*ri.end()) &
			Self::bits_from(*ri.start())
	}

	pub fn lowest_bit(&self) -> Option<Shift64> {
		/*
		 * Get the special cases out of the way; 0 is common and would
		 * otherwise be most expensive, and Rust won't let us negate
		 * the largest negative signed value (rightly so).
		 */
		if (self.0 == 0) {
			return (None);
		}
		if (self.0 == 0x8000_0000_0000_0000) {
			return (Some(shift64!(63)));
		}

		let mut pos: u64 = 63;
		let mask = self.0 & ((-i64::try_from(self.0).unwrap()) as u64);

		if ((mask & 0x0000_0000_ffff_ffff) != 0) {
			pos -= 32;
		}

		if ((mask & 0x0000_ffff_0000_ffff) != 0) {
			pos -= 16;
		}

		if ((mask & 0x00ff_00ff_00ff_00ff) != 0) {
			pos -= 8;
		}

		if ((mask & 0x0f0f_0f0f_0f0f_0f0f) != 0) {
			pos -= 4;
		}

		if ((mask & 0x3333_3333_3333_3333) != 0) {
			pos -= 2;
		}

		if ((mask & 0x5555_5555_5555_5555) != 0) {
			pos -= 1;
		}

		Some(Shift64::new(pos))
	}
}

#[macro_export]
macro_rules! mask64_bits {
	($tt:tt, $($tail:tt),+) => {
		Mask64::new_u64(
			Mask64::bit_u64($crate::u64_literal_helper!($tt)).0 |
			$crate::mask64_bits!($($tail),+).0
		)
	};
	($tt:tt) => { Mask64::bit_u64($crate::u64_literal_helper!($tt)) };
	($f:tt ..) => { Mask64::bits_from_u64($f) };
	($f:tt .. $t:tt) => { Mask64::bits_range_u64($f, $t) };
	($f:tt ..= $t:tt) => { Mask64::bits_range_inclusive_u64($f, $t) };
	(.. $t:tt) => { Mask64::bits_to_u64($t) };
	(..= $t:tt) => { Mask64::bits_to_inclusive_u64($t) };
}

impl DiscreteFinite for Mask64 {
	const MIN: Self = Self(0);
	const MAX: Self = Self(u64::MAX);

	#[inline(always)]
	fn next(&self) -> Self {
		Self(self.0.saturating_add(1))
	}

	#[inline(always)]
	fn prev(&self) -> Self {
		Self(self.0.saturating_sub(1))
	}
}

impl_commutative_prim!(Mask64, BitAnd, bitand, &, u64);
impl_commutative_prim!(Mask64, BitOr, bitor, |, u64);
impl_commutative_prim!(Mask64, BitXor, bitxor, ^, u64);
#[cfg(target_pointer_width = "64")]
impl_commutative_prim!(Mask64, BitAnd, bitand, &, usize);
#[cfg(target_pointer_width = "64")]
impl_commutative_prim!(Mask64, BitOr, bitor, |, usize);
#[cfg(target_pointer_width = "64")]
impl_commutative_prim!(Mask64, BitXor, bitxor, ^, usize);
impl_binop_rhsprim!(Mask64, BitAnd, bitand, &, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_binop_rhsprim!(Mask64, BitAnd, bitand, &, usize);
impl_binop_rhsprim!(Mask64, BitOr, bitor, |, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_binop_rhsprim!(Mask64, BitOr, bitor, |, usize);
impl_binop_rhsprim!(Mask64, BitXor, bitxor, ^, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_binop_rhsprim!(Mask64, BitXor, bitxor, ^, usize);

impl_commutative_assign_prim!(Mask64, BitAndAssign, bitand_assign, &=, u64);
#[cfg(target_pointer_width = "64")]
impl_commutative_assign_prim!(Mask64, BitAndAssign, bitand_assign, &=, usize);
impl_commutative_assign_prim!(Mask64, BitOrAssign, bitor_assign, |=, u64);
#[cfg(target_pointer_width = "64")]
impl_commutative_assign_prim!(Mask64, BitOrAssign, bitor_assign, |=, usize);
impl_commutative_assign_prim!(Mask64, BitXorAssign, bitxor_assign, ^=, u64);
#[cfg(target_pointer_width = "64")]
impl_commutative_assign_prim!(Mask64, BitXorAssign, bitxor_assign, ^=, usize);

impl_binopassign_rhsprim!(Mask64,
			  BitAndAssign, bitand_assign, &=, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_binopassign_rhsprim!(Mask64, BitAndAssign, bitand_assign, &=, usize);
impl_binopassign_rhsprim!(Mask64, BitOrAssign, bitor_assign, |=, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_binopassign_rhsprim!(Mask64, BitOrAssign, bitor_assign, |=, usize);
impl_binopassign_rhsprim!(Mask64,
			  BitXorAssign, bitxor_assign, ^=, u8, u16, u32);
#[cfg(not(target_pointer_width = "64"))]
impl_binopassign_rhsprim!(Mask64, BitXorAssign, bitxor_assign, ^=, usize);

impl_binop!(Mask64, BitAnd, bitand, &, Mask64);
impl_binop!(Mask64, BitOr, bitor, |, Mask64);
impl_binop!(Mask64, BitXor, bitxor, ^, Mask64);
impl_unop!(Not, not, !, Mask64);

impl_binopassign!(Mask64, BitAndAssign, bitand_assign, &=, Mask64);
impl_binopassign!(Mask64, BitOrAssign, bitor_assign, |=, Mask64);
impl_binopassign!(Mask64, BitXorAssign, bitxor_assign, ^=, Mask64);

impl_binop_rhsprim!(Mask64, Shl, shl, <<, u64, usize);
impl_binop_rhsprim!(Mask64, Shr, shr, >>, u64, usize);

impl_binop!(Mask64, Shl, shl, <<, Shift64);
impl_binop!(Mask64, Shr, shr, >>, Shift64);
impl_binopassign!(Mask64, ShlAssign, shl_assign, <<=, Shift64);
impl_binopassign!(Mask64, ShrAssign, shr_assign, >>=, Shift64);

#[cfg(feature = "x86_paddr")]
mod paddr;

#[cfg(feature = "x86_vaddr")]
mod vaddr;
