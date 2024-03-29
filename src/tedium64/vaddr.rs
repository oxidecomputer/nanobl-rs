/*
 * Copyright 2024 Oxide Computer Company
 */

use super::*;
use x86::bits64::paging::VAddr;

impl_binop!(VAddr, BitAnd, bitand, &, Mask64);
impl_binop!(VAddr, BitOr, bitor, |, Mask64);
impl_binop!(VAddr, BitXor, bitxor, ^, Mask64);

impl_binopassign!(VAddr, BitAndAssign, bitand_assign, &=, Mask64);
impl_binopassign!(VAddr, BitOrAssign, bitor_assign, |=, Mask64);
impl_binopassign!(VAddr, BitXorAssign, bitxor_assign, ^=, Mask64);

impl Shl<Shift64> for VAddr {
	type Output = u64;
	#[inline(always)]
	fn shl(self, rhs: Shift64) -> u64 {
		self.0 << rhs.0
	}
}

impl Shr<Shift64> for VAddr {
	type Output = u64;
	#[inline(always)]
	fn shr(self, rhs: Shift64) -> u64 {
		self.0 >> rhs.0
	}
}
