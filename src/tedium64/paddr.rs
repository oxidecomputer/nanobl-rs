/*
 * Copyright 2024 Oxide Computer Company
 */

use super::*;
use x86::bits64::paging::PAddr;

impl_binop!(PAddr, BitAnd, bitand, &, Mask64);
impl_binop!(PAddr, BitOr, bitor, |, Mask64);
impl_binop!(PAddr, BitXor, bitxor, ^, Mask64);

impl_binopassign!(PAddr, BitAndAssign, bitand_assign, &=, Mask64);
impl_binopassign!(PAddr, BitOrAssign, bitor_assign, |=, Mask64);
impl_binopassign!(PAddr, BitXorAssign, bitxor_assign, ^=, Mask64);

impl Shl<Shift64> for PAddr {
	type Output = u64;
	#[inline(always)]
	fn shl(self, rhs: Shift64) -> u64 {
		self.0 << rhs.0
	}
}

impl Shr<Shift64> for PAddr {
	type Output = u64;
	#[inline(always)]
	fn shr(self, rhs: Shift64) -> u64 {
		self.0 >> rhs.0
	}
}
