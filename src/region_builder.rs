/*
 * Copyright 2024 Oxide Computer Company
 */

#![allow(dead_code)]

use crate::to_inclusive::ToInclusive;
use core::marker::PhantomData;
use core::ops::RangeInclusive;
use core::{mem, slice};

#[derive(Debug)]
pub struct RegionBuilder {
	def_addr: Option<u64>,
	def_count: Option<u64>,
	arg_addr: Option<u64>,
	arg_count: Option<u64>,
	cur_addr: Option<u64>,
	cur_count: Option<u64>,
	addr_off: u64,
	count_off: u64,
	first_error: Option<Error>,
}

#[derive(Debug)]
pub struct Region {
	pub addr: u64,
	pub addr_off: u64,
	pub count: u64,
	pub count_off: u64,
}

#[cfg(target_pointer_width = "64")]
#[derive(Debug)]
pub struct NativeRegion<T = u8>
where
	T: Copy,
{
	pub addr: usize,
	pub addr_off: usize,
	pub count: usize,
	pub count_off: usize,
	_marker: PhantomData<T>,
}

#[cfg(target_pointer_width = "64")]
impl<T> NativeRegion<T>
where
	T: Copy,
{
	pub fn as_ptr(&self) -> *const T {
		self.addr as *const T
	}

	pub fn as_mut_ptr(&mut self) -> *mut T {
		self.addr as *mut T
	}

	///
	/// # Safety
	///
	/// Requirements are identical to those for slice::from_raw_parts().
	/// Note that some of the required invariants can be guaranteed by
	/// application of appropriate constraints when building this region.
	///
	pub unsafe fn as_slice<'a>(&self) -> &'a [T] {
		slice::from_raw_parts(
			self.as_ptr(),
			self.count / mem::size_of::<T>(),
		)
	}

	///
	/// # Safety
	///
	/// Requirements are identical to those for
	/// slice::from_raw_parts_mut().  Note that some of the required
	/// invariants can be guaranteed by application of appropriate
	/// constraints when building this region.
	///
	pub unsafe fn as_mut_slice<'a>(&mut self) -> &'a mut [T] {
		slice::from_raw_parts_mut(
			self.as_mut_ptr(),
			self.count / mem::size_of::<T>(),
		)
	}

	pub fn as_range(&self) -> RangeInclusive<usize> {
		#![allow(clippy::reversed_empty_ranges)]
		if (self.count == 0) {
			(1 ..= 0)
		} else {
			(self.addr ..= self.addr + (self.count - 1))
		}
	}
}

#[derive(Debug)]
pub struct ItemRangeErr {
	pub item: u64,
	pub range: RangeInclusive<u64>,
	pub msg: &'static str,
}

impl ItemRangeErr {
	pub fn new(
		item: u64,
		range: RangeInclusive<u64>,
		optmsg: Option<&'static str>,
	) -> Self {
		Self {
			item,
			range,
			msg: optmsg.unwrap_or(
				"datum does not satisfy range constraint",
			),
		}
	}
}

#[derive(Debug)]
pub struct ItemAlignErr {
	pub item: u64,
	pub align: u64,
	pub msg: &'static str,
}

impl ItemAlignErr {
	pub fn new(
		item: u64,
		align: u64,
		optmsg: Option<&'static str>,
	) -> Self {
		Self {
			item,
			align,
			msg: optmsg.unwrap_or(
				"datum does not satisfy alignment constraint",
			),
		}
	}
}

#[derive(Debug)]
pub struct ItemConstraintErr {
	pub item: Option<u64>,
	pub msg: &'static str,
}

impl ItemConstraintErr {
	pub fn new(item: Option<u64>, msg: &'static str) -> Self {
		Self { item, msg }
	}
}

#[derive(Debug)]
pub struct ItemMissingErr {
	pub msg: &'static str,
}

impl ItemMissingErr {
	pub fn new(optmsg: Option<&'static str>) -> Self {
		Self {
			msg: optmsg.unwrap_or(
				"datum is required by this region type",
			),
		}
	}
}

#[derive(Debug)]
pub struct ItemForbiddenErr {
	pub item: u64,
	pub msg: &'static str,
}

impl ItemForbiddenErr {
	pub fn new(item: u64, optmsg: Option<&'static str>) -> Self {
		Self {
			item,
			msg: optmsg.unwrap_or(
				"datum is not allowed for this region type",
			),
		}
	}
}

#[derive(Debug)]
pub struct ItemMultipleOverflowErr {
	pub item: u64,
	pub multiple: u64,
	pub msg: &'static str,
}

impl ItemMultipleOverflowErr {
	pub fn new(
		item: u64,
		multiple: u64,
		optmsg: Option<&'static str>,
	) -> Self {
		Self {
			item,
			multiple,
			msg: optmsg.unwrap_or(
				"datum computation would overflow its type",
			),
		}
	}
}

#[derive(Debug)]
pub struct RegionOverflowErr {
	pub addr: u64,
	pub count: u64,
	pub msg: &'static str,
}

impl RegionOverflowErr {
	pub fn new(
		addr: u64,
		count: u64,
		optmsg: Option<&'static str>,
	) -> Self {
		Self {
			addr,
			count,
			msg: optmsg.unwrap_or(
				"region end would overflow its address space",
			),
		}
	}
}

#[derive(Debug)]
pub struct RegionRangeErr {
	pub addr: u64,
	pub count: u64,
	pub range: RangeInclusive<u64>,
	pub msg: &'static str,
}

impl RegionRangeErr {
	pub fn new(
		addr: u64,
		count: u64,
		range: RangeInclusive<u64>,
		optmsg: Option<&'static str>,
	) -> Self {
		Self {
			addr,
			count,
			range,
			msg: optmsg.unwrap_or(
				"region does not satisfy range constraint",
			),
		}
	}
}

#[derive(Debug)]
pub struct RegionConstraintErr {
	pub addr: Option<u64>,
	pub count: Option<u64>,
	pub msg: &'static str,
}

impl RegionConstraintErr {
	pub fn new(
		addr: Option<u64>,
		count: Option<u64>,
		msg: &'static str,
	) -> Self {
		Self { addr, count, msg }
	}
}

#[derive(Debug)]
pub enum Error {
	AddressInForbiddenRange(ItemRangeErr),
	AddressOutOfBounds(ItemRangeErr),
	AddressP2Unaligned(ItemAlignErr),
	AddressUnaligned(ItemAlignErr),
	AddressFailsConstraint(ItemConstraintErr),
	AddressMissing(ItemMissingErr),
	AddressForbidden(ItemForbiddenErr),
	CountInForbiddenRange(ItemRangeErr),
	CountOutOfBounds(ItemRangeErr),
	CountMultipleTooBig(ItemMultipleOverflowErr),
	CountNotMultiple(ItemAlignErr),
	CountFailsConstraint(ItemConstraintErr),
	CountMissing(ItemMissingErr),
	CountForbidden(ItemForbiddenErr),
	RegionTooBig(RegionOverflowErr),
	RegionInForbiddenRange(RegionRangeErr),
	RegionOutOfBounds(RegionRangeErr),
	RegionFailsConstraint(RegionConstraintErr),
}

fn clamp_to<R: ToInclusive<u64>>(v: u64, rb: &R) -> u64 {
	let ri = rb.to_inclusive();

	assert!(!ri.is_empty());
	v.clamp(*ri.start(), *ri.end())
}

/*
 * Two inclusive ranges of discrete elements overlap iff either of them
 * includes the other's starting bound.  This is trivial to check in one
 * direction because we require one of the ranges to be an inclusive range
 * already.  The other side requires slightly more work; we must obtain the
 * starting bound for the generic constraint as if it were an inclusive range.
 * This can be complicated further by either range being empty, in which case
 * there is no overlap.
 */
/*
 * XXX Attributes on expressions are experimental, and custom inner attributes
 * are unstable.  So this is apparently the only way to get around rustfmt's
 * inability to format the tail of this function.  Thanks, rustfmt!
 */
#[rustfmt::skip]
#[allow(clippy::suspicious_operation_groupings)]
pub fn ranges_overlap<R: ToInclusive<u64>>(
	first: &RangeInclusive<u64>,
	bound: &R,
) -> bool {
	let second = bound.to_inclusive();

	!first.is_empty() &&
	!second.is_empty() &&
	(first.contains(second.start()) || second.contains(first.start()))
}

impl RegionBuilder {
	pub fn new(addr: Option<u64>, count: Option<u64>) -> Self {
		Self {
			def_addr: None,
			def_count: None,
			arg_addr: addr,
			arg_count: count,
			cur_addr: addr,
			cur_count: count,
			addr_off: 0,
			count_off: 0,
			first_error: None,
		}
	}

	pub fn default_address(mut self, addr: u64) -> Self {
		self.def_addr = Some(addr);
		if (self.cur_addr.is_none()) {
			self.cur_addr = Some(addr);
		}
		self
	}

	pub fn default_count(mut self, count: u64) -> Self {
		self.def_count = Some(count);
		if (self.cur_count.is_none()) {
			self.cur_count = Some(count);
		}
		self
	}

	pub fn forbid_address(mut self, optmsg: Option<&'static str>) -> Self {
		if let Some(addr) = self.arg_addr {
			self.cur_addr = None;
			self.first_error.get_or_insert(
				Error::AddressForbidden(ItemForbiddenErr::new(
					addr, optmsg,
				)),
			);
		}
		self
	}

	pub fn forbid_count(mut self, optmsg: Option<&'static str>) -> Self {
		if let Some(count) = self.arg_count {
			self.cur_count = None;
			self.first_error.get_or_insert(Error::CountForbidden(
				ItemForbiddenErr::new(count, optmsg),
			));
		}
		self
	}

	pub fn forbid_address_in<R: ToInclusive<u64>>(
		mut self,
		r: &R,
		optmsg: Option<&'static str>,
	) -> Self {
		if let Some(addr) = self.cur_addr {
			if (r.contains(&addr)) {
				let ri = r.to_inclusive();
				assert!(!ri.is_empty());
				self.first_error.get_or_insert(
					Error::AddressInForbiddenRange(
						ItemRangeErr::new(
							addr, ri, optmsg,
						),
					),
				);
				self.cur_addr = None;
			}
		}
		self
	}

	pub fn bound_address_in<R: ToInclusive<u64>>(
		self,
		r: &R,
		optmsg: Option<&'static str>,
	) -> Self {
		self.bound_address_in_any(&[r], optmsg)
	}

	pub fn bound_address_in_any<R: ToInclusive<u64>>(
		mut self,
		rs: &[&R],
		optmsg: Option<&'static str>,
	) -> Self {
		if let Some(addr) = self.cur_addr {
			for r in rs.iter() {
				if (r.contains(&addr)) {
					return (self);
				}
			}

			self.cur_addr = None;
			if (rs.len() == 1) {
				let ri = rs[0].to_inclusive();
				assert!(!ri.is_empty());
				self.first_error.get_or_insert(
					Error::AddressOutOfBounds(
						ItemRangeErr::new(
							addr, ri, optmsg,
						),
					),
				);
			} else {
				self.first_error.get_or_insert(
					Error::AddressFailsConstraint(
						ItemConstraintErr::new(
							Some(addr),
							optmsg.unwrap_or(
		"address satisfies none of multiple range constraints"))));
			}
		}
		self
	}

	pub fn clamp_address_to<R: ToInclusive<u64>>(mut self, r: &R) -> Self {
		if let Some(addr) = self.cur_addr {
			self.cur_addr = Some(clamp_to(addr, r));
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn align_address_down_to<T: Into<u64>>(mut self, align: T) -> Self {
		let align = align.into();
		assert!(align.is_power_of_two());

		if let Some(addr) = self.cur_addr {
			self.addr_off = addr & (align - 1);
			self.cur_addr = Some(addr & !(align - 1));
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn align_address_up_to<T: Into<u64>>(mut self, align: T) -> Self {
		let align = align.into();
		assert!(align.is_power_of_two());

		if let Some(addr) = self.cur_addr {
			self.addr_off = 0;
			self.cur_addr = addr
				.checked_add(align - 1)
				.map(|a| a & !(align - 1));
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn bound_address_p2align<T: Into<u64>>(
		mut self,
		align: T,
		optmsg: Option<&'static str>,
	) -> Self {
		let align = align.into();
		assert!(align.is_power_of_two());

		if let Some(addr) = self.cur_addr {
			if ((addr & (align - 1)) != 0) {
				self.first_error.get_or_insert(
					Error::AddressP2Unaligned(
						ItemAlignErr::new(
							addr, align, optmsg,
						),
					),
				);
				self.cur_addr = None;
			}
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn bound_address_align<T: Into<u64>>(
		mut self,
		align: T,
		optmsg: Option<&'static str>,
	) -> Self {
		let align = align.into();
		assert!(align != 0);

		if let Some(addr) = self.cur_addr {
			if (addr % align != 0) {
				self.first_error.get_or_insert(
					Error::AddressUnaligned(
						ItemAlignErr::new(
							addr, align, optmsg,
						),
					),
				);
				self.cur_addr = None;
			}
		}
		self
	}

	pub fn bound_address_with<F>(mut self, f: F) -> Self
	where
		F: Fn(Option<u64>) -> Result<(), &'static str>,
	{
		let res = f(self.cur_addr);
		if let Err(e) = res {
			self.first_error.get_or_insert(
				Error::AddressFailsConstraint(
					ItemConstraintErr::new(
						self.cur_addr,
						e,
					),
				),
			);
			self.cur_addr = None;
		}
		self
	}

	pub fn forbid_count_in<R: ToInclusive<u64>>(
		mut self,
		r: &R,
		optmsg: Option<&'static str>,
	) -> Self {
		if let Some(count) = self.cur_count {
			if (r.contains(&count)) {
				let ri = r.to_inclusive();
				assert!(!ri.is_empty());
				self.first_error.get_or_insert(
					Error::CountInForbiddenRange(
						ItemRangeErr::new(
							count, ri, optmsg,
						),
					),
				);
				self.cur_count = None;
			}
		}
		self
	}

	pub fn bound_count_in<R: ToInclusive<u64>>(
		mut self,
		r: &R,
		optmsg: Option<&'static str>,
	) -> Self {
		if let Some(count) = self.cur_count {
			if (!r.contains(&count)) {
				let ri = r.to_inclusive();
				assert!(!ri.is_empty());
				self.first_error.get_or_insert(
					Error::CountOutOfBounds(
						ItemRangeErr::new(
							count, ri, optmsg,
						),
					),
				);
				self.cur_count = None;
			}
		}
		self
	}

	pub fn clamp_count_to<R: ToInclusive<u64>>(mut self, r: &R) -> Self {
		if let Some(count) = self.cur_count {
			self.cur_count = Some(clamp_to(count, r));
		}
		self
	}

	pub fn bound_count_with<F>(mut self, f: F) -> Self
	where
		F: Fn(Option<u64>) -> Result<(), &'static str>,
	{
		let res = f(self.cur_count);
		if let Err(e) = res {
			self.first_error.get_or_insert(
				Error::CountFailsConstraint(
					ItemConstraintErr::new(
						self.cur_count,
						e,
					),
				),
			);
			self.cur_count = None;
		}
		self
	}

	pub fn multiply_count_by(
		mut self,
		v: u64,
		optmsg: Option<&'static str>,
	) -> Self {
		if let Some(count) = self.cur_count {
			self.cur_count = count.checked_mul(v);
			if (self.cur_count.is_none()) {
				self.first_error.get_or_insert(
					Error::CountMultipleTooBig(
						ItemMultipleOverflowErr::new(
							count, v, optmsg,
						),
					),
				);
			}
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn align_count_down_to<T: Into<u64>>(mut self, align: T) -> Self {
		let align = align.into();
		assert!(align.is_power_of_two());

		if let Some(count) = self.cur_count {
			self.count_off = 0;
			self.cur_count = Some(count & !(align - 1));
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn align_count_up_to<T: Into<u64>>(mut self, align: T) -> Self {
		let align = align.into();
		assert!(align.is_power_of_two());

		if let Some(count) = self.cur_count {
			self.cur_count = count
				.checked_add(align - 1)
				.map(|c| c & !(align - 1));
			self.count_off =
				self.cur_count.map_or(0, |c| c - count);
		}
		self
	}

	#[cfg(any(target_pointer_width = "32", target_pointer_width = "64"))]
	pub fn bound_count_multiple<T: Into<u64>>(
		mut self,
		v: T,
		optmsg: Option<&'static str>,
	) -> Self {
		let v = v.into();
		assert!(v != 0);

		if let Some(count) = self.cur_count {
			if (count % v != 0) {
				self.first_error.get_or_insert(
					Error::CountNotMultiple(
						ItemAlignErr::new(
							count, v, optmsg,
						),
					),
				);
				self.cur_count = None;
			}
		}
		self
	}

	fn constrain_as_region(&mut self, optmsg: Option<&'static str>) {
		match (self.cur_addr, self.cur_count) {
			(None, _) => self.cur_count = None,
			(_, None) | (Some(_), Some(0)) => (),
			(Some(addr), Some(count)) => {
				if (addr.checked_add(count - 1).is_none()) {
					self.first_error.get_or_insert(
						Error::RegionTooBig(
							RegionOverflowErr::new(
								addr, count,
								optmsg,
							),
						),
					);
					self.cur_addr = None;
					self.cur_count = None;
				}
			}
		};
	}

	pub fn forbid_region_overlap<R: ToInclusive<u64>>(
		mut self,
		r: &R,
		optmsg: Option<&'static str>,
	) -> Self {
		self.constrain_as_region(optmsg);

		match (self.cur_addr, self.cur_count) {
			(Some(_), Some(0)) | (Some(_), None) => {
				return (self.forbid_address_in(r, optmsg))
			}
			(None, _) => (),
			(Some(addr), Some(count)) => {
				let region = (addr ..= addr + (count - 1));
				if (ranges_overlap(&region, r)) {
					let ri = r.to_inclusive();
					assert!(!ri.is_empty());
					self.first_error.get_or_insert(
						Error::RegionInForbiddenRange(
							RegionRangeErr::new(
								addr, count,
								ri, optmsg,
							),
						),
					);
					self.cur_addr = None;
					self.cur_count = None;
				}
			}
		};
		self
	}

	pub fn bound_region_in<R: ToInclusive<u64>>(
		self,
		r: &R,
		optmsg: Option<&'static str>,
	) -> Self {
		self.bound_region_in_any(&[r], optmsg)
	}

	pub fn bound_region_in_any<R: ToInclusive<u64>>(
		mut self,
		rs: &[&R],
		optmsg: Option<&'static str>,
	) -> Self {
		self.constrain_as_region(optmsg);

		match (self.cur_addr, self.cur_count) {
			(Some(_), Some(0)) | (Some(_), None) => {
				return (self.bound_address_in_any(rs, optmsg))
			}
			(None, _) => (),
			(Some(addr), Some(count)) => {
				for r in rs.iter() {
					if (r.contains(&addr) &&
						r.contains(
							&(addr + (count - 1)),
						)) {
						return (self);
					}
				}

				self.cur_addr = None;
				self.cur_count = None;
				if (rs.len() == 1) {
					let ri = rs[0].to_inclusive();
					assert!(!ri.is_empty());
					self.first_error.get_or_insert(
						Error::RegionOutOfBounds(
							RegionRangeErr::new(
								addr, count,
								ri, optmsg,
							),
						),
					);
					return (self);
				}

				self.first_error.get_or_insert(
					Error::RegionFailsConstraint(
						RegionConstraintErr::new(
							Some(addr), Some(count),
							optmsg.unwrap_or(
		"region satisfies none of multiple range constraints"))));
			}
		}
		self
	}

	pub fn bound_region_with<F>(mut self, f: F) -> Self
	where
		F: Fn(Option<u64>, Option<u64>) -> Result<(), &'static str>,
	{
		self.constrain_as_region(Some(
			"regions must fit within the address space",
		));
		let res = f(self.cur_addr, self.cur_count);
		if let Err(e) = res {
			self.first_error.get_or_insert(
				Error::RegionFailsConstraint(
					RegionConstraintErr::new(
						self.cur_addr,
						self.cur_count,
						e,
					),
				),
			);
			self.cur_addr = None;
			self.cur_count = None;
		}
		self
	}

	pub fn into_inner(
		self,
	) -> Result<(Option<u64>, u64, Option<u64>, u64), Error> {
		if let Some(e) = self.first_error {
			Err(e)
		} else {
			Ok((
				self.cur_addr,
				self.addr_off,
				self.cur_count,
				self.count_off,
			))
		}
	}

	pub fn into_tuple(self) -> Result<(u64, u64, u64, u64), Error> {
		let inner = self.into_inner()?;

		let a = inner.0.ok_or_else(|| {
			Error::AddressMissing(ItemMissingErr::new(None))
		})?;
		let c = inner.2.ok_or_else(|| {
			Error::CountMissing(ItemMissingErr::new(None))
		})?;

		Ok((a, inner.1, c, inner.3))
	}

	pub fn into_addr(self) -> Result<u64, Error> {
		let inner = self.into_inner()?;

		inner.0.ok_or_else(|| {
			Error::AddressMissing(ItemMissingErr::new(None))
		})
	}

	pub fn into_count(self) -> Result<u64, Error> {
		let inner = self.into_inner()?;

		inner.2.ok_or_else(|| {
			Error::CountMissing(ItemMissingErr::new(None))
		})
	}

	pub fn into_region(mut self) -> Result<Region, Error> {
		self.constrain_as_region(Some(
			"regions must fit within the address space",
		));
		let t = self.into_tuple()?;

		Ok(Region {
			addr: t.0,
			addr_off: t.1,
			count: t.2,
			count_off: t.3,
		})
	}

	#[cfg(target_pointer_width = "64")]
	#[inline]
	pub fn into_inner_native(
		self,
	) -> Result<(Option<usize>, usize, Option<usize>, usize), Error> {
		let inner = self.into_inner()?;

		Ok((
			inner.0.map(|v| v as usize),
			inner.1 as usize,
			inner.2.map(|v| v as usize),
			inner.3 as usize,
		))
	}

	#[cfg(target_pointer_width = "64")]
	#[inline]
	pub fn into_tuple_native(
		self,
	) -> Result<(usize, usize, usize, usize), Error> {
		let t = self.into_tuple()?;

		Ok((t.0 as usize, t.1 as usize, t.2 as usize, t.3 as usize))
	}

	#[cfg(target_pointer_width = "64")]
	#[inline]
	pub fn into_addr_native(self) -> Result<usize, Error> {
		Ok(self.into_addr()? as usize)
	}

	#[cfg(target_pointer_width = "64")]
	#[inline]
	pub fn into_count_native(self) -> Result<usize, Error> {
		Ok(self.into_count()? as usize)
	}

	#[cfg(target_pointer_width = "64")]
	pub fn into_native_region<T>(mut self) -> Result<NativeRegion<T>, Error>
	where
		T: Copy,
	{
		self.constrain_as_region(Some(
			"regions must fit within the address space",
		));
		let t = self
			.bound_address_align(
				core::mem::align_of::<T>() as u64,
				Some("required for requested type"),
			)
			.bound_count_multiple(
				mem::size_of::<T>() as u64,
				Some("required for requested type"),
			)
			.into_tuple_native()?;

		Ok(NativeRegion {
			addr: t.0,
			addr_off: t.1,
			count: t.2,
			count_off: t.3,
			_marker: PhantomData,
		})
	}
}
