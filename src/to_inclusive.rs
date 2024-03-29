/*
 * Copyright 2024 Oxide Computer Company
 */

use core::ops::{Bound, RangeFull};
use core::ops::{Range, RangeBounds, RangeFrom, RangeInclusive, RangeTo};

/*
 * For a discrete, bounded type T (e.g., any finite integer type), every
 * nonempty range can be expressed as a RangeInclusive<T>.  This form is most
 * convenient to work with, and we can simplify some of this code considerably
 * by using it.  This conversion routine allows callers into this subsystem to
 * supply any range-like bounds while internally using the simplest form.
 *
 * This is similar to but less flexible than the Domain trait offered by the
 * ranges crate.  In fact, there is really only one thing about this that's
 * better than that crate, and we should probably just bite the bullet and use
 * it instead.  The advantage?  We use the standard types, so that callers to
 * consumers of this don't need to use GenericRange.  That could instead be
 * wrapped in those callers, so meh.
 */
pub trait DiscreteFinite:
	PartialOrd + PartialEq + Ord + Eq + Copy + Sized
{
	const MIN: Self;
	const MAX: Self;
	fn prev(&self) -> Self;
	fn next(&self) -> Self;
}

pub trait ToInclusive<T>: RangeBounds<T>
where
	T: DiscreteFinite,
{
	fn to_inclusive(&self) -> RangeInclusive<T>;
}

macro_rules! impl_discrete_finite {
	($($ty:ty),+) => {
		$(
			impl DiscreteFinite for $ty {
				const MIN: Self = Self::MIN;
				const MAX: Self = Self::MAX;

				fn prev(&self) -> Self {
					self - 1
				}

				fn next(&self) -> Self {
					self + 1
				}
			}
		)+
	};
}

impl_discrete_finite!(u8, u16, u32, u64, u128, usize);
impl_discrete_finite!(i8, i16, i32, i64, i128, isize);

macro_rules! impl_to_inclusive_range {
	($($ty:ty),+) => {
		$(
			impl<T> ToInclusive<T> for $ty
			where T: DiscreteFinite {
			fn to_inclusive(&self) -> RangeInclusive<T> {
				let rb_start = match (self.start_bound()) {
					Bound::Unbounded => <T>::MIN,
					Bound::Included(b) => *b,
					Bound::Excluded(b) if *b == <T>::MAX =>
						return (<T>::MAX ..= <T>::MIN),
					Bound::Excluded(b) => b.next(),
				};

				let rb_end = match (self.end_bound()) {
					Bound::Unbounded => <T>::MAX,
					Bound::Included(b) => *b,
					Bound::Excluded(b) if *b == <T>::MIN =>
						return (<T>::MAX ..= <T>::MIN),
					Bound::Excluded(b) => b.prev(),
				};

				(rb_start ..= rb_end)
			}
		}
		)+
	};
}

impl_to_inclusive_range!(
	Range<T>,
	RangeFrom<T>,
	RangeTo<T>,
	RangeInclusive<T>,
	RangeFull,
	(Bound<T>, Bound<T>)
);
