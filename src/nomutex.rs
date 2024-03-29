/*
 * Copyright 2024 Oxide Computer Company
 */

use core::ops::Deref;

pub struct NoMutex<T> {
	pub v: T,
}

/*
 * Safety: This crate is intended only for single-threaded use.
 */
unsafe impl<T> Sync for NoMutex<T> {}

impl<T> Deref for NoMutex<T> {
	type Target = T;

	fn deref(&self) -> &Self::Target {
		&self.v
	}
}
