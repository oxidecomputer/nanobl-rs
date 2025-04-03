/*
 * Copyright 2025 Oxide Computer Company
 */

#![no_std]
#![feature(naked_functions)]

pub mod console;
pub(crate) mod dw_apb_uart;
pub(crate) mod huashan;
pub mod huashan_hal;
pub(crate) mod nomutex;

pub mod delay;
pub mod idt;
pub mod mem_regions;
pub mod pagetable;
pub mod panic;
pub mod region_builder;
pub mod tedium64;
pub mod to_inclusive;

pub mod prelude {
	pub use crate::eprint;
	pub use crate::eprintln;
	pub use crate::mask64_bits;
	pub use crate::print;
	pub use crate::println;
	pub use crate::shift64;

	#[cfg(feature = "panic_console")]
	pub use crate::pprint;
	#[cfg(feature = "panic_console")]
	pub use crate::pprintln;
}
