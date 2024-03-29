/*
 * Copyright 2024 Oxide Computer Company
 */

#![cfg_attr(not(feature = "auto_reboot"), allow(dead_code))]

use crate::pagetable_impl::{self, MappingDescriptor, MappingOp, PageMapFlags};
use core::ops;
use nanobl_util::region_builder::{NativeRegion, RegionBuilder};
use nanobl_util::{mem_regions, pagetable::*};
use tock_registers::interfaces::{ReadWriteable, Readable};
use x86::bits64::paging::PAddr;

mod regdefs;
use regdefs::*;

/*
 * XXX Everything in this file is terrible.  Temporary hack for bringup only!
 * DO NOT SHIP!!!!!  If this is needed, do it properly and add to the HAL.
 * This subsystem is literally the example that occupies half the Embedded Rust
 * textbook, copy and paste that.
 */

/*
 * There are two relevant register blocks here, the IOMUX and the GPIO
 * controls; they reside on adjacent pages.
 */
const GPIO_BASEADDR: u64 = 0xFED8_0000;

fn map_io_page(addr: u64) -> NativeRegion<u8> {
	const SIZE: PageSize = PageSize::Size4K;
	/* XXX bitflags + const_fn, sigh */
	let flags = PageMapFlags::PCD | PageMapFlags::RW | PageMapFlags::P;

	/*
	 * We have nothing like kbm_valloc() to hand out pointers, but since
	 * this is strictly temporary code anyway we will just identity-map
	 * the page of interest.
	 */

	let multiplier = SIZE.as_size();
	let region: NativeRegion<u8> =
		RegionBuilder::new(Some(addr), Some(1) /* Pages */)
			.multiply_count_by(multiplier as u64, None)
			.forbid_region_overlap(
				&mem_regions::program_range(),
				None,
			)
			.into_native_region()
			.unwrap();

	let md = MappingDescriptor {
		op: MappingOp::Fixed,
		size: SIZE,
		base: PAddr(addr),
		flags,
	};

	pagetable_impl::map_region(&region, &md).unwrap();

	region
}

pub fn init() {
	let map1 = map_io_page(GPIO_BASEADDR);
	let map2 = map_io_page(GPIO_BASEADDR + 0x1000);

	for (pin, f) in nanobl_util::huashan_hal::uart01_iomux_reset_pins() {
		let mut mux = Pinmux::new(pin);
		mux.set(f.try_into().unwrap());
	}

	#[cfg(feature = "auto_reboot")]
	{
		/*
		 * Now tell the SP we're alive.  Mux in GPIO 144 and set its
		 * output to low.
		 */
		let mut mux = Pinmux::new(144);
		mux.set(PinFunction::F1);
		let mut ctl = Gpio::new(144);
		ctl.output(Some(false));
	}

	pagetable_impl::unmap_region(&map2);
	pagetable_impl::unmap_region(&map1);
}

pub struct Pinmux {
	regs: *const IomuxRegister,
	pin: u8,
}

impl ops::Deref for Pinmux {
	type Target = IomuxRegister;

	fn deref(&self) -> &Self::Target {
		unsafe { &*self.ptr() }
	}
}

#[allow(dead_code)]
pub enum PinFunction {
	F0,
	F1,
	F2,
	F3,
	Invalid,
}

impl core::convert::TryFrom<u8> for PinFunction {
	type Error = ();

	fn try_from(v: u8) -> Result<Self, Self::Error> {
		match (v) {
			0 => Ok(PinFunction::F0),
			1 => Ok(PinFunction::F1),
			2 => Ok(PinFunction::F2),
			3 => Ok(PinFunction::F3),
			_ => Err(()),
		}
	}
}

impl Pinmux {
	/* XXX Should have a PinIdx type here because only 0..=0x98 are valid */
	pub fn new(pin: u8) -> Self {
		Self {
			regs: (GPIO_BASEADDR + 0xD00) as *const IomuxRegister,
			pin,
		}
	}

	#[allow(dead_code)]
	pub fn get(&self) -> PinFunction {
		let sel = self.IOMUX_GPIO.read(IOMUX_GPIO::IOMUX_GPIO_X);
		match (sel) {
			0 => PinFunction::F0,
			1 => PinFunction::F1,
			2 => PinFunction::F2,
			3 => PinFunction::F3,
			_ => PinFunction::Invalid,
		}
	}

	pub fn set(&mut self, f: PinFunction) {
		let sel = match (f) {
			PinFunction::F0 => 0,
			PinFunction::F1 => 1,
			PinFunction::F2 => 2,
			PinFunction::F3 => 3,
			_ => unreachable!(),
		};

		self.IOMUX_GPIO.modify(IOMUX_GPIO::IOMUX_GPIO_X.val(sel));
	}

	fn ptr(&self) -> *const IomuxRegister {
		unsafe { (self.regs.add(self.pin.into())) as *const _ }
	}
}

struct Gpio {
	regs: *const GpioRegister,
	idx: u8,
}

impl ops::Deref for Gpio {
	type Target = GpioRegister;

	fn deref(&self) -> &Self::Target {
		unsafe { &*self.ptr() }
	}
}

impl Gpio {
	pub fn new(idx: u8) -> Self {
		Self {
			regs: (GPIO_BASEADDR + 0x1500) as *const GpioRegister,
			idx,
		}
	}

	pub fn output(&mut self, drive: Option<bool>) {
		let v = match (drive) {
			None => {
				self.GPIO.modify(GPIO::OUTPUTENABLE::CLEAR);
				return;
			}
			Some(true) => 1,
			Some(false) => 0,
		};
		self.GPIO.modify(GPIO::OUTPUTENABLE::SET +
			GPIO::PULLDOWNENABLE::CLEAR +
			GPIO::PULLUPENABLE::CLEAR +
			GPIO::OUTPUTVALUE.val(v));
	}

	#[allow(dead_code)]
	pub fn input(&self) -> Option<bool> {
		if (self.GPIO.read(GPIO::OUTPUTENABLE) != 0) {
			None
		} else {
			match (self.GPIO.read(GPIO::PINSTS)) {
				0 => Some(false),
				1 => Some(true),
				_ => unreachable!(),
			}
		}
	}

	fn ptr(&self) -> *const GpioRegister {
		unsafe { (self.regs.add(self.idx.into())) as *const _ }
	}
}
