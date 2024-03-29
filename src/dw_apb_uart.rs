/*
 * Copyright 2024 Oxide Computer Company
 */

use core::marker::PhantomData;
use core::ops;

pub mod regdefs;
use crate::huashan_hal::config as hal_config;
use regdefs::*;

pub mod config {
	use super::hal_config::*;
	use super::regdefs::*;
	use tock_registers::fields::FieldValue;

	pub struct UartConfig {
		pub dlh: u32,
		pub dll: u32,
		pub lcr: FieldValue<u32, LCR::Register>,
		pub mcr: FieldValue<u32, MCR::Register>,
		pub fcr: FieldValue<u32, FCR::Register>,
	}

	impl UartConfig {
		pub fn new(hc: Config) -> Result<UartConfig, ConfigError> {
			/*
			 * XXX provide a way to select clocks automatically
			 */
			let freq = hc.clock as u32;
			let baud = hc.baud_rate.get();
			let divisor: u32 = (freq / 16 + baud / 2) / baud;

			let (dlh, dll) = match divisor {
				1 ..= 0xffff => (
					(divisor & 0xff00) >> 8,
					(divisor & 0xff),
				),
				_ => {
					return (Err(ConfigError::BaudRate(
						freq / 0xf_fff0,
						freq / 16,
					)))
				}
			};
			let fcr = FCR::FIFOE::SET +
				FCR::TET::FillQuarter + FCR::RT::FillHalf;
			let mut mcr =
				MCR::OUT2::SET + MCR::RTS::SET + MCR::DTR::SET;
			if hc.flow_control {
				mcr += MCR::AFCE::SET;
			}

			let lcr = match hc.bitconfig {
				BitConfig::Data5Stop1 => {
					LCR::DLS::Bits5 + LCR::STOP::Stop1
				}
				BitConfig::Data5Stop15 => {
					LCR::DLS::Bits5 + LCR::STOP::Stop15_2
				}
				BitConfig::Data6Stop1 => {
					LCR::DLS::Bits6 + LCR::STOP::Stop1
				}
				BitConfig::Data6Stop2 => {
					LCR::DLS::Bits6 + LCR::STOP::Stop15_2
				}
				BitConfig::Data7Stop1 => {
					LCR::DLS::Bits7 + LCR::STOP::Stop1
				}
				BitConfig::Data7Stop2 => {
					LCR::DLS::Bits7 + LCR::STOP::Stop15_2
				}
				BitConfig::Data8Stop1 => {
					LCR::DLS::Bits8 + LCR::STOP::Stop1
				}
				BitConfig::Data8Stop2 => {
					LCR::DLS::Bits8 + LCR::STOP::Stop15_2
				}
			} + match hc.parity {
				Parity::None => LCR::PEN::CLEAR,
				Parity::Even => LCR::PEN::SET + LCR::EPS::SET,
				Parity::Odd => LCR::PEN::SET,
				Parity::Mark => LCR::PEN::SET + LCR::SP::SET,
				Parity::Space => {
					LCR::PEN::SET +
						LCR::EPS::SET + LCR::SP::SET
				}
			};

			Ok(UartConfig {
				dlh,
				dll,
				lcr,
				mcr,
				fcr,
			})
		}
	}
}

pub mod mode {
	pub struct Unknown;
	pub struct Configuring;
	pub struct Running;
}

use mode::*;

macro_rules! impl_derefptr {
	($n:ident, $addr:expr, $mode:ty, $regty:ty) => {
		impl ops::Deref for $n<$mode> {
			type Target = $regty;

			fn deref(&self) -> &Self::Target {
				unsafe { &*self.ptr() }
			}
		}

		impl $n<$mode> {
			fn ptr(&self) -> *const $regty {
				$addr as *const _
			}
		}
	};
}

macro_rules! impl_into_configuring {
	($n:ident, $mode:ty) => {
		impl $n<$mode> {
			pub fn into_configuring(self) -> $n<Configuring> {
				use tock_registers::interfaces::{
					ReadWriteable, Writeable,
				};
				self.SRR.write(SRR::UR::SET +
					SRR::RFR::SET + SRR::XFR::SET);
				self.LCR.modify(LCR::DLAB::SET);
				$n {
					_marker: PhantomData,
					_mode: PhantomData,
				}
			}
		}
	};
}

macro_rules! uart {
	($n:ident, $addr:expr) => {
		unsafe impl<MODE> Send for $n<MODE> {}

		pub struct $n<MODE> {
			_marker: PhantomData<*const ()>,
			_mode: PhantomData<MODE>,
		}

		impl_derefptr!($n, $addr, Unknown, UnknownRegisterBlock);
		impl_derefptr!(
			$n,
			$addr,
			Configuring,
			ConfiguringRegisterBlock
		);
		impl_derefptr!($n, $addr, Running, RunningRegisterBlock);

		impl<T> $n<T> {
			pub const ADDR: usize = $addr;
		}
		#[allow(dead_code)]
		pub const $n: usize = $addr;

		impl $n<Unknown> {
			pub fn new() -> Self {
				Self {
					_marker: PhantomData,
					_mode: PhantomData,
				}
			}
		}

		impl Default for $n<Unknown> {
			fn default() -> Self {
				Self::new()
			}
		}

		impl_into_configuring!($n, Unknown);
		impl_into_configuring!($n, Running);

		impl $n<Configuring> {
			pub fn into_running(
				self,
				cfg: config::UartConfig,
			) -> $n<Running> {
				use tock_registers::interfaces::{
					ReadWriteable, Writeable,
				};
				self.DLH.set(cfg.dlh);
				self.DLL.set(cfg.dll);
				self.FCR.write(cfg.fcr);
				self.MCR.modify(cfg.mcr);
				self.LCR.modify(cfg.lcr);
				self.LCR.modify(LCR::DLAB::CLEAR);

				$n {
					_marker: PhantomData,
					_mode: PhantomData,
				}
			}
		}
	};
}

uart!(UART0, 0xFEDC_9000);
uart!(UART1, 0xFEDC_A000);
uart!(UART2, 0xFEDC_E000);
uart!(UART3, 0xFEDC_F000);
