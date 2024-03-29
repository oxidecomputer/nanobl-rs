/*
 * Copyright 2024 Oxide Computer Company
 */

use core::convert::TryFrom;

use embedded_hal::blocking::serial as blocking_serial;
use embedded_hal::prelude::*;
use embedded_hal::serial;
use nb::block;
use tock_registers::interfaces::{Readable, Writeable};
use tock_registers::LocalRegisterCopy;

use crate::dw_apb_uart::config as uart_config;
use crate::dw_apb_uart::{self, mode::*, regdefs::*};

/*
 * XXX Things to add here:
 * - A callback to get the list of available clock frequencies
 * - A callback to select one of those clocks for this port and turn it on
 * - A callback to set up pinmuxing for this port
 * - Some way to configure all the ports at once, because they share pins (or
 *   perhaps a way to express priority? Another way would be to keep a map of
 *   pins that have been claimed and then fail this -- which also requires a
 *   singleton port inventory upstream of us.)
 */

fn is_sp5() -> bool {
	const X86_SOCKET_SP5: u32 = 4;
	let cpuid = x86::cpuid::CpuId::new();
	let feats = cpuid.get_feature_info().unwrap();

	match (feats.family_id(), feats.model_id(), feats.stepping_id()) {
		#[rustfmt::skip]
		// Genoa
		(0x19, 0x10..=0x1f, 0x0..=0xf) |
		// Bergamo / Sienna
		(0x19, 0xa0..=0xaf, 0x0..=0xf) |
		// Turin
		(0x1a, 0x00..=0x1f, 0x0..=0xf) => {}

		_ => return false,
	}

	let pkg_type = cpuid
		.get_extended_processor_and_feature_identifiers()
		.unwrap()
		.pkg_type();
	pkg_type == X86_SOCKET_SP5
}

pub fn uart01_iomux_reset_pins() -> impl core::iter::Iterator<Item = (u8, u8)> {
	/*
	 * Configure the IOMUX pins for UART0 & UART1.
	 *
	 * According to the Milan/Genoa PPRs, the IOMUX config and reset values
	 * are as follows:
	 *        +-----+-------------+-------------+
	 *        | Pin |    Milan    |    Genoa    |
	 *        |     | Reset | F0  | Reset | F0  |
	 *        +-----+-------+-----+-------+-----+
	 *  UART0 | 135 |   2   | CTS |   0   | CTS |
	 *        | 136 |   0   | RXD |   0   | RXD |
	 *        | 137 |   2   | RTS |   0   | RTS |
	 *        | 138 |   1   | TXD |   0   | TXD |
	 *        +-----+-------+-----+-------+-----+
	 *  UART1 | 140 |   2   | CTS |       |     |
	 *        | 141 |   0   | RXD |   0   | RXD |
	 *        | 142 |   2   | RTS |   0   | TXD |
	 *        | 143 |   1   | TXD |       |     |
	 *        +-----+-------+-----+-------+-----+
	 *
	 * On Milan then, we definitely need to set the values appropriately for
	 * both UARTs.
	 *
	 * XXX: On Genoa, the reset values would be correct but the observed
	 * values differ, with pins 135-138 matching the Milan reset state and
	 * 142=1.
	 *
	 * Furthermore, if the ABL is configured to output to either UART, that
	 * UART will be appropriately configured by the time we get here.
	 *
	 * Either way, we'll just assume nothing and set the values we want.
	 *
	 * Note: Genoa (and SP5 in general) only supports flow control on UART0.
	 */
	let is_sp5 = is_sp5();
	(135 ..= 138).chain(140 ..= 143).filter_map(move |pin| {
		if is_sp5 && (pin == 140 || pin == 143) {
			return None;
		}
		Some((pin, 0))
	})
}

pub mod config {
	use core::num::NonZeroU32;

	#[allow(clippy::upper_case_acronyms)]
	#[non_exhaustive]
	pub enum Clock {
		Legacy = 1_843_200,
		APB = 48_000_000,
	}

	#[non_exhaustive]
	pub enum Parity {
		None,
		Even,
		Odd,
		Mark,
		Space,
	}

	#[non_exhaustive]
	pub enum BitConfig {
		Data5Stop1,
		Data5Stop15,
		Data6Stop1,
		Data6Stop2,
		Data7Stop1,
		Data7Stop2,
		Data8Stop1,
		Data8Stop2,
	}

	#[derive(Debug)]
	#[non_exhaustive]
	pub enum ConfigError {
		BaudRate(u32, u32),
		InvalidUart,
	}

	pub struct Config {
		pub baud_rate: NonZeroU32,
		pub bitconfig: BitConfig,
		pub parity: Parity,
		pub clock: Clock, /* XXX */
		pub flow_control: bool,
	}

	impl Config {
		pub fn baud_rate(mut self, baud: NonZeroU32) -> Self {
			self.baud_rate = baud;
			self
		}

		pub fn bitconfig(mut self, bc: BitConfig) -> Self {
			self.bitconfig = bc;
			self
		}

		pub fn parity(mut self, par: Parity) -> Self {
			self.parity = par;
			self
		}

		pub fn clock(mut self, cf: Clock) -> Self {
			self.clock = cf;
			self
		}

		pub fn flow_control(mut self, fc: bool) -> Self {
			self.flow_control = fc;
			self
		}
	}

	impl Default for Config {
		fn default() -> Config {
			Config {
				baud_rate: NonZeroU32::new(115_200).unwrap(),
				bitconfig: BitConfig::Data8Stop1,
				parity: Parity::None,
				clock: Clock::APB,
				flow_control: true,
			}
		}
	}
}

pub struct Serial<UART> {
	pub uart: UART,
}

pub trait SerialExt<UART>: Sized {
	fn serial(
		self,
		config: config::Config,
	) -> Result<Serial<UART>, config::ConfigError>;
}

pub trait SerialDev:
	serial::Read<u8, Error = RxError>
	+ serial::Write<u8, Error = TxError>
	+ blocking_serial::Write<u8, Error = TxError>
{
	fn read_exact(&mut self, buf: &mut [u8]) -> Result<(), RxError>;
	fn read_exact_unchecked(&mut self, but: &mut [u8]);
}

#[derive(Debug)]
#[non_exhaustive]
pub enum RxError {
	Break(usize),
	Framing(usize),
	Parity(usize),
}

#[derive(Debug)]
#[non_exhaustive]
pub enum TxError {}

fn check_lsr(
	lsr: LocalRegisterCopy<u32, LSR::Register>,
	off: usize,
) -> Result<(), RxError> {
	/*
	 * Receiver errors:
	 *
	 * BI - This is not an error at all but rather an indicator that a
	 * break was received.  It's asserted with the zero byte in the
	 * receiver.  We want to read the byte and return an error to indicate
	 * this, RxError::Break.
	 *
	 * FE - Framing error.  This is likely to be asserted with BI, but if
	 * BI is clear we will read the probably bad byte from the receiver and
	 * return RxError::Framing.
	 *
	 * PE - Parity error.  Like FE, if BI is not set and FE is not set, we
	 * read the definitely bad byte from the receiver and return
	 * RxError::Parity.
	 *
	 * OE - Overrun error.  There's no way for this interface to indicate
	 * this kind of error, which means that incoming data has been lost.
	 * The data we can obtain from the receiver is valid, so we'll return
	 * it.  This should never happen to us because we always enable
	 * automatic flow control, but it's possible that the other side
	 * doesn't support it and just ignores our deassertion of RTS.
	 *
	 * OE and FE were cleared by our read of LSR.  The others can be cleared
	 * by a read of RBR (unless the next byte received also has an error).
	 * The caller is responsible for performing that read if appropriate.
	 */

	if (lsr.is_set(LSR::BI)) {
		return (Err(RxError::Break(off)));
	}
	if (lsr.is_set(LSR::FE)) {
		return (Err(RxError::Framing(off)));
	}
	if (lsr.is_set(LSR::PE)) {
		return (Err(RxError::Parity(off)));
	}

	Ok(())
}

fn propagate_errcount(e: RxError, base: usize) -> RxError {
	match (e) {
		RxError::Break(off) => RxError::Break(base + off),
		RxError::Framing(off) => RxError::Framing(base + off),
		RxError::Parity(off) => RxError::Parity(base + off),
	}
}

macro_rules! serial {
	($UARTX:ident, $uartx:ident) => {
		impl Serial<dw_apb_uart::$UARTX<Configuring>> {
			pub fn $uartx(
				uart: dw_apb_uart::$UARTX<Configuring>,
				mut config: config::Config,
			) -> Result<
				Serial<dw_apb_uart::$UARTX<Running>>,
				config::ConfigError,
			> {
				/* avoid crashing rustfmt */
				let bad_uart = config::ConfigError::InvalidUart;

				if is_sp5() {
					// SP5 only supports flow control on
					// UART0.
					// XXX: Enabling UART2 prohibits flow
					// control on UART0.
					if dw_apb_uart::$UARTX::<()>::ADDR !=
						dw_apb_uart::UART0
					{
						config.flow_control = false;
					}

					// SP5 doesn't have UART3
					if dw_apb_uart::$UARTX::<()>::ADDR ==
						dw_apb_uart::UART3
					{
						return Err(bad_uart);
					}
				}
				let cfg = uart_config::UartConfig::new(config)?;
				let ur = uart.into_running(cfg);

				Ok(Serial { uart: ur })
			}

			pub fn release(
				self,
			) -> dw_apb_uart::$UARTX<Configuring> {
				self.uart
			}
		}

		impl Serial<dw_apb_uart::$UARTX<Running>> {
			pub fn release(self) -> dw_apb_uart::$UARTX<Running> {
				self.uart
			}

			/*
			 * It turns out that with caches disabled, our limiting
			 * factor in reception speed becomes this code.  To
			 * reach the possible 3 Mbps, we need all of the
			 * following:
			 *
			 * - optimisation level 2 - 8-byte block reads - a
			 * zerocopy receive path - avoiding function calls in
			 * the block read path
			 *
			 * So we have two variants of this, and two of
			 * read_exact that correspond to them.  This one
			 * provides precise error detail if one occurs: the
			 * type of error and the location within the stream.
			 * The _unchecked variant below can't.  It's possible
			 * (just barely) to read LSR between bytes and or that
			 * together at the end of the function to provide more
			 * or less a boolean indicator of error, so we could
			 * improve that version later if needed.  This seems to
			 * be because the tock_register interface is not a
			 * zero-cost abstraction, as this results in a call to
			 * ::zero() for each check that apparently isn't
			 * possible to inline.  While that merits some more
			 * exploration, for the moment we provide an
			 * alternative interface for consumers that do their
			 * own higher level error detection and correction;
			 * e.g., Xmodem.
			 */
			pub fn read_8(&mut self) -> Result<u64, RxError> {
				let r = &self.uart.RBR_THR;
				let mut bytes;

				check_lsr(self.uart.LSR.extract(), 0)?;
				bytes = (r.get() as u64) & 0xff;
				check_lsr(self.uart.LSR.extract(), 1)?;
				bytes |= ((r.get() as u64) & 0xff) << 8;
				check_lsr(self.uart.LSR.extract(), 2)?;
				bytes |= ((r.get() as u64) & 0xff) << 16;
				check_lsr(self.uart.LSR.extract(), 3)?;
				bytes |= ((r.get() as u64) & 0xff) << 24;
				check_lsr(self.uart.LSR.extract(), 4)?;
				bytes |= ((r.get() as u64) & 0xff) << 32;
				check_lsr(self.uart.LSR.extract(), 5)?;
				bytes |= ((r.get() as u64) & 0xff) << 40;
				check_lsr(self.uart.LSR.extract(), 6)?;
				bytes |= ((r.get() as u64) & 0xff) << 48;
				check_lsr(self.uart.LSR.extract(), 7)?;
				bytes |= ((r.get() as u64) & 0xff) << 56;

				Ok(bytes.to_le())
			}

			pub fn read_8_unchecked(&mut self) -> u64 {
				let r = &self.uart.RBR_THR;
				let mut bytes;

				bytes = (r.get() as u64) & 0xff;
				bytes |= ((r.get() as u64) & 0xff) << 8;
				bytes |= ((r.get() as u64) & 0xff) << 16;
				bytes |= ((r.get() as u64) & 0xff) << 24;
				bytes |= ((r.get() as u64) & 0xff) << 32;
				bytes |= ((r.get() as u64) & 0xff) << 40;
				bytes |= ((r.get() as u64) & 0xff) << 48;
				bytes |= ((r.get() as u64) & 0xff) << 56;

				bytes.to_le()
			}

			fn read_exact_helper(
				&mut self,
				buf: &mut [u8],
				checked: bool,
			) -> Result<(), RxError> {
				let mut count = 0;

				while (buf.len() - count > 8) {
					let avail = usize::try_from(
						self.uart.RFL.get(),
					)
					.unwrap();

					if (avail < 8) {
						continue;
					}

					let bytes = if (checked) {
						self.read_8().map_err(|e| {
							propagate_errcount(
								e, count,
							)
						})?
					} else {
						self.read_8_unchecked()
					};

					/*
					 * Safety: Destination is as valid as
					 * the user makes it.
					 */
					unsafe {
						core::ptr::write_unaligned(
							buf[count ..]
								.as_mut_ptr()
								as *mut u64,
							bytes,
						);
					}

					count += 8;
				}

				while (buf.len() > count) {
					let avail = usize::try_from(
						self.uart.RFL.get(),
					)
					.unwrap();

					if (avail == 0) {
						continue;
					}

					let byte = block!(self.read())
						.map_err(|e| {
							propagate_errcount(
								e, count,
							)
						})?;

					/*
					 * Safety: Destination is as valid as
					 * the user makes it.
					 */
					unsafe {
						core::ptr::write_unaligned(
							buf[count ..]
								.as_mut_ptr(),
							byte,
						);
					}

					count += 1;
				}

				Ok(())
			}
		}

		impl SerialExt<dw_apb_uart::$UARTX<Running>>
			for dw_apb_uart::$UARTX<Configuring>
		{
			fn serial(
				self,
				config: config::Config,
			) -> Result<
				Serial<dw_apb_uart::$UARTX<Running>>,
				config::ConfigError,
			> {
				Serial::$uartx(self, config)
			}
		}

		impl SerialExt<dw_apb_uart::$UARTX<Running>>
			for dw_apb_uart::$UARTX<Unknown>
		{
			fn serial(
				self,
				config: config::Config,
			) -> Result<
				Serial<dw_apb_uart::$UARTX<Running>>,
				config::ConfigError,
			> {
				Serial::$uartx(self.into_configuring(), config)
			}
		}

		impl SerialDev for Serial<dw_apb_uart::$UARTX<Running>> {
			fn read_exact(
				&mut self,
				buf: &mut [u8],
			) -> Result<(), RxError> {
				self.read_exact_helper(buf, true)
			}

			fn read_exact_unchecked(&mut self, buf: &mut [u8]) {
				self.read_exact_helper(buf, false).unwrap_or(())
			}
		}

		impl blocking_serial::Write<u8>
			for Serial<dw_apb_uart::$UARTX<Running>>
		{
			type Error = TxError;

			fn bwrite_all(
				&mut self,
				buf: &[u8],
			) -> Result<(), TxError> {
				Ok(for b in buf {
					block!(self.write(*b)).unwrap();
				})
			}

			fn bflush(&mut self) -> Result<(), TxError> {
				block!(self.flush())
			}
		}

		impl serial::Write<u8>
			for Serial<dw_apb_uart::$UARTX<Running>>
		{
			type Error = TxError;

			fn write(&mut self, b: u8) -> nb::Result<(), TxError> {
				if (self.uart.USR.is_set(USR::TFNF)) {
					self.uart.RBR_THR.set(b.into());
					Ok(())
				} else {
					Err(nb::Error::WouldBlock)
				}
			}

			fn flush(&mut self) -> nb::Result<(), TxError> {
				if (self.uart.LSR.is_set(LSR::TEMT)) {
					Ok(())
				} else {
					Err(nb::Error::WouldBlock)
				}
			}
		}

		impl serial::Read<u8> for Serial<dw_apb_uart::$UARTX<Running>> {
			type Error = RxError;

			fn read(&mut self) -> nb::Result<u8, RxError> {
				let lsr = self.uart.LSR.extract();

				if (!lsr.is_set(LSR::DR)) {
					return (Err(nb::Error::WouldBlock));
				}

				check_lsr(lsr, 0).map_err(|e| match (e) {
					RxError::Parity(_) |
					RxError::Break(_) => {
						self.uart.RBR_THR.get();
						e
					}
					RxError::Framing(_) => e,
				})?;

				Ok(self.uart.RBR_THR.get() as u8)
			}
		}
	};
}

serial!(UART0, uart0);
serial!(UART1, uart1);
serial!(UART2, uart2);
serial!(UART3, uart3);
