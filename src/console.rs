/*
 * Copyright 2024 Oxide Computer Company
 */

use core::cell::RefCell;
use core::convert::From;
pub use core::fmt::Write as FmtWrite;
use core::fmt::{self, Debug, Display};
use core::num::*;
use core::str;

use lazy_static::*;
use nb::block;
use void::Void;
use xmodem::io::{self as io, Read, Write};

use crate::delay;
use crate::dw_apb_uart::{self, mode};
use crate::huashan;
use crate::huashan_hal::*;
use crate::nomutex::*;

use heapless::{String, Vec};

macro_rules! console {
	($dev:ident, $baud:expr, $clock:ident, $with_fn:ident) => {
		fn $with_fn<F, T>(mut f: F) -> T
		where
			F: FnMut(&mut dyn SerialDev) -> T,
		{
			type ConDev = Serial<dw_apb_uart::$dev<mode::Running>>;
			lazy_static! {
				static ref CONSOLE: NoMutex<RefCell<ConDev>> = {
					let br =
						NonZeroU32::new($baud).unwrap();
					let cfg = config::Config {
						..Default::default()
					}
					.baud_rate(br)
					.clock(config::Clock::$clock);

					let sd = huashan::$dev
						.borrow_mut()
						.take()
						.unwrap()
						.serial(cfg)
						.unwrap();

					NoMutex {
						v: RefCell::new(sd),
					}
				};
			};

			let c = &mut *CONSOLE.borrow_mut();

			f(c)
		}
	};

	($dev:ident, $baud:expr, $clock:ident) => {
		console!($dev, $baud, $clock, with);
	};

	(PANIC @ $dev:ident, $baud:expr, $clock:ident) => {
		#[cfg(feature = "panic_console")]
		console!($dev, $baud, $clock, with_panic);
		#[cfg(not(feature = "panic_console"))]
		compile_error!("panic_console feature not enabled");
	};
}

include!(concat!(env!("OUT_DIR"), "/condefs.rs"));

#[derive(Debug)]
pub enum Error {
	Io(io::Error),
	Conversion(str::Utf8Error),
}

impl Display for Error {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(f, "{self:?}")
	}
}

impl From<io::Error> for Error {
	fn from(e: io::Error) -> Self {
		Error::Io(e)
	}
}

impl From<str::Utf8Error> for Error {
	fn from(e: str::Utf8Error) -> Self {
		Error::Conversion(e)
	}
}

#[cfg(feature = "panic_console")]
pub mod panic {
	use super::*;

	pub struct Console;

	impl Console {
		pub fn new() -> Self {
			Console {}
		}

		pub fn _wfmt(
			&mut self,
			args: core::fmt::Arguments<'_>,
		) -> core::fmt::Result {
			self.write_fmt(args)
		}
	}

	impl Default for Console {
		fn default() -> Self {
			Self::new()
		}
	}

	impl Write for Console {
		fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
			if (!buf.is_empty()) {
				self.write_all(buf).map_err(|_| {
					io::Error::new(
						io::ErrorKind::Other,
						"impossible write error",
					)
				})?;
			}
			Ok(buf.len())
		}

		fn flush(&mut self) -> io::Result<()> {
			with_panic(|c| c.bflush()).map_err(|_| {
				io::Error::new(
					io::ErrorKind::Other,
					"impossible flush error",
				)
			})
		}

		fn write_all(&mut self, b: &[u8]) -> io::Result<()> {
			with_panic(|c| c.bwrite_all(b)).map_err(|_| {
				io::Error::new(
					io::ErrorKind::Other,
					"impossible write error",
				)
			})
		}
	}

	impl FmtWrite for Console {
		fn write_str(&mut self, s: &str) -> fmt::Result {
			let mut lines = s.lines().peekable();
			while let Some(l) = lines.next() {
				self.write_all(l.as_bytes())
					.map_err(|_| fmt::Error)?;
				if (lines.peek().is_some()) {
					self.write_all(b"\r\n")
						.map_err(|_| fmt::Error)?;
				}
			}
			if (s.ends_with('\n')) {
				self.write_all(b"\r\n")
					.map_err(|_| fmt::Error)?;
			}

			Ok(())
		}
	}
}

pub struct Console;

impl Console {
	pub fn new() -> Self {
		Console {}
	}
}

impl Default for Console {
	fn default() -> Self {
		Self::new()
	}
}

impl Console {
	fn read_infallible(&mut self) -> u8 {
		block!(self.read_valid()).unwrap()
	}

	fn read_valid(&mut self) -> nb::Result<u8, Void> {
		with(|c| loop {
			let r = c.read();
			match (r) {
				Ok(b) => return (Ok(b)),
				Err(nb::Error::WouldBlock) => {
					return (Err(nb::Error::WouldBlock))
				}
				Err(nb::Error::Other(_)) => continue,
			}
		})
	}

	pub fn read_exact_checked(
		&mut self,
		buf: &mut [u8],
	) -> Result<(), RxError> {
		with(|c| c.read_exact(buf))
	}

	pub fn _wfmt(
		&mut self,
		args: core::fmt::Arguments<'_>,
	) -> core::fmt::Result {
		self.write_fmt(args)
	}

	fn write_char(&mut self, c: char) -> Result<(), Error> {
		let mut buf: [u8; 4] = [0; 4];
		let s = c.encode_utf8(&mut buf[..]);
		self.write_all(s.as_bytes())?;
		Ok(())
	}

	fn read_char(&mut self) -> Result<Option<char>, Error> {
		let b = self.read_infallible();
		self.finish_reading_char(b)
	}

	fn finish_reading_char(
		&mut self,
		first: u8,
	) -> Result<Option<char>, Error> {
		let mut char_buf: Vec<u8, 4> = Vec::new();
		char_buf.push(first).unwrap();

		loop {
			if let Ok(s) = str::from_utf8(char_buf.as_slice()) {
				return Ok(Some(s.chars().next().unwrap()));
			}

			let b = self.read_infallible();
			if let Err(_b) = char_buf.push(b) {
				// We have seen 4 bytes and still don't have
				// a valid UTF-8 character.  We need to resync
				// so that special characters can be recognised.
				// Output, but discard the invalid bytes; they
				// are likely just line noise and there is
				// no possibility they will help the user.
				self.write_all(char_buf.as_slice())?;
				return Ok(None);
			}
		}
	}

	fn animate_prompt(&mut self, doodles: &[u8]) -> Result<u8, Error> {
		loop {
			for d in doodles {
				self.write_all(&[*d, b' '])?;
				if let Some(b) =
					delay::delay_unless(1u64 << 30, || {
						self.read_valid().ok()
					}) {
					return (Ok(b));
				}
				self.write_all(b"\x08\x08")?;
			}
		}
	}

	pub fn prompt_and_read_line(
		&mut self,
		buf: &mut String<CON_BUF_SIZE>,
	) -> Result<(), Error> {
		let animate = cfg!(feature = "prompt_animation");
		let mut need_prompt = true;

		buf.clear();

		loop {
			let c = match (need_prompt, animate) {
				(true, true) => {
					let b = self.animate_prompt(b".oOo")?;
					need_prompt = false;
					self.write(b"\x08\x08> ")?;

					self.finish_reading_char(b)?
				}
				(true, false) => {
					need_prompt = false;
					self.write(b"> ")?;

					self.read_char()?
				}
				(false, _) => self.read_char()?,
			};

			match c {
				None => continue,
				// Ignore LF; most terminals, including DEC VTs
				// that are commonly emulated, send only CR when
				// the Enter or Return key is pressed.  If the
				// terminal sends LF as well, we ignore it.
				// While this means we won't work with terminals
				// that send only LF, that behaviour seems much
				// less common than sending only CR.  We
				// previously treated CR and LF the same: either
				// could end a command and each would be echoed;
				// while this "works" more universally, it also
				// requires unusual terminal mappings to avoid
				// odd echo sequences and spurious empty
				// commands.  We are left with the choice of
				// echoing the LF or swallowing it silently; we
				// choose the latter as while any terminal that
				// sends only LF probably expects us to echo it,
				// doing so won't result in anything useful
				// happening.
				Some('\n') => {}
				Some('\r') => {
					self.write_all(b"\r\n")?;
					break;
				}
				// Similarly, we treat BS and DEL the same way,
				// as BS.  This is a bit more dubious in that
				// the two often have different semantics.
				// However, we don't support the kind of line
				// editing that DEL normally drives, and this
				// allows us to accommodate terminals that send
				// either BS or DEL when the <|x| key is
				// pressed, again without requiring any extra
				// mappings to be set up.  When we receive
				// either character, we:
				// - remove the BS/DEL itself from buf
				// - figure out how many bytes were in the last
				//   valid character and remove those too
				// - output BS, SPC, BS
				// - decrement chcount
				Some('\x7f') | Some('\x08') => {
					if let Some(c) = buf.pop() {
						for _ in 0 .. c.len_utf8() {
							const BS: u8 = 0x08;
							self.write_all(&[
								BS, b' ', BS,
							])?;
						}
					}
				}
				Some(c) => {
					if buf.push(c).is_err() {
						break;
					}
					self.write_char(c)?;
				}
			};
		}
		Ok(())
	}
}

/*
 * These are the xmodem::io traits, and they therefore represent the "raw"
 * interface.  No modification or translation of any kind is performed on the
 * bytes we handle here.  In the ancient era (when Xmodem was relevant), this
 * would have been called a "binary-mode" transfer, as distinguished from
 * "ascii-mode" which may perform translations.  The more modern terminal
 * parlance for the latter is "cooked", which we provide to code that intends to
 * interact with humans via our FmtWrite implementation below and
 * prompt_and_read_line() above.  This is as far as we want to go to cope with
 * remote terminals; users who want full raw and cooked mode terminal support
 * are encouraged to use the provided lcmds to boot Unix, which provides them.
 */
impl Read for Console {
	fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
		self.read_exact(buf)?;
		Ok(buf.len())
	}

	fn read_exact(&mut self, buf: &mut [u8]) -> io::Result<()> {
		with(|c| c.read_exact_unchecked(buf));
		Ok(())
	}
}

impl Write for Console {
	fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
		if (!buf.is_empty()) {
			self.write_all(buf).map_err(|_| {
				io::Error::new(
					io::ErrorKind::Other,
					"impossible write error",
				)
			})?;
		}
		Ok(buf.len())
	}

	fn flush(&mut self) -> io::Result<()> {
		with(|c| c.bflush()).map_err(|_| {
			io::Error::new(
				io::ErrorKind::Other,
				"impossible flush error",
			)
		})
	}

	fn write_all(&mut self, b: &[u8]) -> io::Result<()> {
		with(|c| c.bwrite_all(b)).map_err(|_| {
			io::Error::new(
				io::ErrorKind::Other,
				"impossible write error",
			)
		})
	}
}

impl FmtWrite for Console {
	fn write_str(&mut self, s: &str) -> fmt::Result {
		let mut lines = s.lines().peekable();
		while let Some(l) = lines.next() {
			self.write_all(l.as_bytes()).map_err(|_| fmt::Error)?;
			if (lines.peek().is_some()) {
				self.write_all(b"\r\n")
					.map_err(|_| fmt::Error)?;
			}
		}
		if (s.ends_with('\n')) {
			self.write_all(b"\r\n").map_err(|_| fmt::Error)?;
		}

		Ok(())
	}
}

pub fn stdin() -> Console {
	Console::new()
}

pub fn stdout() -> Console {
	Console::new()
}

pub fn stderr() -> Console {
	Console::new()
}

pub fn console() -> Console {
	Console::new()
}

#[macro_export]
macro_rules! print {
	($($arg:tt)*) =>
	    ($crate::console::stdout()._wfmt(
			core::format_args!($($arg)*)).unwrap());
}

#[macro_export]
macro_rules! println {
	() => ($crate::print!("\n"));
	($($arg:tt)*) => ({
		$crate::console::stdout()._wfmt(
			core::format_args!($($arg)*)).unwrap();
		$crate::print!("\n");
	})
}

#[macro_export]
macro_rules! eprint {
	($($arg:tt)*) =>
	    ($crate::console::stderr()._wfmt(
			core::format_args!($($arg)*)).unwrap());
}

#[macro_export]
macro_rules! eprintln {
	() => ($crate::eprint!("\n"));
	($($arg:tt)*) => ({
		$crate::console::stderr()._wfmt(
			core::format_args!($($arg)*)).unwrap();
		$crate::eprint!("\n");
	})
}

#[cfg(feature = "panic_console")]
#[macro_export]
macro_rules! pprint {
	($($arg:tt)*) =>
		($crate::console::panic::Console::new()._wfmt(
			core::format_args!($($arg)*)).unwrap());
}

#[cfg(feature = "panic_console")]
#[macro_export]
macro_rules! pprintln {
	() => ($crate::pprint!("\n"));
	($($arg:tt)*) => ({
		$crate::console::panic::Console::new()._wfmt(
			core::format_args!($($arg)*)).unwrap();
		$crate::pprint!("\n");
	})
}
