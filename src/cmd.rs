/*
 * Copyright 2025 Oxide Computer Company
 */

use api::LcmdDefn;
use api::LcmdHelp;
pub use api::LcmdInstance;
use api::LcmdValue;
use nanobl_util::prelude::*;

mod call;
mod dump;
mod halt;
mod inflate;
mod load;
mod map;
mod msr;
mod panic;
mod port;
mod recv;

static DEFNS: &[&LcmdDefn] = &[
	&call::CALL_DEFN,
	&dump::DUMP_DEFN,
	&halt::HALT_DEFN,
	&HELP_DEFN,
	&inflate::INFLATE_DEFN,
	&map::IS_MAPPED_DEFN,
	&LCMDS_DEFN,
	&load::LOAD_DEFN,
	&map::MAP_DEFN,
	&map::MAPPINGS_DEFN,
	&panic::PANIC_DEFN,
	&port::PORT_READ_DEFN,
	&port::PORT_WRITE_DEFN,
	&msr::RD_MSR_DEFN,
	&recv::RECV_DEFN,
	&map::UNMAP_DEFN,
	&msr::WR_MSR_DEFN,
];

pub mod exports {
	pub use super::api_impl::LcmdValue;
	pub use super::LcmdInstance;
}

mod api {
	pub use super::api_impl::parse_integer;
	pub use super::api_impl::LcmdDefn;
	pub use super::api_impl::LcmdError;
	pub use super::api_impl::LcmdHelp;
	pub use super::api_impl::LcmdInstance;
	pub use super::api_impl::LcmdValue;
}

mod api_impl {
	use crate::cmd::DEFNS;
	use crate::pagetable_impl as pi;
	use core::fmt::{self, Display};
	use nanobl_util::region_builder as rb;
	use num_traits::Num;

	#[allow(unused)]
	#[derive(Debug)]
	pub enum LcmdError {
		Parse(&'static str),
		MissingBaseArgs(rb::Error),
		MissingArgs(&'static str),
		ExtraBaseArgs(rb::Error),
		ExtraArgs(&'static str),
		BadBaseArgs(rb::Error),
		BadArgs(&'static str),
		BaseArgsRange(rb::Error),
		Range(&'static str),
		BaseArgsAlignment(rb::Error),
		UnknownCmd,
		NotImpl,
		Propagated(&'static str),
		BadObj(&'static str),
		ResourceExhaustion(&'static str),
	}

	impl From<rb::Error> for LcmdError {
		#[rustfmt::skip]
		fn from(e: rb::Error) -> Self {
		match (e) {
				rb::Error::AddressForbidden(_) |
				rb::Error::CountForbidden(_) =>
					Self::ExtraBaseArgs(e),

				rb::Error::AddressMissing(_) |
				rb::Error::CountMissing(_) =>
					Self::MissingBaseArgs(e),

				rb::Error::AddressInForbiddenRange(_) |
				rb::Error::AddressOutOfBounds(_) |
				rb::Error::CountInForbiddenRange(_) |
				rb::Error::CountOutOfBounds(_) |
				rb::Error::CountMultipleTooBig(_) |
				rb::Error::RegionTooBig(_) |
				rb::Error::RegionInForbiddenRange(_) |
				rb::Error::RegionOutOfBounds(_) =>
					Self::BaseArgsRange(e),

				rb::Error::AddressP2Unaligned(_) |
				rb::Error::AddressUnaligned(_) |
				rb::Error::CountNotMultiple(_) =>
					Self::BaseArgsAlignment(e),

				rb::Error::AddressFailsConstraint(_) |
				rb::Error::CountFailsConstraint(_) |
				rb::Error::RegionFailsConstraint(_) =>
					Self::BadBaseArgs(e),
			}
		}
	}

	impl From<pi::Error> for LcmdError {
		fn from(e: pi::Error) -> Self {
			match (e) {
				pi::Error::NoMemory =>
					Self::ResourceExhaustion(
			"out of pagetable space; too many mappings"),
				pi::Error::LargerMappingExists =>
					Self::Propagated(
			"cannot split existing larger mapping"),
			}
		}
	}

	#[derive(Debug)]
	pub struct LcmdValue {
		pub addr: Option<u64>,
		pub count: Option<u64>,
	}

	impl LcmdValue {
		pub const NONE: Self = Self {
			addr: None,
			count: None,
		};
	}

	impl Display for LcmdValue {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			if (self.addr.is_none()) {
				return (Ok(()));
			}
			write!(f, "{:x}", self.addr.unwrap())?;
			if (self.count.is_some()) {
				write!(f, ",{:x}", self.count.unwrap())?;
			}

			Ok(())
		}
	}

	pub struct LcmdInstance<'a> {
		defn: &'static LcmdDefn,
		input: LcmdValue,
		args: &'a str,
	}

	impl<'a> LcmdInstance<'a> {
		pub fn new(s: &'a str) -> Result<Self, LcmdError> {
			/*
			 * [addr[,count]]::cmd [arg[ arg...]]
			 */
			let s = s.trim();

			/* Empty string */
			if (s.is_empty()) {
				return (Err(LcmdError::Parse(
					"empty command string; try ::help",
				)));
			}

			let mut iter = s.split("::").fuse();
			let oaddrcountstr = iter.next();
			let ocmdargstr = iter.next();

			/* [...]::[...]::[...] is invalid */
			if iter.next().is_some() {
				return (Err(LcmdError::Parse(
					"multiple commands not allowed; \
					try ::help",
				)));
			}

			/* [...]:: is invalid */
			if (ocmdargstr.is_none() ||
				ocmdargstr.unwrap().is_empty())
			{
				return (Err(LcmdError::Parse(
					"no command name found; try ::help",
				)));
			}

			let mut iter = ocmdargstr.unwrap().split_whitespace();
			let defn;

			let ocmdstr = iter.next();
			if let Some(cmdstr) = ocmdstr {
				defn = DEFNS
					.iter()
					.find(|&d| (cmdstr == d.name()));

				/* [...]::bad_command[...] */
				if (defn.is_none()) {
					return (Err(LcmdError::Parse(
						"unknown command; try ::lcmds",
					)));
				}
			} else {
				/* [...]:: edge case?  Invalid if possible */
				return (Err(LcmdError::Parse(
					"no command name found; try ::help",
				)));
			}

			let defn = defn.unwrap();

			/* ::command[...] is good */
			if (oaddrcountstr.is_none()) {
				return (Ok(Self {
					defn,
					input: LcmdValue {
						addr: None,
						count: None,
					},
					args: iter.remainder().unwrap_or(""),
				}));
			}
			let addrcountstr = oaddrcountstr.unwrap();
			if (addrcountstr.is_empty()) {
				return (Ok(Self {
					defn,
					input: LcmdValue {
						addr: None,
						count: None,
					},
					args: iter.remainder().unwrap_or(""),
				}));
			}

			let mut asiter = addrcountstr.split(',').fuse();
			let oaddrstr = asiter.next();
			let ocountstr = asiter.next();

			/* [...],[...],[...]::command[...] is invalid */
			if asiter.next().is_some() {
				return (Err(LcmdError::Parse(
					"extra count arguments found; \
					try ::help",
				)));
			}

			let oaddr = if (oaddrstr.is_none() ||
				oaddrstr.unwrap().is_empty())
			{
				None
			} else {
				/* addr is not an unsigned integer on error */
				Some(parse_integer::<u64>(oaddrstr.unwrap())?)
			};

			let ocount = if (ocountstr.is_none() ||
				ocountstr.unwrap().is_empty())
			{
				None
			} else {
				/* count is not an unsigned integer on error */
				Some(parse_integer::<u64>(ocountstr.unwrap())?)
			};

			Ok(Self {
				defn,
				input: LcmdValue {
					addr: oaddr,
					count: ocount,
				},
				args: iter.remainder().unwrap_or(""),
			})
		}

		pub fn addr(&self) -> Option<u64> {
			self.input.addr
		}

		pub fn count(&self) -> Option<u64> {
			self.input.count
		}

		pub fn pipe(&mut self, input: LcmdValue) {
			if (input.addr.is_some()) {
				self.input.addr = input.addr;
				if (input.count.is_some()) {
					self.input.count = input.count;
				}
			}
		}

		pub fn args(&self) -> &str {
			self.args
		}

		pub fn exec(self) -> Result<LcmdValue, LcmdError> {
			(self.defn.exec)(self)
		}
	}

	/*
	 * In principle this should be derivable, but it isn't because LcmdDefn
	 * can't derive Debug, because something something not general enough
	 * something something insane lifetime syntax on functions.
	 */
	impl core::fmt::Debug for LcmdInstance<'_> {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			f.debug_struct("LcmdInstance")
				.field("cmd", &self.defn.name)
				.field("input", &self.input)
				.field("args", &self.args)
				.finish()
		}
	}
	pub struct LcmdHelp {
		pub defn: &'static LcmdDefn,
		pub short: &'static str,
		pub synopsis: Option<&'static str>,
		pub desc: Option<&'static str>,
	}

	impl LcmdHelp {
		pub(super) const fn new(
			defn: &'static LcmdDefn,
			short: &'static str,
		) -> Self {
			LcmdHelp {
				defn,
				short,
				synopsis: None,
				desc: None,
			}
		}
	}

	impl Display for LcmdHelp {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			writeln!(f)?;
			writeln!(f, "NAME")?;
			writeln!(f, "  {} - {}", self.defn.name, self.short)?;
			writeln!(f)?;
			writeln!(f, "SYNOPSIS")?;
			if let Some(synopsis) = self.synopsis {
				writeln!(f, "  {synopsis}")?;
			} else {
				writeln!(f, "  ::{}", self.defn.name)?;
			}
			writeln!(f)?;
			if let Some(desc) = self.desc {
				writeln!(f, "DESCRIPTION")?;
				for s in desc.split('\n') {
					writeln!(f, "  {s}")?;
				}
				writeln!(f)?;
			}

			Ok(())
		}
	}

	pub struct LcmdDefn {
		pub name: &'static str,
		pub help: LcmdHelp,
		pub exec: fn(LcmdInstance<'_>) -> Result<LcmdValue, LcmdError>,
	}

	impl LcmdDefn {
		pub(super) fn name(&self) -> &'static str {
			self.name
		}

		pub(super) fn help(&self) -> &LcmdHelp {
			&self.help
		}
	}

	impl Display for LcmdDefn {
		fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
			write!(f, "{:<16} - {}", self.name, self.help.short)
		}
	}

	/*
	 * It'd be nice to use something like the parse_int crate here, but we
	 * can't because (a) such crates aren't no_std; they use either
	 * collect() on a string iterator or str::replace() to remove _ chars,
	 * a really nice feature but we can't do it that way, and (b) they
	 * invariably default to base 10 and we really prefer 16 both to match
	 * mdb(1) and because it's just that kind of tool.
	 */
	pub fn parse_integer<T>(s: &str) -> Result<T, LcmdError>
	where
		T: Num + From<u8>,
	{
		/*
		 * We accept all the standard prefixes Rust uses.  We also
		 * accept the legacy prefix of '0' to mean octal.  This is not
		 * intended to parse anything; we do that below, so there are
		 * cases here that end up being treated as hex even though we
		 * could rule them out as valid if we wanted to; they'll fail
		 * later anyway.
		 *
		 * The default is hex.
		 *
		 * We special-case 0 because (a) it's cheap and common and (b)
		 * doing so avoids tedious conflicts with radix prefixes.
		 */
		let s = s.trim();

		if (s == "0") {
			return (Ok(0.into()));
		}

		let (radix, skip) = match s.get(0 .. 2) {
			Some("0x") | Some("0X") => (16, 2),
			Some("0t") | Some("0T") => (10, 2),
			Some("0o") | Some("0O") => (8, 2),
			Some("0b") | Some("0i") | Some("0I") => (2, 2),
			_ => match s.get(0 .. 1) {
				Some("0") => (8, 1),
				_ => (16, 0),
			},
		};

		<T>::from_str_radix(&s[skip ..], radix).map_err(|_e| {
			LcmdError::Parse("invalid integer; try ::help")
		})
	}
}

static LCMDS_DEFN: LcmdDefn = LcmdDefn {
	name: "lcmds",
	help: LcmdHelp::new(&LCMDS_DEFN, "list loader commands"),
	exec: |_| {
		for defn in DEFNS {
			println!("{}", defn);
		}

		Ok(LcmdValue::NONE)
	},
};

static HELP_DEFN: LcmdDefn = LcmdDefn {
	name: "help",
	help: LcmdHelp {
		synopsis: Some("::help [lcmd]"),
		..LcmdHelp::new(&HELP_DEFN, "display general or command help")
	},
	exec: |inst| {
		let mut iter = inst.args().split_whitespace().fuse().take(1);
		let ocmd = iter.next();

		if let Some(cmd) = ocmd {
			if let Some(defn) =
				DEFNS.iter().find(|&d| (cmd == d.name()))
			{
				print!("{}", defn.help());
				return (Ok(LcmdValue::NONE));
			}
			println!("no such command {}; try ::lcmds", cmd);
			return (Ok(LcmdValue::NONE));
		}

		#[rustfmt::skip]
		print!(concat!("\n",
	"Loader commands (lcmds) are structured similarly to those in the\n",
	"illumos mdb(1) debugger, which has documentation that will not be\n",
	"copied here; generally, however, command syntax is as follows:\n\n",
	"        [addr[,count]]::verb [arguments...]\n\n",
	"The validity and interpretation of <addr>, <count>, and <arguments>\n",
	"vary for each <verb>, which names an lcmd.\n\n",
	"<addr>, <count>, and all other integer arguments to all lcmds are\n",
	"interpreted in a manner similar to that in mdb, plus some overlap\n",
	"with Rust, according to radix-specifying prefixes:\n\n",
	"PREFIX        DECIMAL BASE\n",
	"0x|0X|(none)  16 [hex]\n",
	"0t|0T         10 [dec]\n",
	"0|0o|0O       8  [oct]\n",
	"0b|0i|0I      2  [bin]\n\n",
	"Note that all OUTPUT is always expressed in base 16; where OUTPUT\n",
	"values contain multiple bytes, they are in native-endian format\n",
	"unless otherwise specified according to the lcmd you are using.\n",
	"The helpful Rust convention of using _ to group digits is not\n",
	"supported in input nor produced in output because doing so in a\n",
	"straightforward manner requires heap allocation.\n\n",
	"The output of one command may be piped into another by separating\n",
	"them with the '|' character, and such may be chained to any length.\n",
	"This is equivalent to executing each command in sequence, where\n",
	"each command in the chain receives address and count arguments\n",
	"from the output of the previous command; however, if that command's\n",
	"output does not include one or both of these parameters, the values\n",
	"supplied with the command will be used.  If any command results in\n",
	"an error, the remaining commands will not be executed.\n\n",
	"Safety: The bootloader provides no exception handlers; if an action\n",
	"you perform causes an exception, the processor will shut down.\n",
	"Additional specific notes on safety may be found in the help output\n",
	"for each lcmd.\n\n",
	"For additional help:\n",
	"        ::help lcmd       - detailed help for <lcmd>\n",
	"        ::lcmds           - list available lcmds\n\n"));

		Ok(LcmdValue::NONE)
	},
};
