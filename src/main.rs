/*
 * Copyright 2025 Oxide Computer Company
 */

#![no_std]
#![no_main]
#![feature(str_split_whitespace_remainder)]

use core::arch::global_asm;

global_asm!(include_str!("start.s"), options(att_syntax));

mod cmd;
mod gpio;
mod pagetable_impl;

mod loader {
	use super::cmd::exports::*;
	use nanobl_util::{console, idt, panic, prelude::*};

	#[derive(Debug)]
	struct CmdError;

	fn handle_cmd_str(
		cmdstr: &str,
		pv: LcmdValue,
	) -> Result<LcmdValue, CmdError> {
		match (LcmdInstance::new(cmdstr)) {
			Ok(mut cmdargs) => {
				cmdargs.pipe(pv);
				#[cfg(feature = "echo")]
				println!("< {:x?}", cmdargs);
				let r = cmdargs.exec();

				r.map_err(|e| {
					println!(
						"< [command error]: \
						 {:x?}",
						e
					);

					CmdError
				})
			}
			Err(e) => {
				println!("< '{}': error: {:x?}", cmdstr, e);
				Err(CmdError)
			}
		}
	}

	fn main() -> ! {
		idt::init();
		super::gpio::init();

		println!();
		println!();
		print!("Nano-Bootloader {} ", env!("CARGO_PKG_VERSION"));
		println_features();
		println!("Copyright 2025 Oxide Computer Company");
		println!("All Rights Reserved.");
		println!();

		// Also output banner to the panic console, if feature enabled.
		#[cfg(feature = "panic_console")]
		pprintln!(
			"Nano-Bootloader {} Panic Console",
			env!("CARGO_PKG_VERSION"),
		);

		let mut condev = console::console();
		let mut line = heapless::String::<128>::new();

		loop {
			let s = match condev.prompt_and_read_line(&mut line) {
				Ok(()) => {
					let s = line.as_str().trim();
					if s.is_empty() {
						continue;
					}
					s
				}
				Err(e) => {
					println!("< [input error]: {:x?}", e);
					continue;
				}
			};

			let pipeiter = s.split('|');
			let mut v = LcmdValue::NONE;
			for s in pipeiter {
				let r = handle_cmd_str(s.trim(), v);
				if (r.is_err()) {
					v = LcmdValue::NONE;
					break;
				}
				v = r.unwrap();
			}
			println!("{}", v);
		}
	}

	fn println_features() {
		fn print(prefix: &str, feature: &str) -> &'static str {
			print!("{prefix}");
			print!("{feature}");
			","
		}
		print!("[");
		let mut prefix = "";
		if cfg!(feature = "panic_console") {
			prefix = print(prefix, "panic_console");
		}
		if cfg!(feature = "auto_reboot") {
			prefix = print(prefix, "auto_reboot");
		}
		if cfg!(feature = "prompt_animation") {
			prefix = print(prefix, "prompt_animation");
		}
		if cfg!(feature = "echo") {
			prefix = print(prefix, "echo");
		}
		if cfg!(feature = "x86_paddr") {
			prefix = print(prefix, "x86_paddr");
		}
		if cfg!(feature = "x86_vaddr") {
			print(prefix, "x86_vaddr");
		}
		println!("]");
	}

	#[no_mangle]
	pub extern "system" fn _start(bist_result: u32) -> ! {
		panic::postcode(0xCC57);

		if (bist_result != 0) {
			panic!("{code:04x},{v:04x} BIST failed with result {v}",
				code = 0xB157,
				v = bist_result
			);
		}

		main();
	}
}
