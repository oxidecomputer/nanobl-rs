/*
 * XXX Work around cargo#4423 host/target flags gap
 */
#![allow(unused_parens)]

use std::env;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;

struct Defs {
	console_baud_rate: u32,
	console_dev_name: &'static str,
	console_uart_clock: &'static str,
	console_buf_size: usize,

	panic_console_baud_rate: u32,
	panic_console_dev_name: &'static str,
	panic_console_uart_clock: &'static str,
}

static DEFS: Defs = Defs {
	console_baud_rate: 3_000_000,
	console_dev_name: "UART0",
	console_uart_clock: "APB",
	console_buf_size: 128,

	panic_console_baud_rate: 115_200,
	panic_console_dev_name: "UART1",
	panic_console_uart_clock: "APB",
};

fn main() {
	let out = &PathBuf::from(env::var_os("OUT_DIR").unwrap());
	let mut con_file = File::create(out.join("condefs.rs")).unwrap();

	writeln!(
		con_file,
		"const CON_BAUD_RATE: u32 = {};",
		env::var("NANOBLRS_CONSOLE_BAUD_RATE")
			.unwrap_or_else(|_| DEFS.console_baud_rate.to_string())
			.parse::<u32>()
			.unwrap_or(DEFS.console_baud_rate)
	)
	.unwrap();
	let dev = env::var("NANOBLRS_CONSOLE_DEV_NAME")
		.unwrap_or_else(|_| DEFS.console_dev_name.to_string());
	writeln!(
		con_file,
		"console!({dev}, CON_BAUD_RATE, {clock});",
		clock = env::var("NANOBLRS_CONSOLE_UART_CLOCK").unwrap_or_else(
			|_| DEFS.console_uart_clock.to_string()
		)
	)
	.unwrap();
	writeln!(
		con_file,
		"const CON_BUF_SIZE: usize = {};",
		env::var("NANOBLRS_CONSOLE_BUFFER_SIZE")
			.unwrap_or_else(|_| DEFS.console_buf_size.to_string())
			.parse::<usize>()
			.unwrap_or(DEFS.console_buf_size)
	)
	.unwrap();

	if env::var("CARGO_FEATURE_PANIC_CONSOLE").is_ok() {
		let pdev = env::var("NANOBLRS_PANIC_CONSOLE_DEV_NAME")
			.unwrap_or_else(|_| {
				DEFS.panic_console_dev_name.to_string()
			});
		assert_ne!(
			dev, pdev,
			"Panic console must be different from regular console",
		);
		writeln!(
			con_file,
			"const PANIC_CON_BAUD_RATE: u32 = {};",
			env::var("NANOBLRS_PANIC_CONSOLE_BAUD_RATE")
				.unwrap_or_else(|_| DEFS
					.panic_console_baud_rate
					.to_string())
				.parse::<u32>()
				.unwrap_or(DEFS.panic_console_baud_rate)
		)
		.unwrap();

		writeln!(
			con_file,
			"console!(PANIC @ {pdev}, PANIC_CON_BAUD_RATE, {});",
			env::var("NANOBLRS_PANIC_CONSOLE_UART_CLOCK")
				.unwrap_or_else(|_| DEFS
					.panic_console_uart_clock
					.to_string())
		)
		.unwrap();
	}

	let ldflags =
		["-zseparate-code", "-zmax-page-size=0x1000", "-T", "bl.gld"];

	println!("cargo:rerun-if-changed=bl.gld");
	println!("cargo:rerun-if-env-changed=NANOBLRS_CONSOLE_BAUD_RATE");
	println!("cargo:rerun-if-env-changed=NANOBLRS_CONSOLE_DEV_NAME");
	println!("cargo:rerun-if-env-changed=NANOBLRS_CONSOLE_UART_CLOCK");
	println!("cargo:rerun-if-env-changed=NANOBLRS_CONSOLE_BUFFER_SIZE");
	println!("cargo:rerun-if-env-changed=NANOBLRS_PANIC_CONSOLE_BAUD_RATE");
	println!("cargo:rerun-if-env-changed=NANOBLRS_PANIC_CONSOLE_DEV_NAME");
	println!(
		"cargo:rerun-if-env-changed=NANOBLRS_PANIC_CONSOLE_UART_CLOCK"
	);

	for f in ldflags.iter() {
		println!("cargo:rustc-link-arg-bins={f}");
	}
}
