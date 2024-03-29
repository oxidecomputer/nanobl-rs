/*
 * Copyright 2024 Oxide Computer Company
 */

use static_assertions::*;
use tock_registers::registers::ReadWrite;
use tock_registers::{register_bitfields, register_structs};

register_bitfields! {
	u8,
	pub IOMUX_GPIO [
		IOMUX_GPIO_X OFFSET(0) NUMBITS(2) [],
	]
}

register_bitfields! {
	u32,
	pub GPIO [
		DEBOUNCETMROUT OFFSET(0) NUMBITS(4) [],
		DEBOUNCETMROUTUNIT OFFSET(4) NUMBITS(1) [],
		DEBOUNCECNTRL OFFSET(5) NUMBITS(2) [
			NoDebounce = 0b00,
			PreserveLow = 0b01,
			PreserveHigh = 0b10,
			RemoveBoth = 0b11
		],
		DEBOUNCETMRLARGE OFFSET(7) NUMBITS(1) [],
		LEVELTRIG OFFSET(8) NUMBITS(1) [
			Edge = 0,
			Level = 1
		],
		ACTIVELEVEL OFFSET(9) NUMBITS(2) [
			High = 0b00,
			Low = 0b01,
			Both = 0b10
		],
		INTR_ENABLE_ST OFFSET(11) NUMBITS(1) [],
		INTR_ENABLE_DL OFFSET(12) NUMBITS(1) [],
		WAKECNTRL_S0A3 OFFSET(13) NUMBITS(1) [],
		WAKECNTRL_S3 OFFSET(14) NUMBITS(1) [],
		WAKECNTRL_S5 OFFSET(15) NUMBITS(1) [],
		PINSTS OFFSET(16) NUMBITS(1) [],
		DRVSTRENGTHSEL OFFSET(17) NUMBITS(2) [
			Ohms60_1V8 = 0b01,
			Ohms40 = 0b10,
			Ohms80 = 0b11
		],
		PULLUPSEL OFFSET(19) NUMBITS(1) [
			Ohms4K = 0,
			Ohms8K = 1
		],
		PULLUPENABLE OFFSET(20) NUMBITS(1) [],
		PULLDOWNENABLE OFFSET(21) NUMBITS(1) [],
		OUTPUTVALUE OFFSET(22) NUMBITS(1) [],
		OUTPUTENABLE OFFSET(23) NUMBITS(1) [],
		SWCNTRLIN OFFSET(24) NUMBITS(1) [],
		SWCNTRLEN OFFSET(25) NUMBITS(1) [],
		INTRSTS OFFSET(28) NUMBITS(1) [],
		WAKESTS OFFSET(29) NUMBITS(1) []
	]
}

register_structs! {
	pub IomuxRegister {
		(0x00 => pub IOMUX_GPIO: ReadWrite<u8, IOMUX_GPIO::Register>),
		(0x01 => @END),
	},
	pub GpioRegister {
		(0x00 => pub GPIO: ReadWrite<u32, GPIO::Register>),
		(0x04 => @END),
	}
}

assert_eq_size!(IomuxRegister, [u8; 0x1]);
assert_eq_size!(GpioRegister, [u8; 0x4]);
