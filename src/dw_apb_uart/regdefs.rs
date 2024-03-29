/*
 * Copyright 2024 Oxide Computer Company
 */

use static_assertions::*;
use tock_registers::registers::{ReadOnly, ReadWrite, WriteOnly};
use tock_registers::{register_bitfields, register_structs};

register_bitfields! {
	u32,
	pub IER [
		ERBFI 0,
		ETBEI 1,
		ELSI 2,
		EDSSI 3,
		PTIME 7
	],

	pub FCR [
		FIFOE OFFSET(0) NUMBITS(1) [],
		RFIFOR OFFSET(1) NUMBITS(1) [],
		XFIFOR OFFSET(2) NUMBITS(1) [],
		DMAM OFFSET(3) NUMBITS(1) [],
		TET OFFSET(4) NUMBITS(2) [
			Empty = 0b00,
			Fill2 = 0b01,
			FillQuarter = 0b10,
			FillHalf = 0b11
		],
		RT OFFSET(6) NUMBITS(2) [
			Fill1 = 0b00,
			FillQuarter = 0b01,
			FillHalf = 0b10,
			FillLess2 = 0b11
		]
	],

	pub IIR [
		IID OFFSET(0) NUMBITS(4) [
			ModemStatus = 0,
			NoIntr = 1,
			THREmpty = 2,
			RxData = 4,
			RxLineStatus = 6,
			Busy = 7,
			CharTimeout = 0xc
		],
		FIFOSE OFFSET(6) NUMBITS(2) [
			Disabled = 0,
			Enabled = 3
		]
	],

	pub LCR [
		DLS OFFSET(0) NUMBITS(2) [
			Bits5 = 0,
			Bits6 = 1,
			Bits7 = 2,
			Bits8 = 3
		],
		STOP OFFSET(2) NUMBITS(1) [
			Stop1 = 0,
			Stop15_2 = 1
		],
		PEN OFFSET(3) NUMBITS(1) [],
		EPS OFFSET(4) NUMBITS(1) [],
		SP OFFSET(5) NUMBITS(1) [],
		BREAK OFFSET(6) NUMBITS(1) [],
		DLAB OFFSET(7) NUMBITS(1) []
	],

	pub MCR [
		DTR 0,
		RTS 1,
		OUT1 2,
		OUT2 3,
		LOOPBACK 4,
		AFCE 5,
		SIRE 6
	],

	pub LSR [
		DR 0,
		OE 1,
		PE 2,
		FE 3,
		BI 4,
		THRE 5,
		TEMT 6,
		RFE 7
	],

	pub MSR [
		DCTS 0,
		DDSR 1,
		TERI 2,
		DDCD 3,
		CTS 4,
		DSR 5,
		RI 6,
		DCD 7
	],

	pub USR [
		BUSY 0,
		TFNF 1,
		TFE 2,
		RFNE 3,
		RFF 4
	],

	pub TFL [
		TFL OFFSET(0) NUMBITS(5)
	],

	pub RFL [
		RFL OFFSET(0) NUMBITS(5)
	],

	pub SRR [
		UR 0,
		RFR 1,
		XFR 2
	],

	pub SDMAM [
		SDMAM 0
	],

	pub CPR [
		APB_DATA_WIDTH OFFSET(0) NUMBITS(2) [
			BITS8 = 0,
			BITS16 = 1,
			BITS32 = 2
		],
		AFCE_MODE OFFSET(4) NUMBITS(1) [],
		THRE_MODE OFFSET(5) NUMBITS(1) [],
		SIR_MODE OFFSET(6) NUMBITS(1) [],
		SIR_LP_MODE OFFSET(7) NUMBITS(1) [],
		ADDITIONAL_FEAT OFFSET(8) NUMBITS(1) [],
		FIFO_ACCESS OFFSET(9) NUMBITS(1) [],
		FIFO_STAT OFFSET(10) NUMBITS(1) [],
		SHADOW OFFSET(11) NUMBITS(1) [],
		UART_ADD_ENCODED_PARAMS OFFSET(12) NUMBITS(1) [],
		DMA_EXTRA OFFSET(13) NUMBITS(1) [],
		FIFO_MODE OFFSET(16) NUMBITS(8) [
			BYTES0 = 0,
			BYTES16 = 1,
			BYTES32 = 2,
			BYTES64 = 4,
			BYTES128 = 8,
			BYTES256 = 0x10,
			BYTES512 = 0x20,
			BYTES1K = 0x40,
			BYTES2K = 0x80
		]
	]
}

/*
 * XXX All but the first three registers are common, and should not be copied
 * and pasted here.  Putting them into three separate structs doesn't quite
 * work, because although we can then put them all in a union, that provides
 * us with no way to give callers access to the contained structs even if
 * they have the correct register name.  This problem is solved by this
 * common but fairly hazardous pattern in C:
 *
 * struct a {
 * 	int a_x;
 * 	...
 * };
 *
 * struct b {
 * 	void *b_y;
 * 	...
 * };
 *
 * union u {
 * 	struct u_a;
 * 	struct u_b;
 * };
 *
 * #define	u_x	u_a.a_x
 * #define	u_y	u_b.b_y
 * ...
 *
 * It looks like dtolnay/paste may help here.
 */
register_structs! {
	pub UnknownRegisterBlock {
		(0x00 => _unavail),
		(0x0C => pub LCR: ReadWrite<u32, LCR::Register>),
		(0x10 => pub MCR: ReadWrite<u32, MCR::Register>),
		(0x14 => pub LSR: ReadOnly<u32, LSR::Register>),
		(0x18 => pub MSR: ReadOnly<u32, MSR::Register>),
		(0x1C => pub SCR: ReadWrite<u32>),
		(0x20 => _reserved),
		(0x30 => pub SXXRN: [ReadWrite<u32>; 16]),
		(0x70 => _far),
		(0x74 => _tfr),
		(0x78 => _rfw),
		(0x7C => pub USR: ReadOnly<u32, USR::Register>),
		(0x80 => pub TFL: ReadOnly<u32, TFL::Register>),
		(0x84 => pub RFL: ReadOnly<u32, RFL::Register>),
		(0x88 => pub SRR: WriteOnly<u32, SRR::Register>),
		(0x8C => _srts),
		(0x90 => _sbcr),
		(0x94 => pub SDMAM: ReadWrite<u32, SDMAM::Register>),
		(0x98 => _sfe),
		(0x9C => _srt),
		(0xA0 => _stet),
		(0xA4 => _htx),
		(0xA8 => _dmasa),
		(0xF4 => pub CPR: ReadOnly<u32, CPR::Register>),
		(0xF8 => pub UCV: ReadOnly<u32>),
		(0xFC => pub CTR: ReadOnly<u32>),
		(0x100 => @END),
	},
	pub ConfiguringRegisterBlock {
		(0x00 => pub DLL: ReadWrite<u32>),
		(0x04 => pub DLH: ReadWrite<u32>),
		(0x08 => pub FCR: WriteOnly<u32, FCR::Register>),
		(0x0C => pub LCR: ReadWrite<u32, LCR::Register>),
		(0x10 => pub MCR: ReadWrite<u32, MCR::Register>),
		(0x14 => pub LSR: ReadOnly<u32, LSR::Register>),
		(0x18 => pub MSR: ReadOnly<u32, MSR::Register>),
		(0x1C => pub SCR: ReadWrite<u32>),
		(0x20 => _reserved),
		(0x30 => pub SXXRN: [ReadWrite<u32>; 16]),
		(0x70 => _far),
		(0x74 => _tfr),
		(0x78 => _rfw),
		(0x7C => pub USR: ReadOnly<u32, USR::Register>),
		(0x80 => pub TFL: ReadOnly<u32, TFL::Register>),
		(0x84 => pub RFL: ReadOnly<u32, RFL::Register>),
		(0x88 => pub SRR: WriteOnly<u32, SRR::Register>),
		(0x8C => _srts),
		(0x90 => _sbcr),
		(0x94 => pub SDMAM: ReadWrite<u32, SDMAM::Register>),
		(0x98 => _sfe),
		(0x9C => _srt),
		(0xA0 => _stet),
		(0xA4 => _htx),
		(0xA8 => _dmasa),
		(0xF4 => pub CPR: ReadOnly<u32, CPR::Register>),
		(0xF8 => pub UCV: ReadOnly<u32>),
		(0xFC => pub CTR: ReadOnly<u32>),
		(0x100 => @END),
	},
	pub RunningRegisterBlock {
		(0x00 => pub RBR_THR: ReadWrite<u32>),
		(0x04 => pub IER: ReadOnly<u32, IER::Register>),
		(0x08 => pub IIR: ReadOnly<u32, IIR::Register>),
		(0x0C => pub LCR: ReadWrite<u32, LCR::Register>),
		(0x10 => pub MCR: ReadWrite<u32, MCR::Register>),
		(0x14 => pub LSR: ReadOnly<u32, LSR::Register>),
		(0x18 => pub MSR: ReadOnly<u32, MSR::Register>),
		(0x1C => pub SCR: ReadWrite<u32>),
		(0x20 => _reserved),
		(0x30 => pub SXXRN: [ReadWrite<u32>; 16]),
		(0x70 => _far),
		(0x74 => _tfr),
		(0x78 => _rfw),
		(0x7C => pub USR: ReadOnly<u32, USR::Register>),
		(0x80 => pub TFL: ReadOnly<u32, TFL::Register>),
		(0x84 => pub RFL: ReadOnly<u32, RFL::Register>),
		(0x88 => pub SRR: WriteOnly<u32, SRR::Register>),
		(0x8C => _srts),
		(0x90 => _sbcr),
		(0x94 => pub SDMAM: ReadWrite<u32, SDMAM::Register>),
		(0x98 => _sfe),
		(0x9C => _srt),
		(0xA0 => _stet),
		(0xA4 => _htx),
		(0xA8 => _dmasa),
		(0xF4 => pub CPR: ReadOnly<u32, CPR::Register>),
		(0xF8 => pub UCV: ReadOnly<u32>),
		(0xFC => pub CTR: ReadOnly<u32>),
		(0x100 => @END),
	}
}

assert_eq_size!(UnknownRegisterBlock, [u8; 0x100]);
assert_eq_size!(ConfiguringRegisterBlock, [u8; 0x100]);
assert_eq_size!(RunningRegisterBlock, [u8; 0x100]);
