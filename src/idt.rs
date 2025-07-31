/*
 * Portions of this file are derived from the rxv64 operating system; see
 * https://github.com/dancrossnyc/rxv64/blob/main/AUTHORS.
 *
 * Copyright 2019 The rxv64 Authors
 * Copyright 2025 Oxide Computer Company
 */

use crate::{panic, println};
use core::arch::{asm, naked_asm};
use core::ptr;
use seq_macro::seq;

#[repr(C)]
#[derive(Clone, Copy)]
struct GateDesc([u64; 2]);

impl GateDesc {
	const fn empty() -> GateDesc {
		GateDesc([!0, !0])
	}
}

fn intr64(thunk: unsafe extern "C" fn() -> !) -> GateDesc {
	const SEGMENT_PRESENT: u64 = 1 << (15 + 32);
	const TYPE_INTR_GATE: u64 = 0b1110 << (8 + 32);
	const DPL_KERN: u64 = 0b00 << (13 + 32);
	const CS_SELECTOR: u16 = 0x28;
	const RSP0: u8 = 0;

	// The seemingly superfluous cast to usize and then
	// to u64 is to silence a clippy warning.
	let offset = thunk as usize as u64;
	let lower0_offset = offset & 0x0000_FFFF;
	let lower0 = (u64::from(CS_SELECTOR) << 16) | lower0_offset;
	let lower1_offset = (offset & 0xFFFF_0000) << 32;
	let lower1 = (u64::from(RSP0) << 32) | lower1_offset;
	let segment_bits = SEGMENT_PRESENT | TYPE_INTR_GATE | DPL_KERN;
	let lower = lower1 | lower0 | segment_bits;
	let upper = offset >> 32;
	GateDesc([lower, upper])
}

unsafe fn lidt(idt: &'static Idt) {
	const LIMIT: u16 = core::mem::size_of::<Idt>() as u16 - 1;
	asm!(r#"
		subq $16, %rsp;
		movq {}, 8(%rsp);
		movw ${}, 6(%rsp);
		lidt 6(%rsp);
		addq $16, %rsp;
		"#, in(reg) idt, const LIMIT, options(att_syntax));
}

#[derive(Copy, Clone, Debug)]
#[repr(C)]
struct TrapFrame {
	// Pushed by software.
	rax: u64,
	rbx: u64,
	rcx: u64,
	rdx: u64,
	rsi: u64,
	rdi: u64,
	rbp: u64,
	r8: u64,
	r9: u64,
	r10: u64,
	r11: u64,
	r12: u64,
	r13: u64,
	r14: u64,
	r15: u64,

	// %ds and %es are not used in 64-bit mode, but they exist,
	// so we save and restore them.
	ds: u64, // Really these are u16s, but
	es: u64, // we waste a few bytes to keep
	fs: u64, // the stack aligned.  Thank
	gs: u64, // you, x86 segmentation.

	vector: u64,

	// Sometimes pushed by hardware.
	error: u64,

	// Pushed by hardware.
	rip: u64,
	cs: u64,
	rflags: u64,
	rsp: u64,
	ss: u64,
}

macro_rules! gen_stub {
	($name:ident, $vecnum:expr) => {
		#[allow(dead_code)]
		#[unsafe(naked)]
		unsafe extern "C" fn $name() -> ! {
			naked_asm!("pushq $0; pushq ${}; jmp {}",
				const $vecnum, sym alltraps,
				options(att_syntax))
		}
	};
	($name:ident, $vecnum:expr, err) => {
		#[allow(dead_code)]
		#[unsafe(naked)]
		unsafe extern "C" fn $name() -> ! {
			naked_asm!("pushq ${}; jmp {}",
				const $vecnum, sym alltraps,
				options(att_syntax))
		}
	};
}

macro_rules! gen_vector_stub {
	// These cases include hardware-generated error words
	// on the trap frame
	(vector8, 8) => {
		gen_stub!(vector8, 8, err);
	};
	(vector10, 10) => {
		gen_stub!(vector10, 10, err);
	};
	(vector11, 11) => {
		gen_stub!(vector11, 11, err);
	};
	(vector12, 12) => {
		gen_stub!(vector12, 12, err);
	};
	(vector13, 13) => {
		gen_stub!(vector13, 13, err);
	};
	(vector14, 14) => {
		gen_stub!(vector14, 14, err);
	};
	(vector17, 17) => {
		gen_stub!(vector17, 17, err);
	};
	// No hardware error
	($vector:ident, $num:expr) => {
		gen_stub!($vector, $num);
	};
}

seq!(N in 0..=255 {
	gen_vector_stub!(vector~N, N);
});

#[unsafe(naked)]
unsafe extern "C" fn alltraps() -> ! {
	naked_asm!(r#"
		// Save the x86 segmentation registers.
		subq $32, %rsp
		movq $0, 24(%rsp);
		movw %gs, 24(%rsp);
		movq $0, 16(%rsp);
		movw %fs, 16(%rsp);
		movq $0, 8(%rsp);
		movw %es, 8(%rsp);
		movq $0, (%rsp);
		movw %ds, (%rsp);
		pushq %r15;
		pushq %r14;
		pushq %r13;
		pushq %r12;
		pushq %r11;
		pushq %r10;
		pushq %r9;
		pushq %r8;
		pushq %rbp;
		pushq %rdi;
		pushq %rsi;
		pushq %rdx;
		pushq %rcx;
		pushq %rbx;
		pushq %rax;
		movq %rsp, %rdi;
		callq {trap};
		popq %rax;
		popq %rbx;
		popq %rcx;
		popq %rdx;
		popq %rsi;
		popq %rdi;
		popq %rbp;
		popq %r8;
		popq %r9;
		popq %r10;
		popq %r11;
		popq %r12;
		popq %r13;
		popq %r14;
		popq %r15;
		movw (%rsp), %ds;
		movw 8(%rsp), %es;
		movw 16(%rsp), %fs;
		movw 24(%rsp), %gs;
		addq $32, %rsp;
		// Pop vector and error word.
		addq $16, %rsp;
		iretq;
		"#,
		trap = sym trap,
		options(att_syntax))
}

#[repr(C, align(4096))]
struct Idt {
	entries: [GateDesc; 256],
}

impl Idt {
	const fn empty() -> Idt {
		Idt {
			entries: [GateDesc::empty(); 256],
		}
	}

	fn init(&mut self) {
		self.entries = seq!(N in 0..=255 {
			[#(
				intr64(vector~N),
			)*]
		});
	}
}

extern "C" fn trap(frame: &mut TrapFrame) {
	println!("Exception:");
	println!("{frame:#x?}");
	println!("cr0: {:#x}", unsafe { x86::controlregs::cr0() });
	println!("cr2: {:#x}", unsafe { x86::controlregs::cr2() });
	println!("cr3: {:#x}", unsafe { x86::controlregs::cr3() });
	println!("cr4: {:#x}", unsafe { x86::controlregs::cr4() });
	println!("efer: {:#x}", unsafe {
		x86::msr::rdmsr(x86::msr::IA32_EFER)
	});
	unsafe {
		backtrace(frame.rbp);
	}
	// Arrange for the exception return to land in a halt loop.
	// The seemingly superfluous cast to usize and then again to
	// u64 is to keep clippy happy.
	frame.rip = panic::dnr as usize as u64;
}

// Print a call back trace starting from the given frame pointer.
//
// # Safety
// Be sure to call this with something you are fairly certain
// is a valid stack frame that does not alias the current stack.
unsafe fn backtrace(rbp: u64) {
	fn inner(mut rbp: u64) {
		extern "C" {
			static __sstack: [u8; 0];
			static __estack: [u8; 0];
		}
		println!("stack %rip trace:");
		let sstack = unsafe { __sstack.as_ptr() as u64 };
		let estack = unsafe { __estack.as_ptr() as u64 };
		while rbp != 0 {
			if rbp < sstack ||
				estack < rbp + 16 || rbp & 0b1111 != 0
			{
				println!(
					"bogus frame pointer {rbp:#x} \
					(stack {sstack:#x}..{estack:#x})"
				);
				break;
			}
			let p = rbp as *const u64;
			let next_rbp = unsafe { ptr::read(p) };
			if next_rbp != 0 && next_rbp <= rbp {
				println!(
					"stack corrupt \
					{next_rbp:#x} <= {rbp:#x}"
				);
				break;
			}
			let rip = unsafe { ptr::read(p.add(1)) };
			rbp = next_rbp;
			println!("{rip:#x}");
		}
	}
	inner(rbp);
}

/// Initialize and load the IDT.
/// Should be called exactly once, early in
/// boot.
pub fn init() {
	use core::sync::atomic::{AtomicBool, Ordering};
	static INITED: AtomicBool = AtomicBool::new(false);
	if INITED.swap(true, Ordering::AcqRel) {
		panic!("IDT already initialized");
	}
	static mut NANOIDT: Idt = Idt::empty();
	let nanoidt = &raw mut NANOIDT;
	let nanoidt = unsafe { &mut *nanoidt };
	nanoidt.init();
	unsafe {
		lidt(nanoidt);
	}
}
