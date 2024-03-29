#
# Copyright 2021 Oxide Computer Company
#

#
# This is what the world looks like when we start.  See also the block comment
# in bl.gld.  The addresses here assume _BL_BSS_SEGS == 1 which implies the
# Rust BSS and pagetables starting at 0x7FFF_0000, but increasing it just moves
# the rest of the addresses down without changing any of the relative addresses
# within the bootblock.
#
# 8000_0000 +--------------------+ <-- __TOP_RAM, __ebss
#           | Unused pagetables  |
# 7FFF_7000 +- - - - - - - - - - + <-- __epagetable
#           | empty PT           |
# 7FFF_6000 +--------------------+ <-- pt_uarts
#           | empty PT           |
# 7FFF_5000 +--------------------+ <-- pt_loader
#           | empty PT           |
# 7FFF_4000 +--------------------+ <-- pdt_3g
#           | empty PT           |
# 7FFF_3000 +--------------------+ <-- pdt_1g
#           | empty PT           |
# 7FFF_2000 +--------------------+ <-- pdpt
#           | empty PT           |
# 7FFF_1000 +--------------------+ <-- pml4, __spagetable
#           | Unused             |
# 7FFF_xxxx +- - - - - - - - - - +
#           | .bss               |
# 7FFF_0000 +--------------------+ <-- __sbss
#           | _reset             |
# 7FFE_FFF0 +--------------------+ <-- %ip: FFF0
#           | .boot.text         |
# 7FFE_FE00 +--------------------+
#           | .boot.rodata ...   |
# 7FFE_FD50 +--------------------+
#           | GDT                |
# 7FFE_FD20 +--------------------+ <-- gdt16
#           | ...                |
#           | 0x7ffe_fd20        |
# 7FFE_FD0A +--------------------+ <-- gdt16addr
#           | 0x17               |
# 7FFE_FD08 +--------------------+ <-- gdt16lim
#           | ...                |
#           | 0x0000 0x0000_0000 |
# 7FFE_FD00 +--------------------+ <-- .boot.rodata, idt16
#           | .text, .data, ...  |
# 7FFE_0000 +- - - - - - - - - - + <-- %cs: F000 (not really)
#           | .text, .data, ...  |
# 7Fxx_x000 +--------------------+ <-- __BL_BASE
#
# In keeping with our general ideology of doing as little as possible, the
# only function of this code is to get into legacy protected mode with a
# single 4GB code segment and a single 4GB data segment, commonly referred
# to as 32-bit protected mode or big-flat mode.  Once there, we create minimal
# pagetables, enter long mode, give ourselves a stack, andget out of here,
# transferring control to Rust code.
#
# The contents of bl.gld (and its comments) should be read in concert with this
# discussion and the code below to understand where the various restrictions
# come from and what is required to relax them if more space is needed for some
# particular use.  These mechanisms have generally been selected to provide
# simplicity and ease of understanding for this bootblock code and flexibility
# for Rust code, at the expense of flexibility here.  The idea behind that is
# that the only purpose of this code is to set us up to run Rust, so it's not
# likely to require much change; all new features will be in Rust.
#

MSR_EFER =		0xC0000080
MSR_EFER_LME =		0x100
MSR_EFER_NXE =		0x800
.hidden MSR_EFER, MSR_EFER_LME, MSR_EFER_NXE

MSR_MTRR_DEF_TYPE =	0x2FF
MSR_MTRR_DEF_TYPE_EN =	0x800
MTRR_TYPE_WB =		0x06
.hidden MSR_MTRR_DEF_TYPE, MSR_MTRR_DEF_TYPE_EN, MTRR_TYPE_WB

CR0_PG =		0x80000000
CR0_ET =		0x00000010
CR0_WP =		0x00010000
CR0_PE =		0x00000001
.hidden CR0_PG, CR0_ET, CR0_PE

CR4_PAE =		0x20
.hidden CR4_PAE

PML4E_RW =		0x2
PML4E_P =		0x1
.hidden PML4E_RW, PML4E_P

PDPE_RW =		0x2
PDPE_P =		0x1
.hidden PDPE_RW, PDPE_P

PDE_RW =		0x2
PDE_P =			0x1
.hidden PDE_RW, PDE_P

PTE_NOCACHE =		0x10
PTE_RW =		0x2
PTE_P =			0x1
.hidden PTE_NOCACHE, PTE_RW, PTE_P

PAGE_MASK =		0xFFFFF000
PAGE_SIZE =		0x1000
PAGE_SHIFT =		12
PDE_SHIFT =		21
IDX_MASK =		0x1FF
PDT_IDX_MASK =		IDX_MASK << PDE_SHIFT
PT_IDX_MASK =		IDX_MASK << PAGE_SHIFT
.hidden PAGE_MASK, PAGE_SIZE, PAGE_SHIFT, PDE_SHIFT, PDT_IDX_MASK, PT_IDX_MASK

UART0_REGS =		0xFEDC9000
UART1_REGS =		0xFEDCA000
.hidden UART0_REGS, UART1_REGS

IOPORT_DEBUG =		0x80
DEBUGMSG_HELLO =	0x1DE
DEBUGMSG_HALTED =	0xFFFFDEAD
.hidden IOPORT_DEBUG, DEBUGMSG_HELLO, DEBUGMSG_HALTED

#
# Offsets within .boot.rodata.
#
IDT16 =			0x0000
GDTPTR =		0x0008
GDT =			0x0020
.hidden IDT16, GDTPTR, GDT

.section ".boot.rodata", "a", @progbits
idt16:
	.word 0
	.long 0

.align 16
gdtptr:
	.word gdt_sz - 1
	.long gdt
	.long 0

.align 16
gdt:
	# Unusable entry
	.word	0x0000, 0x0000
	.byte	0x00, 0x00, 0x00, 0x00

	# Data segment, selector 0x08
	.word	0xffff, 0x0000
	.byte	0x00, 0x93, 0xcf, 0x00

	# Code segment, selector 0x10
	.word	0xffff, 0x0000
	.byte	0x00, 0x9a, 0xcf, 0x00

	# unused
	.quad	0

	# unused
	.quad	0

	# 64-bit fake code segment, selector 0x28
	.quad	0x00209a0000000000
gdt_sz = . - gdt

.section ".boot.text", "ax", @progbits
.code16
_start16:
	cli

	#
	# %eax contains the BIST result.  We'd like to save it.
	#
	movl	%eax, %ebp

	#
	# Say hello!
	#
	movl	$DEBUGMSG_HELLO, %eax
	outl	%eax, $IOPORT_DEBUG

	#
	# Invalidate the TLB; this may be superstition.
	#
	xorl	%eax, %eax
	movl	%eax, %cr3

	#
	# The IDT and GDT addresses must be offsets within this segment, which
	# the link-editor doesn't really handle very well.  Instead of asking
	# it to do so, we provide the segment base address as a symbol and
	# compute the proper offset at runtime.
	#
	movl	$idt16, %ebx
	subl	$__bootblock_segbase, %ebx
	lidtw	%cs:(%bx)

	movl	$gdtptr, %ebx
	subl	$__bootblock_segbase, %ebx
	lgdtl	%cs:(%bx)

	movl	$(CR0_ET | CR0_PE), %eax
	movl	%eax, %cr0
	ljmpl	$0x10, $_start32

.code32
_start32:
	#
	# All data segment descriptors are set to the singular big flat mode
	# data segment, which is read-write.
	#
	movw	$0x08, %ax
	movw	%ax, %ds
	movw	%ax, %es
	movw	%ax, %ss
	movw	%ax, %fs
	movw	%ax, %gs

	#
	# Apply racing stripes: set default memory type to WB.  We promise
	# that all accesses to non-WB regions from this point forward will be
	# through mappings with a lower cacheability; we are about to map the
	# UARTs UC.  Our contract with the OS requires it to either follow
	# this restriction (it does so) or disable the default MTRR prior to
	# such an access.
	#
	# Important: Since paging isn't on yet, this means we must not access
	# any MMIO space until we are in long mode.
	#
go_fast:
	movl	$MSR_MTRR_DEF_TYPE, %ecx
	movl	$(MSR_MTRR_DEF_TYPE_EN | MTRR_TYPE_WB), %eax
	xorl	%edx, %edx
	wrmsr

	#
	# Clear the BSS, because it includes the pagetables we're going to set
	# up next.  This would be roughly twice as fast if we could wait until
	# we're in long mode, but alas we cannot.
	#
clear_bss:
	movl	$__ebss, %ecx
	movl	$__sbss, %edi
	xorl	%eax, %eax
	subl	%edi, %ecx
	shrl	$2, %ecx
	rep stosl

	#
	# Program, map thyself.  We create identity mappings for a single
	# region from __sstack to __ebss.  This includes stack space, the
	# loader, this bootblock, and the BSS which in turn includes the
	# pagetables themselves.
	#
	# Rust code can create additional mappings as required; see
	# src/pagetable_impl.rs.  XXX As a temporary measure until we have the
	# new improved UART driver, we also map the UARTs here.  This should
	# be removed and done by Rust instead.
	#
make_pagetables:
	# Top level, single entry pointing to pdpt
	movl	$(pdpt + PML4E_RW + PML4E_P), pml4

	# Second level, pdpt has two entries pointing to pdt_1g and pdt_3g
	movl	$(pdt_1g + PDPE_RW + PDPE_P), pdpt + 8
	movl	$(pdt_3g + PDPE_RW + PDPE_P), pdpt + 0x18

	#
	# Third and fourth levels: we have two regions to map, one which covers
	# the loader and is done below, the other which covers the two UARTs
	# which are entirely fixed.  We do those here.
	#
.equiv	UARTS_PDEVAL, pt_uarts + PDE_RW + PDE_P
.equiv	UARTS_PDEADDR, pdt_3g + ((UART0_REGS & PDT_IDX_MASK) >> (PDE_SHIFT - 3))
.equiv	UART0_PTEVAL, UART0_REGS + PTE_NOCACHE + PTE_RW + PTE_P
.equiv	UART1_PTEVAL, UART1_REGS + PTE_NOCACHE + PTE_RW + PTE_P
.equiv	UART0_PTEADDR, pt_uarts + (UART0_REGS & PT_IDX_MASK) >> (PAGE_SHIFT - 3)
.equiv	UART1_PTEADDR, pt_uarts + (UART1_REGS & PT_IDX_MASK) >> (PAGE_SHIFT - 3)

	movl	$UARTS_PDEVAL, UARTS_PDEADDR
	movl	$UART0_PTEVAL, UART0_PTEADDR
	movl	$UART1_PTEVAL, UART1_PTEADDR

	#
	# Now the loader itself, including the pagetables.  While these values
	# are known constants at build time, the assembler and link-editor
	# can't communicate them effectively so we compute them at runtime.
	# We assume everything fits into a single bottom-level pagetable, which
	# is to say it is all in the same 2 MiB region.  Change this code if
	# not; the link-editor script will fail if it won't fit.
	#
	# First we compute and store a PDE referring to the pagetable at
	# pt_loader to map the 2 MiB region containing [__sstack, __ebss).
	#
.equiv	LOADER_PDEVAL, pt_loader + PDE_RW + PDE_P

	movl	$__sstack, %eax		# first addr to map
	movl	$pdt_1g, %ebx		# PDT base
	shrl	$PDE_SHIFT, %eax	# lpfn of region to map
	andl	$IDX_MASK, %eax		# index into PDT
	leal	(%ebx, %eax, 8), %ebx	# address of PDE
	movl	$LOADER_PDEVAL, (%ebx)

	#
	# Now build the pagetable at pt_loader to map the parts of that 2 MiB
	# region we care about using 4 KiB pages.  These are all read-write
	# identity mappings, so PTEval == (pfn << PAGE_SHIFT) | RW | P.
	#
	# %ecx == number of pages to map, computed by:
	#
	# (__ebss - __sstack) >> PAGE_SHIFT or
	# (__ebss - 1 - (__sstack - 1)) >> PAGE_SHIFT as it is here
	#
	# __ebss and __sstack are required to be page-aligned, enforced by
	# the link-editor configuration.
	#
	movl	$(__ebss - 1), %eax	# last address to map
	movl	$pt_loader, %ebx	# PT base
	movl	%eax, %ecx		# build page count in %ecx
	shrl	$PAGE_SHIFT, %eax	# pfn of last page to map
	subl	$(__sstack - 1), %ecx	# region size in bytes
	movl	%eax, %edi		# build PTEval in %eax, index in %edi
	shll	$PAGE_SHIFT, %eax	# identity-map page base address
	andl	$IDX_MASK, %edi		# index into PT
	shrl	$PAGE_SHIFT, %ecx	# final page count to map
	leal	(%ebx, %edi, 8), %ebx	# address of PTE
	addl	$(PTE_RW | PTE_P), %eax	# set flags in PTEval

1:
	movl	%eax, (%ebx)
	subl	$8, %ebx		# address of next PTE, 8 bytes each
	subl	$PAGE_SIZE, %eax	# next PTEval = next lower pfn
	loop	1b

lm_setup:
	movl	$pml4, %eax
	movl	%eax, %cr3

	movl	%cr4, %eax
	orl	$CR4_PAE, %eax
	movl	%eax, %cr4

	movl	$MSR_EFER, %ecx
	rdmsr
	orl	$(MSR_EFER_LME | MSR_EFER_NXE), %eax
	wrmsr

	movl	%cr0, %eax
	orl	$(CR0_PG | CR0_WP), %eax
	movl	%eax, %cr0
	ljmpl	$0x28, $_start64

.code64
_start64:

	#
	# Now give ourselves a stack, then call _start(bist_result), which
	# should never return.
	#
	movq	$__estack, %rsp
	movl	%ebp, %edi	# saved BIST result
	movq	$0, %rbp	# fake frame pointer
	call	_start

.globl dnr
dnr:
	movl	$DEBUGMSG_HALTED, %eax
	outl	%eax, $IOPORT_DEBUG
1:
	cli
	hlt
	jmp	1b

#
# We start here; the jump is the first instruction executed on the BSP.
#
.section ".boot.reset", "ax", @progbits
.code16
.globl _reset
_reset:
	jmp	_start16

.org 0x10, 0x90

.section ".pagetables", "aw", @nobits
.align 0x1000, 0
.globl __spagetable
__spagetable:
pml4:
	.skip	PAGE_SIZE

pdpt:
	.skip	PAGE_SIZE

pdt_1g:
	.skip	PAGE_SIZE

pdt_3g:
	.skip	PAGE_SIZE

pt_loader:
	.skip	PAGE_SIZE

pt_uarts:
	.skip	PAGE_SIZE

.globl __epagetable
__epagetable:
