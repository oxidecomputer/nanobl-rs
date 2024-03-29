# Nanobl-rs, an AMD64 bootloader

> [!NOTE]
> This software is not used in the Oxide Computer, directly or indirectly.
> It is a tool used to aid in bringing up new boards and for other
> experimental purposes.

The goal of this bootloader is to enter 64-bit mode, get an ELF object into
DRAM, interpret it, and call into its entry point.  It can also do other
things, but by default touches only:

* `%cr0`, `%cr4`, and `EFER` because they're necessary to enter long mode
* The general purpose registers, because that's how the processor works
* MSR 0x2FF, to set the default memory type to writeback
* Some amount of DRAM immediately below our load address, for a stack
* The UART selected as the console
* Whatever's at the other end of legacy I/O port 0x80, for historical reasons

Additional details on the contract between bootloaders and the AMD64 operating
system used on Oxide computers may be found at [the illumos kernel entry
point](https://github.com/oxidecomputer/illumos-gate/blob/6602c7873f4b6fbdd5a1b44abe31f038c9f61e6e/usr/src/uts/oxide/os/fakebop.c#L530)
and the [illumos earlyboot VM theory
statement](https://github.com/oxidecomputer/illumos-gate/blob/6602c7873f4b6fbdd5a1b44abe31f038c9f61e6e/usr/src/uts/oxide/vm/kboot_mmu.c#L90).

## Building

You may build using GNU make, or by using `cargo build` directly.  The former
will set up cargo options that are required to build successfully on illumos,
and is the recommended method.  The requirements for building are the same on
either GNU/Linux or illumos:

- GNU make (optional).  On illumos you can use `gmake` instead if `make` is
  the real make.  If you do not have GNU make at all, you can still build
using `cargo build`; see GNUmakefile for required options.

- GNU ld.  On illumos, we assume that `gld` is GNU ld and try to use that; on
  GNU/Linux we assume `ld` is GNU ld.  `ld.lld` from the LLVM suite (and its
various aliases like `rust-lld`) does NOT work correctly; it creates a GOT.
See #11.

If you are on another platform (e.g., OSX), you'll need to figure out where GNU
ld is and adjust accordingly; it should be possible to build on any platform
that has a GNU ld capable of understanding and producing amd64 ELF objects.  You
can set the following variables on the command line to adjust the environment
for and arguments to `cargo`, respectively:

- `ENV_FOR_CARGO`: The contents of this have are passed into `cargo`'s
  environment.  This is where you can do something like `make
ENV_FOR_CARGO='CARGO_BUILD_RUSTFLAGS="-C linker=/path/to/gld"'`; it obviously
can also be used to pass other things into the Rust part of the build.

- `FLAGS_FOR_CARGO`: Self-explanatory; for example, make the build more
  verbose: `make FLAGS_FOR_CARGO=-v`.

There are two (useful) output files from the build process, both of which will
by default end up in `obj` when built using GNU make: an ELF binary for the
standalone amd64 target, and a table of symbols and addresses associated with
it.  The output directory may be changed if desired by overriding the
`OUTPUT_DIR` make variable.

There are a few additional parameters that can be set at build time governing
the location and functionality of the console, including the baud rate.  These
may be found in `build.rs` along with the environment variables that may be
passed in to override them.  Use the `ENV_FOR_CARGO` facility to do this if
building with GNU make.  You should not normally need to do this, as the
defaults should work for Gimlet and AMD reference boards that have been
modified to support 3Mbps UART operation.  If you are bringing up a reference
board and need to use a RS-232 UART connector instead, you will need to lower
the baud rate at build time.

## Flash Image Building

The generated "reset image" will be placed in `obj/nanobl-rs.elf`.  This
binary may be passed to the
[amd-host-image-builder](https://github.com/oxidecomputer/amd-host-image-builder)
tool (not currently public, will be available in the future) or another tool
capable of constructing a bootable flash image for your board.  See the
documentation that comes with the tool you're using for further information.
This ELF object **cannot** be burned directly to flash, nor will you have
success using `gobjcopy` or similar tools to convert it to a raw binary and
then burning it.  AMD's hidden-core firmware requires additional configuration
and metadata beyond an AMD64 loader like nanobl-rs, and appropriate tools to
construct it.

There are some additional tools in the `tools` directory that are gross and
terrible but can be used to burn images on legacy boards with BMCs, e.g.
Ethanol-X, without using a browser.  To use `hbstool`, you will need the
`spv_ipmi` tool provided by AMD; it is not included here for licensing
reasons.  This tool does not work well when communicating with a BMC across
lossy or high-latency networks, and may not work across tunnelled links if
MTUs require fragmentation of large UDP packets.  If your reference board has
a newer BMC using OpenBMC-based software, you may be able to use open source
IPMI tools instead.  On boards with an Oxide service processor, burn the
resulting image using [humility](https://github.com/oxidecomputer/humility).
Other methods are also possible, such as hardware burners.

## Usage

Loader command (lcmd) syntax is patterned after
[mdb(1)](https://illumos.org/man/1/mdb) dcmds, but most are much more
rudimentary.  In particular, terminal capabilities are almost entirely absent;
you can backspace and that's about it.  The baud rate to use is selected at
build time; see `build.rs`.  `::help` provides a general introduction to the
tool, and `::lcmds` lists the available commands.  The basic flow looks
something like this:

```
$ hbstool x put milan-bl.bin
Firmware upload start
Image size      : 16.00 (mb)
100 % [########################################]
Firmware upload complete
Firmware update start
100 % [########################################]
Firmware update complete.
$ picocom -s "sx -Xk" --flow h -b 3000000 /dev/serial/by-id/usb-FTDI_C232HD-DDHSP-0_FT4YH7TU-if00-port0

...

> 100000000::recv -m				// Xmodem receive into DRAM
< LcmdArgs { cmd: "recv", addr: Some(100000000), count: None, args: "" }

*** file: bl.elf
$ sx -Xk bl.elf
Sending bl.elf, 52 blocks: Give your local XMODEM receive command now.
Bytes Sent:   6784   BPS:1553

Transfer complete

*** exit status: 0 ***

> 100000000::load				// Interpret ELF object
< LcmdArgs { cmd: "load", addr: Some(100000000), count: None, args: "" }
< [command error]: BaseArgsRange(RegionInForbiddenRange(RegionRangeErr { addr: 76ff0000, count: 440, range: 76effb08..=77000057, msg: "loadable segment destination overlaps with the bootloader" }))
> 101000000::recv -m				// Oops, that's the bootloader!
< LcmdArgs { cmd: "recv", addr: Some(100000000), count: None, args: "" }

*** file: add2.elf
$ sx -Xk add2.elf				// Receive the correct program
Sending add2.elf, 54 blocks: Give your local XMODEM receive command now.
Bytes Sent:   7040   BPS:1453

Transfer complete

*** exit status: 0 ***

> 100000000,100::dump				// Look at what we got
< LcmdArgs { cmd: "dump", addr: Some(100000000), count: Some(100), args: "" }
                   \/ 1 2 3  4 5 6 7  8 9 a b  c d e f  v123456789abcdef
       100000000:  7f454c46 02010100 00000000 00000000  .ELF............
       100000010:  02003e00 01000000 00f00f21 00000000  ..>........!....
       100000020:  40000000 00000000 f0190000 00000000  @...............
       100000030:  00000000 40003800 02004000 05000400  ....@.8...@.....
       100000040:  01000000 07000000 00100000 00000000  ................
       100000050:  00f00f21 00000000 00f00f21 00000000  ...!.......!....
       100000060:  00050000 00000000 00050000 00000000  ................
       100000070:  00100000 00000000 51e57464 07000000  ........Q.td....
       100000080:  00000000 00000000 00000000 00000000  ................
       100000090:  00000000 00000000 00000000 00000000  ................
       1000000a0:  00000000 00000000 10000000 00000000  ................
       1000000b0:  00000000 00000000 00000000 00000000  ................
       1000000c0:  00000000 00000000 00000000 00000000  ................
       1000000d0:  00000000 00000000 00000000 00000000  ................
       1000000e0:  00000000 00000000 00000000 00000000  ................
       1000000f0:  00000000 00000000 00000000 00000000  ................
> 100000000::load				// Interpret ELF object
< LcmdArgs { cmd: "load", addr: Some(100000000), count: None, args: "" }
210ff000					// Gives us the entry point
> 210ff000,100::dump				// Look at what we loaded
< LcmdArgs { cmd: "dump", addr: Some(210ff000), count: Some(100), args: "" }
                   \/ 1 2 3  4 5 6 7  8 9 a b  c d e f  v123456789abcdef
        210ff000:  554889e5 41554989 f5415453 4889fbbf  UH..AUI..ATSH...
        210ff010:  cccc0000 50e8af03 000041b8 00002001  ....P.....A.....
        210ff020:  b9000030 01ba0400 1001bec0 c62d00bf  ...0.........-..
        210ff030:  00000001 e88a0000 00ba1100 0000be80  ................
        210ff040:  f40f2149 89c44889 c7e80003 0000ba3b  ..!I..H........;
        210ff050:  0000004c 89e7be40 f40f21e8 ee020000  ...L...@..!.....
        210ff060:  5a4a8d04 2b5b415c 415d5dc3 554889e5  ZJ..+[A\A]].UH..
        210ff070:  41574531 ff41564c 8d771441 554989d5  AWE1.AVL.w.AUI..
        210ff080:  41544989 f4534889 fb4c89f7 51e87603  ATI..SH..L..Q.v.
        210ff090:  00004d39 ef731da8 01741948 89dfe865  ..M9.s...t.H...e
        210ff0a0:  0300004c 89f74388 043c49ff c7e85603  ...L..C..<I...V.
        210ff0b0:  0000ebde 5a4c89f8 5b415c41 5d415e41  ....ZL..[A\A]A^A
        210ff0c0:  5f5dc355 b8c0c62d 004889e5 41574156  _].U...-.H..AWAV
        210ff0d0:  41554154 53415141 89f189d6 31d241f7  AUATSAQA....1.A.
        210ff0e0:  f18d9100 00d0fe0f b6dc440f b6f08d87  ..........D.....
        210ff0f0:  000000ff 83f80377 4d448b2c 85f0f40f  .......wMD.,....
> 210ff000::call 38552f 1022c0			// Call it
< LcmdArgs { cmd: "call", addr: Some(210ff000), count:

Hello, world!

My return value will be the sum of my arguments.  Goodbye!
4877ef						// Called function's return
>
...
```

The illumos repository provides [instructions and
tools](https://github.com/oxidecomputer/illumos-gate/tree/stlouis/etc-stlouis)
for constructing and booting illumos images suitable for loading via nanobl-rs
for bringup and experimentation.  Host system software images for production
and most other uses do not use nanobl-rs and are instead built using the
[helios tools](https://github.com/oxidecomputer/helios).

### Performance

The DesignWare UART AMD uses in current EPYC processors supports operation up
to 3 Mbaud.  Thus, ideally one should be able to upload files at approximately
300 kB/s.  That's quite fast, certainly good enough for loading kernels and
minimal boot archives.  However, it is not possible to reach this speed.  If
you use a Linux client, you will by default see around 57 kB/s at 3 Mbaud.
Examining the wire protocol will show that the client-side UART (typically an
FTDI adapter) is deasserting RTS most of the time, which allows for reliable
transfer without data loss or corruption but also prevents any bytes from
being transferred during those windows.

The main reason for this is that by default the Linux `ftdi_sio` driver and
the illumos `usbftdi` driver both use a USB latency timer of 16, which is far
too slow for this speed of operation.  The USB driver doesn't slurp bytes out
of the FTDI chip's buffer often enough, so the buffer fills and the UART
deasserts RTS (as it should).  The Linux driver provides a means of reducing
this timer, however: the user program may issue `TIOCGSSERIAL` with flags
including `ASYNC_LOW_LATENCY` to effectively set `UPF_LOW_LATENCY` on the
serial port.  If your terminal communication program offers a way to induce
this, issue that argument or command; unfortunately, most don't, but there is
a bigger hammer in the form of a universal per-port setting:

```
$ sudo sh -c "echo 1 >
/sys/devices/pci0000:00/0000:00:08.1/0000:0a:00.3/usb3/3-3/3-3.4/3-3.4.1/3-3.4.1:1.0/ttyUSB0/latency_timer"
```

Of course, your device likely won't be at this address, and it may not be
ttyUSB0 either.  Adjust as needed, or try adding this udev rule to pick it up
automatically (this is for the C232H or any other FT232H; if you are using
some other USB UART, change the IDs):

```
SUBSYSTEMS=="usb", ENV{DEVTYPE}=="usb_interface", ATTRS{idVendor}=="0403", \
ATTRS{idProduct}=="6014", ATTR{*/latency_timer}="1"
```

The illumos driver does not currently provide a means of lowering the latency
timer; see [issue 14169](https://www.illumos.org/issues/14169).

Reducing the latency timer value will get you from 57 kB/s to about 180-240
kB/s.  It is possible that with an illumos client (i.e., one with DTrace), one
could correlate RTS deassertions with driver/client behaviour and bump this
higher still.  It's also possible that the nanobl-rs side needs to be better
too.  Xmodem isn't all that good for high-speed data transfer; a better
protocol like Zmodem would undoubtedly improve things, but Xmodem is cheap to
implement!

Note that if you are doing a custom build to use some lower baud rate (115200
or perhaps 1000000) because you are using an unmodified reference board with
RS-232, your maximum throughput will be correspondingly low regardless of the
latency timer or anything else you might do.  Modifying reference boards to
provide 3.3V or 1.8V UART signals at the full throughput is highly
recommended if you intend to load anything of significance over the UART.

## Environment

You are in 64-bit mode with the loader and its pagetables identity-mapped in
the bottom half of 32-bit space.  The default MTRR is enabled and set to
writeback; other MTRRs are unmodified from their reset values.  The two main
UARTs are also identity-mapped, uncacheably.  MMIO space is in the high half
of the 32-bit space, and all mappings that have been created there are marked
uncacheable (try `::mappings`).  Stay away from it unless that's what you want
to play with.  Other than in the ELF loader, there is only minimal protection;
you are responsible for knowing what is and is not safe to touch.  The general
rule is that the loader will prohibit only those actions that can never be
useful (i.e., will always cause the processor to shut down).  There are many
actions that could be useful but are usually unwise; those are mostly allowed.

Have fun!
