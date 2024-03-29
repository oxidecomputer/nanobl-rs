These tools are total garbage and you probably shouldn't use them (the
inclusion of the word `tool` in the name is usually a good indicator).  What's
here:

- `ipmi`, a tiny wrapper around `spv_ipmi` for turning on, off, and resetting
  a collection of BMCs that have DNS names of the form `ethx-[something]-bmc`,
as mine do.  If yours don't, this is useless to you and you should change it
or write your own.  Handy only because of the bugs in `spv_ipmi` and the
pointlessness of IPMI "security".

- `hbstool`, a similar wrapper that puts and gets flash images.  This does
  enough that it's actually kind of useful.  Should be enhanced with the
command to clear the "I'm doing a flash upgrade" bit in the BMC that gets
stuck on sometimes, and unified with `ipmi`.  Or replace the whole thing with
a real tool.

What's not here:

- `spv_ipmi`, which appears to be an AMD fork of `ipmitool` that includes
  support for the OEM commands AMD/Insyde added to the BMC software on certain
AMD reference boards for reading and writing the behind-the-BMC boot flash
device.  Immensely useful for developing the bootloader itself, because you
can avoid using the web UI (which in some BMC FW versions doesn't even work).
This protocol should be reversed and built into a usable tool, but it's also
possible that newer OpenBMC-based firmware obviates this.  Note that this tool
dumps core if given a hostname instead of an IP address for the BMC (no word
on what happens if you give it a v6 address; possibly nuclear armageddon).
The license under which AMD forked this is unclear, and AMD don't provide a
redistributable license for their fork, so out of caution this tool is not
included.  If you need it, you'll need to get it from your AMD rep.

Seriously, this is trash.  Move along.
