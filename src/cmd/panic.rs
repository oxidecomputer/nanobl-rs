/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;

pub(super) static PANIC_DEFN: LcmdDefn = LcmdDefn {
	name: "panic",
	help: LcmdHelp::new(&PANIC_DEFN, "exercise the panic handler"),
	exec: panic,
};

fn panic(_: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	panic!("{:04x},{:04x} user-requested panic", 0x7E57, 0x9A55);
}
