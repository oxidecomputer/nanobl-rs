/*
 * Copyright 2024 Oxide Computer Company
 */

use super::api::*;
use nanobl_util::{panic, prelude::*};

pub(super) static HALT_DEFN: LcmdDefn = LcmdDefn {
	name: "halt",
	help: LcmdHelp::new(&HALT_DEFN, "terminate the bootloader"),
	exec: halt,
};

fn halt(_: LcmdInstance<'_>) -> Result<LcmdValue, LcmdError> {
	println!("Bootloader terminating.");
	panic::halt();
}
