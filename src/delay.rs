/*
 * Copyright 2024 Oxide Computer Company
 */

use x86::time;

/// If `stopfn()` returns Some(_), we abort the delay.
pub fn delay_unless<T, F>(ticks: u64, mut stopfn: F) -> Option<T>
where
	F: FnMut() -> Option<T>,
{
	/*
	 * Safe because this crate works only at CPL = 0.
	 * Rollover for this timer is > 200 years and it is
	 * architecturally forbidden to go backward because this crate
	 * is only useful when run on the BSP without thread scheduling.
	 */
	let then = unsafe { time::rdtscp() };
	while (unsafe { time::rdtscp() } - then < ticks) {
		let r = stopfn();
		if (r.is_some()) {
			return (r);
		}
	}

	None
}

pub fn delay(ticks: u64) {
	delay_unless::<(), _>(ticks, || None);
}
