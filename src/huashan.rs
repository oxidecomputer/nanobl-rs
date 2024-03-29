/*
 * Copyright 2024 Oxide Computer Company
 */

use crate::dw_apb_uart::{self, mode};
use crate::nomutex::*;
use core::cell::RefCell;
use lazy_static::*;

pub struct HuashanPeripheral<P> {
	p: Option<P>,
}

macro_rules! peripheral {
	($n:ident) => {
		lazy_static! {
			pub static ref $n: NoMutex<
				RefCell<
					HuashanPeripheral<
						dw_apb_uart::$n<mode::Unknown>,
					>,
				>,
			> = {
				let p = HuashanPeripheral {
					p:
						Some(<dw_apb_uart::$n<
							mode::Unknown,
						>>::new()),
				};

				NoMutex { v: RefCell::new(p) }
			};
		}
	};
}

impl<P> HuashanPeripheral<P> {
	pub fn take(&mut self) -> Option<P> {
		self.p.take()
	}
}

peripheral!(UART0);
peripheral!(UART1);
peripheral!(UART2);
peripheral!(UART3);
