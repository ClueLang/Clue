#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use std::sync::RwLock;

use lazy_static::lazy_static;

use crate::env::EnvData;

/// This an allocator that supports only some platforms but has really good performances
#[cfg(feature = "rpmalloc")]
#[global_allocator]
static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

pub mod compiler;
pub mod env;
pub mod parser;
pub mod preprocessor;
pub mod scanner;

lazy_static! {
	pub static ref ENV_DATA: RwLock<EnvData> = RwLock::new(EnvData::new());
	//pub static ref LUA_G: RwLock<parser::LocalsList> = RwLock::new(None);
}

/// TODO
#[macro_export]
macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string()),
		}
	};
}

/// Creates a String by pre-allocating it and using `push_str` for every argument given to the macro
#[macro_export]
macro_rules! format_clue {
    ($($strings:expr),+) => {{
        use std::ops::AddAssign;
        let mut len_format_clue = 0;
        $(len_format_clue.add_assign(AsRef::<str>::as_ref(&$strings).len());)+
        let mut output_format_clue = String::with_capacity(len_format_clue);
        $(output_format_clue.push_str($strings.as_ref());)+
        output_format_clue
    }};
}
