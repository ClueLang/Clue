#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

use std::sync::RwLock;

use lazy_static::lazy_static;

use crate::env::EnvData;

pub mod compiler;
pub mod env;
pub mod parser;
pub mod scanner;

lazy_static! {
	pub static ref ENV_DATA: RwLock<EnvData> = RwLock::new(EnvData::new());
	pub static ref LUA_G: RwLock<parser::LocalsList> = RwLock::new(None);
}

#[macro_export]
macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string()),
		}
	};
}

pub fn add_global(name: String, luatype: parser::LuaType) -> Result<(), String> {
	if let Some(globals) = &mut *check!(LUA_G.write()) {
		globals.insert(name, luatype);
	}
	Ok(())
}