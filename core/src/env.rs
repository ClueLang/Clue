#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use clap::ValueEnum;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[clap(rename_all = "verbatim")]
pub enum ContinueMode {
	#[default]
	#[clap(name = "simple")]
	Simple,
	LuaJIT,
	#[clap(name = "goto")]
	Goto,
	MoonScript,
}

/*
#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum TypesMode {
	NONE,
	WARN,
	STRICT,
}

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum LuaSTD {
	NONE,
	LUAJIT,
	LUA54,
	//ADD MORE LATER
}
*/

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[clap(rename_all = "verbatim")]
pub enum LuaVersion {
	#[default]
	LuaJIT,
	Lua54,
	Lua53,
	Lua52,
	Lua51,
	BLUA,
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[clap(rename_all = "verbatim")]
pub enum BitwiseMode {
	#[default]
	Clue,
	#[clap(name = "library")]
	Library,
	#[clap(name = "vanilla")]
	Vanilla,
}

#[derive(Debug, Default, Clone)]
pub struct Options {
	pub env_tokens: bool,
	pub env_struct: bool,
	pub env_jitbit: Option<String>,
	pub env_bitwise: BitwiseMode,
	pub env_continue: ContinueMode,
	pub env_rawsetglobals: bool,
	pub env_debug: bool,
	pub env_output: bool,
	pub env_target: Option<LuaVersion>,
	pub env_targetos: String,
	//pub env_types: TypesMode,
	//pub env_std: LuaSTD,
}

impl Options {
	pub fn preset(&mut self) {
		use LuaVersion::*;
		let Some(version) = self.env_target else {
			return;
		};
		match version {
			LuaJIT => {
				if self.env_jitbit.is_none() {
					self.env_jitbit = Some(String::from("bit"));
				}
				self.env_bitwise = BitwiseMode::Library;
				self.env_continue = ContinueMode::Goto;
			}
			Lua54 | Lua53 => {
				self.env_bitwise = BitwiseMode::Vanilla;
				self.env_continue = ContinueMode::Goto;
			}
			Lua52 => {
				if self.env_jitbit.is_none() {
					self.env_jitbit = Some(String::from("bit32"));
				}
				self.env_bitwise = BitwiseMode::Library;
				self.env_continue = ContinueMode::Goto;
			}
			Lua51 => {
				if self.env_jitbit.is_none() {
					self.env_jitbit = Some(String::from("bit"));
				}
				self.env_bitwise = BitwiseMode::Library;
				self.env_continue = ContinueMode::MoonScript;
			}
			BLUA => {
				self.env_bitwise = BitwiseMode::Clue;
				self.env_continue = ContinueMode::Simple;
				self.env_rawsetglobals = true;
			}
		}
	}
}
