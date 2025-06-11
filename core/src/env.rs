//! The `env` module contains miscellaneous structs and enums related to the compiler options available
//!
//! It contains [`Options`] struct is the main struct used to store the compiler options,
//! and is used by the [`Compiler`](crate::compiler::Compiler) to determine how to compile the code
//! and also other helpful enums such as [`LuaVersion`], [`BitwiseMode`] and [`ContinueMode`]

use std::{path::PathBuf, fmt::Display};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

use clap::ValueEnum;

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[clap(rename_all = "verbatim")]
/// The continue mode to use when compiling `continue` keywords
pub enum ContinueMode {
	#[default]
	#[clap(name = "simple")]
	/// This mode uses the native continue keyword,
	/// this can only be used in implementations which support it (Works in BLUA)
	Simple,

	#[clap(name = "goto")]
	/// Clue will use `goto continue;` and a `::continue::` label when compiling `continue` keywords
	/// instead of assuming the version of Lua you're compiling to has a proper continue keyword
	/// (Works in Lua 5.2+ and LuaJIT)
	Goto,

	/// This approach is guaranteed to work with any version of Lua although
	/// it has a performance impact because it uses an additional loop
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
/// The Lua version to target
pub enum LuaVersion {
	#[default]
	LuaJIT,
	Lua54,
	Lua53,
	Lua52,
	Lua51,
	BLUA,
}

impl Display for LuaVersion {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		write!(f, "{}", match self {
			LuaVersion::LuaJIT => "LuaJIT",
			LuaVersion::Lua54 => "Lua 5.4",
			LuaVersion::Lua53 => "Lua 5.3",
			LuaVersion::Lua52 => "Lua 5.2",
			LuaVersion::Lua51 => "Lua 5.1",
			LuaVersion::BLUA => "BLUA",
		})
	}
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[clap(rename_all = "verbatim")]
/// The mode to use for bitwise operations
pub enum BitwiseMode {
	#[default]
	/// Use the bitwise operators from Clue as is with no change (Works in BLUA)
	Clue,

	#[clap(name = "library")]
	/// This mode uses the bit library to perform bitwise operations
	/// (Works in LuaJIT (using bit) and Lua 5.2 (using bit32))
	Library,

	#[clap(name = "vanilla")]
	/// This mode uses the bitwise operators from standard Lua
	/// (Works in Lua 5.3+)
	Vanilla,
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
//#[clap(rename_all = "verbatim")]
pub enum KeepMode {
	#[default]
	/// Keep every instance
	Keep,

	/// Emit a warning if used and remove every instance
	Remove,

	/// Fail compilation if used
	Forbid,
}

impl Display for KeepMode {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
		write!(f, "{}", match self {
			KeepMode::Keep => "keep",
			KeepMode::Remove => "remove",
			KeepMode::Forbid => "forbid",
		})
	}
}

#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The compiler options for Clue
pub struct Options {
	/// The path of the output file, if any
	pub env_outputname: Option<PathBuf>,

	/// Prints the tokens to stdout
	pub env_tokens: bool,

	/// Prints the AST to stdout
	pub env_struct: bool,

	/// The name of the varible the bits library is assigned to
	pub env_bitlib: Option<String>,

	/// The mode to use for bitwise operations
	pub env_bitwise: BitwiseMode,

	/// The continue mode to use when compiling `continue` keywords
	pub env_continue: ContinueMode,

	/// Whether to use rawset(_G, ...) instead of simply x = ... for globals
	pub env_rawsetglobals: bool,

	/// The mode to use for number suffixes (LL, ULL, i)
	pub env_numsuffix: KeepMode,

	/// Whether to print debug information
	pub env_debug: bool,

	/// Whether to print the output to stdout
	pub env_output: bool,

	/// Whether to print the preprocessed file
	pub env_expand: bool,

	/// The Lua version to target
	pub env_target: Option<LuaVersion>,

	/// The path to the output file
	pub env_targetos: String,

	/// Whether to print the symbol table
	pub env_symbols: bool,

	//pub env_types: TypesMode,
	//pub env_std: LuaSTD,
}

impl Options {
	/// Applies the chosen preset to the options
	/// This should be called after `env_bitlib` or `env_target` is set to reflect the chosen preset
	pub fn preset(&mut self) {
		use LuaVersion::*;
		let Some(version) = self.env_target else {
			return;
		};
		match version {
			LuaJIT => {
				if self.env_bitlib.is_none() {
					self.env_bitlib = Some(String::from("bit"));
				}
				self.env_bitwise = BitwiseMode::Library;
				self.env_continue = ContinueMode::Goto;
			}
			Lua54 | Lua53 => {
				self.env_bitwise = BitwiseMode::Vanilla;
				self.env_continue = ContinueMode::Goto;
			}
			Lua52 => {
				if self.env_bitlib.is_none() {
					self.env_bitlib = Some(String::from("bit32"));
				}
				self.env_bitwise = BitwiseMode::Library;
				self.env_continue = ContinueMode::Goto;
			}
			Lua51 => {
				if self.env_bitlib.is_none() {
					self.env_bitlib = Some(String::from("bit"));
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
