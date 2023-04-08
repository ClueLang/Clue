//! The `env` module contains miscellaneous structs and enums related to the compiler options available
//!
//! It contains [`Options`] struct is the main struct used to store the compiler options,
//! and is used by the [`Compiler`](crate::compiler::Compiler) to determine how to compile the code
//! and also other helpful enums such as [`LuaVersion`], [`BitwiseMode`] and [`ContinueMode`]

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
	/// Simple: This mode uses the native continue keyword.
	/// This can only be used in implementations which support it.
	Simple,
	/// DEPRECATED
	///
	/// LuaJIT: Same as `Goto`, only for compatibility reasons
	LuaJIT,
	#[clap(name = "goto")]
	/// Goto: Clue will use goto continue; and a ::continue:: label when compiling `continue` keywords
	/// instead of assuming the version of Lua you're compiling to has a proper continue keyword.
	/// This will work with most versions of lua (lua 5.2, luajit, blua)
	Goto,
	/// Moonscript: This approach is guaranteed to work with any version of lua although
	/// it has a performance impact because it uses a loop.
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
	/// LuaJIT
	LuaJIT,
	/// Lua 5.4
	Lua54,
	/// Lua 5.3
	Lua53,
	/// Lua 5.2
	Lua52,
	/// Lua 5.1
	Lua51,
	/// BLUA
	BLUA,
}

#[derive(Default, Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[clap(rename_all = "verbatim")]
/// The mode to use for bitwise operations
pub enum BitwiseMode {
	#[default]
	/// Clue: This mode uses the bitwise operators from Clue as is with no change
	/// Works in BLUA
	Clue,
	#[clap(name = "library")]
	/// Library: This mode uses the bit library to perform bitwise operations
	/// Works in LuaJIT (bit), Lua 5.2 (bit32)
	Library,
	/// Vanilla: This mode uses the bitwise operators from standard Lua
	/// Works in Lua 5.3+
	#[clap(name = "vanilla")]
	Vanilla,
}

#[derive(Debug, Default, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// The compiler options for Clue
pub struct Options {
	/// Prints the tokens to stdout
	pub env_tokens: bool,
	/// Prints the AST to stdout
	pub env_struct: bool,
	/// DEPRECATED
	///
	/// The name of the varible the bits library is assigned to
	pub env_jitbit: Option<String>,
	/// The mode to use for bitwise operations
	pub env_bitwise: BitwiseMode,
	/// The continue mode to use when compiling `continue` keywords
	pub env_continue: ContinueMode,
	/// Whether to use rawset(_G, ...) instead of _G.x = ...
	pub env_rawsetglobals: bool,
	/// Whether to print debug information
	pub env_debug: bool,
	/// Whether to print the output to stdout
	pub env_output: bool,
	/// The Lua version to target
	pub env_target: Option<LuaVersion>,
	/// The path to the output file
	pub env_targetos: String,
	//pub env_types: TypesMode,
	//pub env_std: LuaSTD,
}

impl Options {
	/// Applies the chosen preset to the options
	/// This should be called after `env_jitbit` or `env_target` is set to reflect the chosen preset
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
