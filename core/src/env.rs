use clap::ValueEnum;

macro_rules! value_enum {
	($enum:ident, $($name:ident, $value:literal),+) => {
		#[derive(Debug, Copy, Clone, PartialEq, Eq)]
		pub enum $enum {
			$($name,)+
		}

		impl ValueEnum for $enum {
			fn value_variants<'a>() -> &'a [Self] {
				use $enum::*;
				&[$($name,)+]
			}

			fn to_possible_value<'a>(&self) -> Option<clap::PossibleValue<'a>> {
				use $enum::*;
				Some(clap::PossibleValue::new(match self {
					$($name => $value,)+
				}))
			}
		}
	};
}

value_enum!(
	ContinueMode,
	Simple, "simple",
	LuaJIT, "luajit",
	MoonScript, "moonscript"
);

impl Default for ContinueMode {
	fn default() -> Self {
		Self::Simple
	}
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

value_enum!(
	LuaVersion,
	LuaJIT, "luajit",
	Lua54, "lua54",
	BLUA, "blua"
);

#[derive(Debug, Default, Clone)]
pub struct Options {
	pub env_tokens: bool,
	pub env_struct: bool,
	pub env_jitbit: Option<String>,
	pub env_continue: ContinueMode,
	pub env_rawsetglobals: bool,
	pub env_debug: bool,
	pub env_output: bool,
	//pub env_types: TypesMode,
	//pub env_std: LuaSTD,
}

impl Options {
	pub fn preset(mut self, version: LuaVersion) -> Self {
		use LuaVersion::*;
		match version {
			LuaJIT => {
				if self.env_jitbit.is_none() {
					self.env_jitbit = Some(String::from("bit"));
				}
				self.env_continue = ContinueMode::LuaJIT;
			}
			Lua54 => {
				self.env_continue = ContinueMode::MoonScript;
			}
			BLUA => {
				self.env_continue = ContinueMode::Simple;
				self.env_rawsetglobals = true;
			}
		}
		self
	}
}