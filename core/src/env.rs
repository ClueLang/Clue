use clap::ValueEnum;

macro_rules! value_enum {
	($enum:ident, $name1:ident, $value1:literal, $($name:ident, $value:literal),+) => {
		#[derive(Debug, Copy, Clone, PartialEq, Eq)]
		pub enum $enum {
			$name1, $($name,)+
		}

		impl ValueEnum for $enum {
			fn value_variants<'a>() -> &'a [Self] {
				use $enum::*;
				&[$name1, $($name,)+]
			}

			fn to_possible_value<'a>(&self) -> Option<clap::PossibleValue<'a>> {
				use $enum::*;
				Some(clap::PossibleValue::new(match self {
					$name1 => $value1,
					$($name => $value,)+
				}))
			}
		}

		impl Default for $enum {
			fn default() -> Self {
				return $enum::$name1
			}
		}
	};
}

value_enum!(
	ContinueMode,
	Simple, "simple",
	LuaJIT, "LuaJIT",
	Goto, "goto",
	MoonScript, "MoonScript"
);

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
	LuaJIT, "LuaJIT",
	Lua54, "Lua5.4",
	BLUA, "BLUA"
);

value_enum!(
	BitwiseMode,
	Clue, "Clue",
	LuaJIT, "LuaJIT",
	Vanilla, "vanilla"
);

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
	pub fn preset(mut self) -> Self {
		use LuaVersion::*;
		let Some(version) = self.env_target else {
			return self;
		};
		match version {
			LuaJIT => {
				if self.env_jitbit.is_none() {
					self.env_jitbit = Some(String::from("bit"));
				}
				self.env_bitwise = BitwiseMode::LuaJIT;
				self.env_continue = ContinueMode::Goto;
			}
			Lua54 => {
				self.env_bitwise = BitwiseMode::Vanilla;
				self.env_continue = ContinueMode::Goto;
			}
			BLUA => {
				self.env_bitwise = BitwiseMode::Clue;
				self.env_continue = ContinueMode::Simple;
				self.env_rawsetglobals = true;
			}
		}
		self
	}
}