use clap::ValueEnum;

#[derive(Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum ContinueMode {
	Simple,
	LuaJIT,
	MoonScript,
}

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
