use clap::ValueEnum;
use std::fmt::Write;

#[derive(Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum ContinueMode {
	SIMPLE,
	LUAJIT,
	MOONSCRIPT,
}

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

#[macro_export]
macro_rules! flag {
	($arg: ident) => {
		ENV_DATA.read().expect("Can't lock env_data").$arg()
	};
}

pub struct EnvData {
	env_tokens: bool,
	env_struct: bool,
	env_output: bool,
	env_jitbit: Option<String>,
	env_continue: ContinueMode,
	env_rawsetglobals: bool,
	env_debug: bool,
	env_types: TypesMode,
	env_std: LuaSTD,

	ouput_code: String,
}

impl EnvData {
	pub fn new() -> Self {
		Self {
			env_tokens: false,
			env_struct: false,
			env_output: false,
			env_jitbit: None,
			env_continue: ContinueMode::SIMPLE,
			env_rawsetglobals: false,
			env_debug: false,
			env_types: TypesMode::NONE,
			env_std: LuaSTD::NONE,
			ouput_code: String::with_capacity(512),
		}
	}

	pub fn set_data(
		&mut self,
		env_tokens: bool,
		env_struct: bool,
		env_output: bool,
		env_jitbit: Option<String>,
		env_continue: ContinueMode,
		env_rawsetglobals: bool,
		env_debug: bool,
		env_types: TypesMode,
		env_std: LuaSTD,
	) {
		self.env_tokens = env_tokens;
		self.env_struct = env_struct;
		self.env_output = env_output;
		self.env_jitbit = env_jitbit;
		self.env_continue = env_continue;
		self.env_rawsetglobals = env_rawsetglobals;
		self.env_debug = env_debug;
		self.env_types = env_types;
		self.env_std = env_std;
	}

	pub fn env_tokens(&self) -> bool {
		self.env_tokens
	}

	pub fn env_struct(&self) -> bool {
		self.env_struct
	}

	pub fn env_output(&self) -> bool {
		self.env_output
	}

	pub fn env_jitbit(&self) -> &Option<String> {
		&self.env_jitbit
	}

	pub fn env_continue(&self) -> ContinueMode {
		self.env_continue
	}

	pub fn env_rawsetglobals(&self) -> bool {
		self.env_rawsetglobals
	}

	pub fn env_debug(&self) -> bool {
		self.env_debug
	}

	pub fn env_types(&self) -> TypesMode {
		self.env_types
	}

	pub fn env_std(&self) -> LuaSTD {
		self.env_std
	}

	pub fn ouput_code(&self) -> &str {
		&self.ouput_code
	}

	pub fn add_output_code(&mut self, add: String) {
		write!(self.ouput_code, "{add}").expect("something really unexpected happened");
	}

	pub fn rewrite_output_code(&mut self, output: String) {
		self.ouput_code = output
	}
}

impl Default for EnvData {
	fn default() -> Self {
		Self::new()
	}
}
