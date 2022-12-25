use clap::ValueEnum;

#[derive(Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
pub enum ContinueMode {
	SIMPLE,
	LUAJIT,
	MOONSCRIPT,
}

impl Default for ContinueMode {
	fn default() -> Self {
		Self::SIMPLE
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

pub struct EnvData {
	output_code: String,
}

impl EnvData {
	pub fn new() -> Self {
		Self {
			output_code: String::with_capacity(512),
			//env_types: TypesMode::NONE,
			//env_std: LuaSTD::NONE,
		}
	}
	pub fn output_code(&self) -> &str {
		&self.output_code
	}

	pub fn add_output_code(&mut self, add: String) {
		self.output_code.push_str(&add);
	}

	pub fn rewrite_output_code(&mut self, output: String) {
		self.output_code = output
	}
}

impl Default for EnvData {
	fn default() -> Self {
		Self::new()
	}
}

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
