use std::fmt::Write;
use clap::ArgEnum;

#[derive(Copy, Clone, PartialEq, ArgEnum)]
pub enum ContinueMode {
	SIMPLE,
	LUAJIT,
	MOONSCRIPT
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
	env_dontsave: bool,
	env_pathiscode: bool,
	env_rawsetglobals: bool,
	env_debugcomments: bool,

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
			env_dontsave: false,
			env_pathiscode: false,
			env_rawsetglobals: false,
			env_debugcomments: false,
			ouput_code: String::new(),
		}
	}

	pub fn set_data(
		&mut self,
		env_tokens: bool,
		env_struct: bool,
		env_output: bool,
		env_jitbit: Option<String>,
		env_continue: ContinueMode,
		env_dontsave: bool,
		env_pathiscode: bool,
		env_rawsetglobals: bool,
		env_debugcomments: bool,
	) {
		self.env_tokens = env_tokens;
		self.env_struct = env_struct;
		self.env_output = env_output;
		self.env_jitbit = env_jitbit;
		self.env_continue = env_continue;
		self.env_dontsave = env_dontsave;
		self.env_pathiscode = env_pathiscode;
		self.env_rawsetglobals = env_rawsetglobals;
		self.env_debugcomments = env_debugcomments;
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
	pub fn env_dontsave(&self) -> bool {
		self.env_dontsave
	}
	pub fn env_pathiscode(&self) -> bool {
		self.env_pathiscode
	}
	pub fn env_rawsetglobals(&self) -> bool {
		self.env_rawsetglobals
	}
	pub fn env_debugcomments(&self) -> bool {
		self.env_debugcomments
	}
	pub fn ouput_code(&self) -> &str {
		&self.ouput_code
	}
	pub fn add_output_code(&mut self, add: String) {
		write!(self.ouput_code, "{}", add).expect("something really unexpected happened");
	}
}

impl Default for EnvData {
	fn default() -> Self {
		Self::new()
	}
}