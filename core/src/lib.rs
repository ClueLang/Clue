use std::{ffi::OsStr, fmt::Display, path::Path};

use code::Code;
use env::{BitwiseMode, ContinueMode, LuaVersion, Options};
use preprocessor::{preprocess_code, preprocess_codes, read_file, PPCode, PPVars};
use scanner::{scan_code, Token};

#[cfg(feature = "rpmalloc")]
#[global_allocator]
static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

pub mod code;
pub mod compiler;
pub mod env;
pub mod parser;
pub mod preprocessor;
pub mod scanner;

#[macro_export]
macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string()),
		}
	};
}

#[macro_export]
macro_rules! format_clue {
	($($strings:expr),+) => {{
		use std::ops::AddAssign;
		let mut len_format_clue = 0;
		$(len_format_clue.add_assign(AsRef::<str>::as_ref(&$strings).len());)+
		let mut output_format_clue = String::with_capacity(len_format_clue);
		$(output_format_clue.push_str($strings.as_ref());)+
		output_format_clue
	}};
}

struct Clue {
	options: Options,
}

impl Clue {
	fn new() -> Self {
		Clue {
			options: Options::default(),
		}
	}

	fn tokens(&mut self, env_tokens: bool) {
		self.options.env_tokens = env_tokens;
	}
	fn env_struct(&mut self, env_tokens: bool) {
		self.options.env_tokens = env_tokens;
	}

	fn bitwise_mode(&mut self, mode: BitwiseMode) {
		self.options.env_bitwise = mode;
	}

	fn continue_mode(&mut self, mode: ContinueMode) {
		self.options.env_continue = mode;
	}

	fn rawsetglobals(&mut self, env_rawsetglobal: bool) {
		self.options.env_rawsetglobals = env_rawsetglobal;
	}

	fn debug(&mut self, env_debug: bool) {
		self.options.env_debug = env_debug;
	}

	fn output(&mut self, output: bool) {
		self.options.env_output = output;
	}

	fn target(&mut self, version: Option<LuaVersion>) {
		self.options.env_target = version;
		self.options.preset();
	}

	fn target_os(&mut self, os: String) {
		self.options.env_targetos = os;
	}
}

impl Clue {
	fn preprocess_code(&self, code: String) -> Result<Code, String> {
		let mut code = code;
		let filename = "(libclue)".to_owned();
		let (codes, variables, ..) = preprocess_code(
			unsafe { code.as_bytes_mut() },
			1,
			false,
			&filename,
			&self.options,
		)?;
		preprocess_codes(0, codes, &variables, &filename)
	}
	fn preprocess_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		path: P,
	) -> Result<Code, String> {
		let filepath: &Path = path.as_ref();
		let filename = filepath.file_name().unwrap().to_string_lossy().into_owned();
		let (codes, variables) = read_file(path, &filename, &self.options)?;
		preprocess_codes(0, codes, &variables, &filename)
	}
	fn preprocess_folder<P: AsRef<Path> + AsRef<OsStr> + Display>(&self, path: P) {
		todo!()
	}
}

impl Clue {
	fn scan_preprocessed_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		code: Code,
		path: P,
	) -> Result<Vec<Token>, String> {
		let filepath: &Path = path.as_ref();
		let filename = filepath.file_name().unwrap().to_string_lossy().into_owned();
		scan_code(code, &filename)
	}

	fn scan_preprocessed(&self, code: Code) -> Result<Vec<Token>, String> {
		scan_code(code, &"(library)".to_owned())
	}

	fn scan_code(&self, code: String) -> Result<Vec<Token>, String> {
		let code = self.preprocess_code(code)?;
		self.scan_preprocessed(code)
	}
	fn scan_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		filename: P,
	) -> Result<Vec<Token>, String> {
		let code = self.preprocess_file(&filename)?;
		self.scan_preprocessed_file(code, &filename)
	}
	fn scan_folder<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		filename: P,
	) -> Result<Vec<Token>, String> {
		todo!()
	}
}
