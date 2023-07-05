//! # The Clue compiler core
//! This is the core of the Clue compiler
//! This is used by the cli but can also be used by other projects
//! It is recommended to use [`Clue`] instead of the lower level APIs unless you need to

use std::{ffi::OsStr, fmt::{Display, format}, fs, path::{Path, PathBuf}, ops::Range, sync::OnceLock};
use ahash::AHashMap;
use code::Code;
use compiler::Compiler;
use env::{BitwiseMode, ContinueMode, LuaVersion, Options};
use parser::{parse_tokens, Expression};
use preprocessor::{preprocess_code, preprocess_codes, read_file};
use scanner::{scan_code, Token};
use colored::*;

#[cfg(feature = "rpmalloc")]
#[global_allocator]
/// The best memory allocator available for Clue
static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

pub static mut FILES: OnceLock<AHashMap<String, String>> = OnceLock::new();

pub mod code;
pub mod compiler;
pub mod env;
pub mod parser;
pub mod preprocessor;
pub mod scanner;
pub mod lsp;

#[macro_export]
/// Check whether `tocheck` is `Ok` or `Err`
/// If it's `Ok` it returns it
/// If it's `Err` it converts the error to a `String` and propagates it to the caller function
macro_rules! check {
	($tocheck:expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string()),
		}
	};
}

#[macro_export]
/// Format strings, used mainly inside the Clue code base
/// This is used to format strings in a way that is more efficient than using `format!`
///
/// # Example
/// ```rust
/// use clue_core::format_clue;
///
/// let a = "Hello";
/// let b = "World";
/// let c = format_clue!(a, ", ", b, "!");
/// assert_eq!(c, "Hello, World!");
/// ```
macro_rules! format_clue {
    ($($strings:expr),+) => {{
        let vc = [
			$($strings.to_string(),)+
        ];

        vc.join("")
    }};
}

/* TO BE REDONE
/// The main Clue library API
/// This is the API that you will use to interact with Clue for most use cases
/// It's recommended to use this API instead of the lower level APIs unless you need to
pub struct Clue {
	options: Options,
}

impl Clue {
	/// Create a new `Clue` instance
	pub fn new() -> Self {
		Clue {
			options: Options::default(),
		}
	}

	/// Sets the `tokens` option
	/// If `tokens` is `true` then then the `tokens` option will be enabled
	/// If `tokens` is `false` then then the `tokens` option will be disabled
	pub fn tokens(&mut self, env_tokens: bool) {
		self.options.env_tokens = env_tokens;
	}

	/// Sets the `struct` option
	/// If `struct` is `true` then then the `struct` option will be enabled
	/// If `struct` is `false` then then the `struct` option will be disabled
	pub fn env_struct(&mut self, env_struct: bool) {
		self.options.env_struct = env_struct;
	}

	/// Sets the `bitwise_mode` option
	/// The `bitwise_mode` option is used to set the bitwise mode
	/// See [`BitwiseMode`] for the available bitwise modes
	pub fn bitwise_mode(&mut self, mode: BitwiseMode) {
		self.options.env_bitwise = mode;
	}

	/// Sets the `continue_mode` option
	/// The `continue_mode` option is used to set the continue mode
	/// See [`ContinueMode`] for the available continue modes
	pub fn continue_mode(&mut self, mode: ContinueMode) {
		self.options.env_continue = mode;
	}

	/// Sets the `rawsetglobals` option
	/// When the `rawsetglobals` option is enabled, Clue will rawset(_G, ...) instead of simply x = ... for globals
	pub fn rawsetglobals(&mut self, env_rawsetglobal: bool) {
		self.options.env_rawsetglobals = env_rawsetglobal;
	}

	/// Sets the `debug` option
	/// When the `debug` option is enabled, debug mode will be enabled
	pub fn debug(&mut self, env_debug: bool) {
		self.options.env_debug = env_debug;
	}

	/// Sets the `output` option
	/// When the `output` option is enabled, the output will be printed to the console
	pub fn output(&mut self, output: bool) {
		self.options.env_output = output;
	}

	/// Sets the `expand` option
	/// When the `expand` option is enabled, the preprocessed file will be printed to the console
	pub fn expand(&mut self, expand: bool) {
		self.options.env_expand = expand;
	}

	/// Sets the `target` option
	/// The `target` option is used to set the target Lua version
	///
	/// To enable the target Lua version, set `version` to the desired Lua version (e.g. `Some(LuaVersion::Lua53)`)
	/// To disable the target Lua version, set `version` to `None`
	///
	/// See [`LuaVersion`] for the available Lua versions
	pub fn target(&mut self, version: Option<LuaVersion>) {
		self.options.env_target = version;
		self.options.preset();
	}

	/// Sets the `target_os` option
	/// The `target_os` option is used to set the target operating system
	/// See [`std::env::const::OS`] for specifying the operating system
	pub fn target_os(&mut self, os: String) {
		self.options.env_targetos = os;
	}
}

impl Clue {
	/// Preprocesses the given code
	/// Takes a [`String`] containing the code to preprocess
	///
	/// Returns a [`Result`] containing the preprocessed code
	/// If the code was successfully preprocessed, the [`Result`] will return a [`Code`] containing the preprocessed code
	///
	/// # Errors
	/// If an error occurs while preprocessing the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///     let clue = Clue::new();
	///     let code = clue.preprocess_code("print(\"Hello World!\")".to_owned())?;
	///
	///     Ok(())
	/// }
	pub fn preprocess_code(&self, code: String) -> Result<Code, String> {
		let mut code = code;
		let filename = String::from("(library)");
		let (codes, variables, ..) = preprocess_code(
			// SAFETY: This is safe because the preprocessor will never output anything other than UTF-8
			unsafe { code.as_bytes_mut() },
			1,
			false,
			&filename,
			&self.options,
		)?;
		preprocess_codes(0, codes, &variables, &filename)
	}

	/// Preprocesses the given file
	/// Takes any type that implements [`AsRef<Path>`] and [`AsRef<OsStr>`] and [`Display`] containing the path to the file to preprocess
	///
	/// Returns a [`Result`] containing the preprocessed code
	/// If the code was successfully preprocessed, the [`Result`] will return a [`Code`] containing the preprocessed code
	///
	/// # Errors
	/// If an error occurs while preprocessing the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///     let clue = Clue::new();
	///     let code = clue.preprocess_file("../examples/fizzbuzz.clue")?;
	///
	///     Ok(())
	/// }
	/// ```
	pub fn preprocess_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		path: P,
	) -> Result<Code, String> {
		let filepath = PathBuf::from(path.to_string());
		let filename = filepath
			.file_name()
			.ok_or_else(|| format!("Invalid path: {}", path))?
			.to_string_lossy()
			.into_owned();
		let (codes, variables) = read_file(filepath, &filename, &self.options)?;
		preprocess_codes(0, codes, &variables, &filename)
	}
}

impl Clue {
	/// Scans the given preprocessed code for tokens also taking the filename
	///
	/// Takes a [`Code`] containing the preprocessed code to scan
	/// and any type that implements [`AsRef<Path>`] and [`AsRef<OsStr>`] and [`Display`] containing the filename
	///
	/// Returns a [`Result`] containing the scanned tokens
	///
	/// If the code was successfully scanned, the [`Result`] will return a [`Vec<Token>`] containing the scanned tokens
	///
	/// # Errors
	/// If an error occurs while scanning the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	pub fn scan_preprocessed_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		code: Code,
		path: P,
	) -> Result<Vec<Token>, String> {
		let filepath: &Path = path.as_ref();
		let filename = filepath
			.file_name()
			.ok_or_else(|| format!("Invalid path: {}", path))?
			.to_string_lossy()
			.into_owned();
		scan_code(code, &filename)
	}

	/// Scans the given preprocessed code for tokens
	/// Takes a [`Code`] containing the preprocessed code to scan
	///
	/// Returns a [`Result`] containing the scanned tokens
	///
	/// If the code was successfully scanned, the [`Result`] will return a [`Vec<Token>`] containing the scanned tokens
	///
	/// # Errors
	/// If an error occurs while scanning the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///   let clue = Clue::new();
	///   let code = clue.preprocess_code("print(\"Hello World!\")".to_owned())?;
	///   let tokens = clue.scan_preprocessed(code)?;
	///
	///   Ok(())
	/// }
	pub fn scan_preprocessed(&self, code: Code) -> Result<Vec<Token>, String> {
		scan_code(code, &String::from("(library)"))
	}

	/// Scans the given code for tokens
	/// Takes a [`String`] containing the code to scan
	/// Returns a [`Result`] containing the scanned tokens
	///
	/// If the code was successfully scanned, the [`Result`] will return a [`Vec<Token>`] containing the scanned tokens
	///
	/// # Errors
	/// If an error occurs while scanning the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///    let clue = Clue::new();
	///    let tokens = clue.scan_code("print(\"Hello World!\")".to_owned())?;
	///
	///   Ok(())
	/// }
	pub fn scan_code(&self, code: String) -> Result<Vec<Token>, String> {
		let code = self.preprocess_code(code)?;
		self.scan_preprocessed(code)
	}

	/// Scans the given file for tokens
	/// Takes any type that implements [`AsRef<Path>`] and [`AsRef<OsStr>`] and [`Display`] containing the path to the file to scan
	/// Returns a [`Result`] containing the scanned tokens
	///
	/// If the code was successfully scanned, the [`Result`] will return a [`Vec<Token>`] containing the scanned tokens
	///
	/// # Errors
	/// If an error occurs while scanning the file, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///   let clue = Clue::new();
	///   let tokens = clue.scan_file("../examples/fizzbuzz.clue")?;
	///
	///   Ok(())
	/// }
	pub fn scan_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		filename: P,
	) -> Result<Vec<Token>, String> {
		let code = self.preprocess_file(&filename)?;
		self.scan_preprocessed_file(code, &filename)
	}
}

impl Clue {
	/// Parses the given preprocessed code
	/// Takes a [`Code`] containing the preprocessed code to parse
	/// Returns a [`Result`] containing the parsed expression
	///
	/// If the code was successfully parsed, the [`Result`] will return a `(Expression, String)` containing the parsed expression and the static variables
	///
	/// # Errors
	/// If an error occurs while parsing the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///  let clue = Clue::new();
	///  let code = clue.preprocess_code("print(\"Hello World!\")".to_owned())?;
	///  let (expression, statics) = clue.parse_preprocessed(code)?;
	///
	///  Ok(())
	/// }
	pub fn parse_preprocessed(&self, code: Code) -> Result<(Expression, String), String> {
		let tokens = self.scan_preprocessed(code)?;
		self.parse_tokens(tokens)
	}

	/// Parses the given [`Vec`] of [`Token`]
	/// Takes a [`Vec<Token>`] containing the tokens to parse
	/// Returns a [`Result`] containing the parsed expression
	///
	/// If the code was successfully parsed, the [`Result`] will return a `(Expression, String)` containing the parsed expression and the static variables
	///
	/// # Errors
	/// If an error occurs while parsing the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///    let clue = Clue::new();
	///    let tokens = clue.scan_code("print(\"Hello World!\")".to_owned())?;
	///    let (expression, statics) = clue.parse_tokens(tokens)?;
	///
	///    Ok(())
	/// }
	pub fn parse_tokens(&self, tokens: Vec<Token>) -> Result<(Expression, String), String> {
		parse_tokens(tokens, &String::from("(library)"), &self.options)
	}

	/// Parses the given code
	/// Takes a [`String`] containing the code to parse
	/// Returns a [`Result`] containing the parsed expression
	///
	/// If the code was successfully parsed, the [`Result`] will return a `(Expression, String)` containing the parsed expression and the static variables
	///
	/// # Errors
	/// If an error occurs while parsing the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///   let clue = Clue::new();
	///   let (expression, statics) = clue.parse_code("print(\"Hello World!\")".to_owned())?;
	///
	///   Ok(())
	/// }
	pub fn parse_code(&self, code: String) -> Result<(Expression, String), String> {
		let tokens = self.scan_code(code)?;
		self.parse_tokens(tokens)
	}

	/// Parses the given file
	/// Takes any type that implements [`AsRef<Path>`] and [`AsRef<OsStr>`] and [`Display`] containing the path to the file to parse
	/// Returns a [`Result`] containing the parsed expression
	///
	/// If the code was successfully parsed, the [`Result`] will return a `(Expression, String)` containing the parsed expression and the static variables
	///
	/// # Errors
	/// If an error occurs while parsing the file, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///   let clue = Clue::new();
	///   let (expression, statics) = clue.parse_file("../examples/fizzbuzz.clue")?;
	///
	///   Ok(())
	/// }
	pub fn parse_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		path: P,
	) -> Result<(Expression, String), String> {
		let filepath: &Path = path.as_ref();
		let filename = filepath
			.file_name()
			.ok_or_else(|| format!("Invalid path: {}", path))?
			.to_string_lossy()
			.into_owned();
		let tokens = self.scan_file(&path).unwrap();

		parse_tokens(tokens, &filename, &self.options)
	}
}

impl Clue {
	/// Compiles the given [`Vec`] of [`Token`]
	/// Takes a [`Vec<Token>`] containing the tokens to compile
	/// Returns a [`Result`] containing the compiled code
	///
	/// If the code was successfully compiled, the [`Result`] will return a [`String`] containing the compiled code
	///
	/// # Errors
	/// If an error occurs while compiling the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rustrust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///    let clue = Clue::new();
	///    let tokens = clue.scan_code("print(\"Hello World!\")".to_owned())?;
	///    let code = clue.compile_tokens(tokens)?;
	///
	///    Ok(())
	/// }
	pub fn compile_tokens(&self, tokens: Vec<Token>) -> Result<String, String> {
		let (ctokens, statics) = self.parse_tokens(tokens)?;
		let filename = String::from("(library)");
		let compiler = Compiler::new(&self.options, &filename);
		Ok(statics + &compiler.compile_tokens(0, ctokens)?)
	}

	/// Compiles the given preprocessed code
	/// Takes a [`Code`] containing the preprocessed code to compile
	/// Returns a [`Result`] containing the compiled code
	///
	/// If the code was successfully compiled, the [`Result`] will return a [`String`] containing the compiled code
	///
	/// # Errors
	/// If an error occurs while compiling the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///     let clue = Clue::new();
	///     let code = clue.preprocess_code("print(\"Hello World!\")".to_owned())?;
	///     let compiled = clue.compile_preprocessed(code)?;
	///
	///     Ok(())
	/// }
	pub fn compile_preprocessed(&self, code: Code) -> Result<String, String> {
		let tokens = self.scan_preprocessed(code)?;
		self.compile_tokens(tokens)
	}

	/// Compiles the given AST
	/// Takes a [`(Expression, String)`] containing the AST to compile and the statics
	/// Returns a [`Result`] containing the compiled code
	///
	/// If the code was successfully compiled, the [`Result`] will return a [`String`] containing the compiled code
	///
	/// # Errors
	/// If an error occurs while compiling the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///    let clue = Clue::new();
	///    let parse_result = clue.parse_code("print(\"Hello World!\")".to_owned())?;
	///    let code = clue.compile_ast(parse_result)?;
	///
	///    Ok(())
	/// }
	pub fn compile_ast(&self, (ctokens, statics): (Expression, String)) -> Result<String, String> {
		let filename = String::from("(library)");
		let compiler = Compiler::new(&self.options, &filename);
		Ok(statics + &compiler.compile_tokens(0, ctokens)?)
	}

	/// Compiles the given code
	/// Takes a [`String`] containing the code to compile
	/// Returns a [`Result`] containing the compiled code
	///
	/// If the code was successfully compiled, the [`Result`] will return a [`String`] containing the compiled code
	///
	/// # Errors
	/// If an error occurs while compiling the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///    let clue = Clue::new();
	///    let code = clue.compile_code("print(\"Hello World!\")".to_owned())?;
	///
	///    Ok(())
	/// }
	pub fn compile_code(&self, code: String) -> Result<String, String> {
		let tokens = self.scan_code(code)?;
		self.compile_tokens(tokens)
	}

	/// Compiles the given file
	/// Takes any type that implements [`AsRef<Path>`] and [`AsRef<OsStr>`] and [`Display`] containing the path to the file to compile
	/// Returns a [`Result`] containing the compiled code
	///
	/// If the code was successfully compiled, the [`Result`] will return a [`String`] containing the compiled code
	///
	/// # Errors
	/// If an error occurs while compiling the code, an [`Err`] containing a [`String`] with the error message will be returned
	///
	/// # Example
	/// ```rust
	/// use clue_core::Clue;
	///
	/// fn main() -> Result<(), String> {
	///    let clue = Clue::new();
	///    let code = clue.compile_file("../examples/fizzbuzz.clue")?;
	///
	///    Ok(())
	/// }
	pub fn compile_file<P: AsRef<Path> + AsRef<OsStr> + Display>(
		&self,
		path: P,
	) -> Result<String, String> {
		let tokens = self.scan_file(&path)?;
		let result = self.compile_tokens(tokens)?;
		if self.options.env_output {
			fs::write(path, &result).map_err(|e| e.to_string())?;
		}
		Ok(result)
	}
}

/// Creates a new [`Clue`] instance with the default options
///
/// # Example
/// ```rust
/// use clue_core::Clue;
///
/// let clue = Clue::default();
/// ```
impl Default for Clue {
	fn default() -> Self {
		Clue::new()
	}
}*/

fn get_errored_edges<'a, T: Iterator<Item = &'a str>>(
    code: &'a str,
    splitter: impl FnOnce(&'a str, char) -> T,
) -> &str {
    splitter(code, '\n')
        .next()
        .unwrap_or_default()
}

pub fn finish<T>(errors: u8, to_return: T) -> Result<T, String> {
	match errors {
		0 => {
			Ok(to_return)
		},
		1 => {
			Err(String::from("Cannot continue compiling due to the previous error!"))
		}
		n => {
			Err(format!("Cannot continue compiling due to {} previous errors!", n))
		}
	}
}

pub trait ErrorMessaging {
	fn send(
		&mut self,
		kind: ColoredString,
		message: impl Into<String>,
		line: usize,
		column: usize,
		range: Range<usize>,
		is_first: bool,
		help: Option<&str>
	) {
		let filename = self.get_filename();
		let header = format!(
			"{}{} in {}:{}:{}!",
			if is_first {
				""
			} else {
				"\n----------------------------------\n\n"
			},
			kind,
			filename,
			line,
			column
		);
		let full_message = format!(
			"{}: {}{}",
			kind,
			message.into().replace('\n', "<new line>").replace('\t', "<tab>"),
			if let Some(help) = help {
				format!("\n{}: {}", "Help".cyan().bold(), help)
			} else {
				String::from("")
			}
		);
		if let Some(code) = unsafe { FILES.get().unwrap().get(filename) } {
			let before_err = get_errored_edges(&code[..range.start], str::rsplit);
			let after_err = get_errored_edges(&code[range.end..], str::split);
			let errored = &code[range];
			eprintln!(
				"{}\n\n{}{}{}\n\n{}",
				header,
				before_err.trim_start(),
				errored.red().underline(),
				after_err.trim_end(),
				full_message
			)
		} else {
			eprintln!("{}\n{}", header, full_message)
		}
	}

	fn error(
		&mut self,
		message: impl Into<String>,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>
	) {
		let is_first = self.is_first(true);
		self.send("Error".red().bold(), message, line, column, range, is_first, help)
	}

	fn expected(
		&mut self,
		expected: &str,
		got: &str,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>)
	{
		self.error(format!("Expected '{expected}', got '{got}'"), line, column, range, help)
	}

	fn expected_before(
		&mut self,
		expected: &str,
		before: &str,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>
	) {
		self.error(format!("Expected '{expected}' before '{before}'"), line, column, range, help)
	}

	fn warning(
		&mut self,
		message: impl Into<String>,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>
	) {
		let is_first = self.is_first(false);
		self.send("Warning".yellow().bold(), message, line, column, range, is_first, help)
	}

	fn get_filename(&self) -> &str;

	fn is_first(&mut self, error: bool) -> bool;
}