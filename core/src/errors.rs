use crate::{code::Position, format_clue};
use ahash::AHashMap;
use colored::*;
use std::{
	ops::Range,
	sync::{Arc, OnceLock, RwLock}, fmt::Display,
};

type FileMap = Arc<RwLock<AHashMap<String, String>>>;
type ErrorsVec = Arc<RwLock<Vec<ClueError>>>;

#[macro_export]
macro_rules! impl_errormessaging {
	($struct:ty $(, $fn:item)*) => {
		impl ErrorMessaging for $struct {
			fn get_filename(&mut self, is_error: bool) -> &str {
				self.errors = match self.errors.checked_add(is_error as u16) {
					Some(errors) => errors,
					None => {
						use $crate::errors::print_errors;
						#[cfg(feature = "lsp")]
						print_errors(false);
						#[cfg(not(feature = "lsp"))]
						print_errors();
						panic!("Too many errors, probably an error loop.");
					}
				};
				self.filename
			}

			$($fn),*
		}
	};
}

#[derive(Clone)]
pub struct ClueError {
	kind: ColoredString,
	filename: String,
	message: String,
	position: Position,
	help: Option<String>,
	code: Option<String>
}

impl Display for ClueError {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		let header = format!("{} in {}:{}:{}", self.kind, self.filename, self.position.0, self.position.1);
		let message = format!(
			"{}: {}{}",
			self.kind,
			self.message,
			self.help.as_ref().map_or_else(
				String::new,
				|help| format!("\n{}: {}", "Help".cyan().bold(), help)
			)
		);
		if let Some(code) = &self.code {
			write!(f, "{}\n\n{}\n\n{}", header, code, message)
		} else {
			write!(f, "{}\n\n{}", header, message)
		}
	}
}

#[cfg(feature = "lsp")]
impl ClueError {
	pub fn to_lsp_string(&self) -> String {
		crate::lsp::make_error_string(
			&self.kind,
			&self.message,
			&self.filename,
			self.position..{
				if let Some(code) = &self.code {
					let mut end_position = self.position;
					for c in code.split_once("\u{1B}[4;31m").unwrap().1.chars() {
						match c {
							'\u{1b}' => break,
							'\n' => {
								end_position.0 += 1;
								end_position.1 = 1;
							},
							_ => end_position.1 += 1,
						}
					}
					end_position
				} else {
					(0, 0)
				}
			}
		)
	}
}

#[inline]
fn get_files() -> FileMap {
	static FILES: OnceLock<FileMap> = OnceLock::new();
	FILES
		.get_or_init(|| Arc::new(RwLock::new(AHashMap::new())))
		.clone()
}

/// Adds the source code of a file to the FILES map used to print error messages.
pub fn add_source_file(filename: &str, code: impl Into<String>) {
	let files = get_files();
	let mut files = files.write().unwrap();
	if let Some(file) = files.get_mut(filename) {
		*file = code.into();
	} else {
		files.insert(filename.to_owned(), code.into());
	}
}

#[inline]
pub fn get_errors() -> ErrorsVec {
	static ERRORS: OnceLock<ErrorsVec> = OnceLock::new();
	ERRORS
		.get_or_init(|| Arc::new(RwLock::new(Vec::new())))
		.clone()
}

#[cfg(feature = "lsp")]
/// Prints all previous error messages stored, then clears the vector.
/// If `symbols` is true the errors will be printed the way the Clue LSP reads them.
pub fn print_errors(symbols: bool) {
	let errors = get_errors();
	let mut errors = errors.write().unwrap();
	let f = if symbols {ClueError::to_lsp_string} else {ClueError::to_string};
	for error in errors.iter() {
		println!("{}", f(error));
	}
	errors.clear();
}
#[cfg(not(feature = "lsp"))]
/// Prints all previous error messages stored, then clears the vector.
pub fn print_errors() {
	let errors = get_errors();
	let mut errors = errors.write().unwrap();
	for error in errors.iter() {
		println!("{error}");
	}
	errors.clear();
}

fn get_errored_edges<'a>(
	code: &'a str,
	splitter: impl FnOnce(&'a str, char) -> Option<(&'a str, &'a str)>,
) -> (&'a str, &'a str) {
	splitter(code, '\n').unwrap_or((code, code))
}

pub fn finish_step<T>(filename: &String, errors: u16, to_return: T) -> Result<T, String> {
	match errors {
		0 => Ok(to_return),
		1 => Err(format!(
			"Cannot continue compiling \"{filename}\" due to an error!"
		)),
		n => Err(format!(
			"Cannot continue compiling \"{filename}\" due to {n} errors!"
		)),
	}
}

pub trait ErrorMessaging {
	fn send(
		&mut self,
		is_error: bool,
		message: impl Into<String>,
		position: Position,
		range: Range<usize>,
		help: Option<&str>,
	) {
		let filename = self.get_filename(is_error);
		get_errors().write().unwrap().push(ClueError {
			filename: filename.to_string(),
			position,
			kind: if is_error { "Error".red() } else { "Warning".yellow() }.bold(),
			message: message.into().replace('\n', "<new line>").replace('\t', "<tab>"),
			help: help.map(|help| help.to_string()),
			code: get_files().read().expect("Couldn't read file map").get(filename).map(|code| {
				let before_err = get_errored_edges(&code[..range.start], str::rsplit_once).1;
				let after_err = get_errored_edges(&code[range.end..], str::split_once).0;
				let errored = &code[range];
				format_clue!(
					before_err.trim_start(),
					errored.red().underline(),
					after_err.trim_end()
				)
			})
		});
	}

	fn error(
		&mut self,
		message: impl Into<String>,
		position: Position,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.send(true, message, position, range, help)
	}

	fn expected(
		&mut self,
		expected: &str,
		got: &str,
		position: Position,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.error(
			format!("Expected '{expected}', got '{got}'"),
			position,
			range,
			help,
		)
	}

	fn expected_before(
		&mut self,
		expected: &str,
		before: &str,
		position: Position,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.error(
			format!("Expected '{expected}' before '{before}'"),
			position,
			range,
			help,
		)
	}

	fn warning(
		&mut self,
		message: impl Into<String>,
		position: Position,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.send(false, message, position, range, help)
	}

	fn get_filename(&mut self, is_error: bool) -> &str;
}
