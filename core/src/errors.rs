use ahash::AHashMap;
use colored::*;
use std::{
	ops::Range,
	sync::{Arc, OnceLock, RwLock},
};

type FileMap = Arc<RwLock<AHashMap<String, String>>>;
type ErrorsVec = Arc<RwLock<Vec<String>>>;

#[macro_export]
macro_rules! impl_errormessaging {
	($struct:ty) => {
		impl ErrorMessaging for $struct {
			fn get_filename(&mut self, is_error: bool) -> &str {
				self.errors += is_error as u8;
				self.filename
			}
		}
	};
}

#[inline]
fn get_files() -> FileMap {
	static FILES: OnceLock<FileMap> = OnceLock::new();
	FILES
		.get_or_init(|| Arc::new(RwLock::new(AHashMap::new())))
		.clone()
}

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

pub fn print_errors() -> usize {
	let errors = get_errors();
	let mut errors = errors.write().unwrap();
	for error in errors.iter() {
		eprintln!("{error}");
	}
	let len = errors.len();
	errors.clear();
	return len;
}

fn get_errored_edges<'a, T: Iterator<Item = &'a str>>(
	code: &'a str,
	splitter: impl FnOnce(&'a str, char) -> T,
) -> &str {
	splitter(code, '\n').next().unwrap_or_default()
}

pub fn finish_step<T>(filename: &String, errors: u8, to_return: T) -> Result<T, String> {
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
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>,
	) {
		let filename = self.get_filename(is_error);
		let kind = if is_error {
			"Error".red()
		} else {
			"Warning".yellow()
		}
		.bold();
		let header = format!("{} in {}:{}:{}", kind, filename, line, column);
		let full_message = format!(
			"{}: {}{}",
			kind,
			message
				.into()
				.replace('\n', "<new line>")
				.replace('\t', "<tab>"),
			if let Some(help) = help {
				format!("\n{}: {}", "Help".cyan().bold(), help)
			} else {
				String::from("")
			}
		);
		let error = if let Some(code) = get_files()
			.read()
			.expect("Couldn't read file map")
			.get(filename)
		{
			let before_err = get_errored_edges(&code[..range.start], str::rsplit);
			let after_err = get_errored_edges(&code[range.end..], str::split);
			let errored = &code[range];
			format!(
				"{}\n\n{}{}{}\n\n{}",
				header,
				before_err.trim_start(),
				errored.red().underline(),
				after_err.trim_end(),
				full_message
			)
		} else {
			format!("{}\n{}", header, full_message)
		};
		get_errors().write().unwrap().push(error);
	}

	fn error(
		&mut self,
		message: impl Into<String>,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.send(true, message, line, column, range, help)
	}

	fn expected(
		&mut self,
		expected: &str,
		got: &str,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.error(
			format!("Expected '{expected}', got '{got}'"),
			line,
			column,
			range,
			help,
		)
	}

	fn expected_before(
		&mut self,
		expected: &str,
		before: &str,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.error(
			format!("Expected '{expected}' before '{before}'"),
			line,
			column,
			range,
			help,
		)
	}

	fn warning(
		&mut self,
		message: impl Into<String>,
		line: usize,
		column: usize,
		range: Range<usize>,
		help: Option<&str>,
	) {
		self.send(false, message, line, column, range, help)
	}

	fn get_filename(&mut self, is_error: bool) -> &str;
}
