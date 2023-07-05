//! The preprocessor is the first step in the compilation process.
//! It is responsible for removing comments and expanding macros and directives.
//!
//! It exposes three functions: [`preprocess_code`], [`preprocess_codes`] and [`preprocess_variables`]

use crate::{
	check,
	finish,
	code::{Code, CodeChar},
	env::Options,
	format_clue,
	ErrorMessaging,
	SYMBOLS,
};
use ahash::AHashMap;
use std::{
	cmp,
	collections::VecDeque,
	env,
	fs,
	iter::{Peekable, Rev},
	path::PathBuf,
	str::{self, Split},
	u8
};
use utf8_decode::decode;

macro_rules! pp_if {
	($code:ident, $ifname:ident, $prev:ident) => {{
		let check = $code.$ifname(b'{');
		$code.keep_block($prev && check);
	}};
}

/// A HashMap of preprocessor variables.
pub type PPVars = AHashMap<Code, PPVar>;
/// A list of code segments and its size.
pub type PPCode = (VecDeque<(Code, bool)>, usize);

#[derive(Debug, Clone)]
/// A preprocessor variable or macro.
pub enum PPVar {
	/// A simple variable.
	Simple(Code),

	/// A variable that has to be processed before expansion.
	ToProcess(Code),

	/// A macro.
	Macro {
		/// The code of the macro.
		code: PPCode,

		/// The arguments of the macro.
		args: Vec<Code>,

		/// The preprocessor variables of the macro.
		ppvars: PPVars,

		/// Whether the macro is variadic.
		vararg: bool,
	},

	/// Variadic arguments variable in a macro.
	VarArgs(PPCode),
}

#[derive(PartialEq, Eq, PartialOrd, Ord)]
enum CommentState {
	String,
	None,
	Single,
	Multi,
	Ended,
}

struct CodeFile<'a> {
	options: &'a Options,
	code: &'a mut [u8],
	comment: CommentState,
	checked: usize,
	read: usize,
	peeked: Option<CodeChar>,
	line: usize,
	column: usize,
	filename: &'a String,
	last_if: bool,
	cscope: u8,
	ends: Vec<u8>,
	errors: u8,
}

impl ErrorMessaging for CodeFile<'_> {
	fn get_filename(&self) -> &str {
		self.filename
	}

	fn is_first(&mut self, error: bool) -> bool {
		if error {
			self.errors += 1;
		}
		self.errors == 1
	}
}

impl<'a> CodeFile<'a> {
	fn new(
		code: &'a mut [u8],
		line: usize,
		filename: &'a String,
		cscope: u8,
		options: &'a Options,
	) -> Self {
		Self {
			options,
			code,
			comment: CommentState::None,
			checked: 0,
			read: 0,
			peeked: None,
			line,
			column: 1,
			filename,
			last_if: true,
			cscope,
			ends: Vec::new(),
			errors: 0,
		}
	}

	fn is_ascii(&mut self, c: Option<CodeChar>) -> Option<CodeChar> {
		match c {
			None => None,
			Some(c) if c.0.is_ascii() => Some(c),
			Some((_, line, column)) => {
				let c = decode(
					&mut self.code[self.read - 1..cmp::min(self.read + 3, self.code.len())].iter().copied()
				).unwrap().unwrap();
				self.error(
					format!("Invalid character '{c}'"),
					line,
					column,
					self.read - 1..self.read,
					None
				);
				None
			}
		}
	}

	fn skip_whitespace(&mut self) {
		while let Some((c, ..)) = self.peek_char_unchecked() {
			if c.is_ascii_whitespace() {
				self.read_char_unchecked();
			} else {
				break;
			}
		}
	}

	fn read_char_unchecked(&mut self) -> Option<CodeChar> {
		if self.peeked.is_some() {
			let peeked = self.peeked;
			self.peeked = None;
			peeked
		} else {
			let next = self.code.get(self.read + 1).copied();
			let current = self.code.get_mut(self.read);
			if let Some(current) = current {
				let c = *current;
				self.read += 1;
				let line = self.line;
				let column = self.column;
				self.column += 1;
				if self.comment > CommentState::None && *current != b'\n' {
					*current = b' ';
				}
				match c {
					b'\n' => {
						self.line += 1;
						self.column = 1;
						if self.comment == CommentState::Single {
							self.comment = CommentState::None;
						}
					}
					b'/' if self.comment == CommentState::None => {
						if let Some(next) = next {
							self.comment = match next {
								b'/' => {
									*current = b' ';
									CommentState::Single
								}
								b'*' => {
									*current = b' ';
									CommentState::Multi
								}
								_ => CommentState::None,
							}
						}
					}
					b'*' if self.comment == CommentState::Multi => {
						if let Some(next) = next {
							if next == b'/' {
								self.comment = CommentState::Ended;
							}
						}
					}
					_ if self.comment == CommentState::Ended => {
						self.comment = CommentState::None;
					}
					_ => {}
				}
				Some((*current, line, column))
			} else {
				None
			}
		}
	}

	fn read_char(&mut self) -> Option<CodeChar> {
		let c = self.read_char_unchecked();
		self.is_ascii(c)
	}

	fn peek_char_unchecked(&mut self) -> Option<CodeChar> {
		if self.peeked.is_none() {
			self.peeked = self.read_char_unchecked();
		}
		self.peeked
	}

	fn peek_char(&mut self) -> Option<CodeChar> {
		let c = self.peek_char_unchecked();
		self.is_ascii(c)
	}

	fn assert_char(&mut self, wanted_c: u8) -> bool {
		match self.read_char() {
			None => {
				self.expected_before(
					&String::from_utf8_lossy(&[wanted_c]),
					"<end>",
					self.line,
					self.column,
					self.read - 1..self.read,
					None
				);
				false
			}
			Some((c, line, column)) if c != wanted_c => {
				self.expected(
					&String::from_utf8_lossy(&[wanted_c]),
					&String::from_utf8_lossy(&[c]),
					line,
					column,
					self.read - 1..self.read,
					None
				);
				false
			}
			_ => true
		}
	}

	fn assert_reach(&mut self, wanted_c: u8) -> bool {
		self.skip_whitespace();
		self.assert_char(wanted_c)
	}

	fn read(
		&mut self,
		mut get: impl FnMut(&mut Self) -> Option<CodeChar>,
		mut check: impl FnMut(&mut Self, CodeChar) -> bool,
	) -> Code {
		let mut code = Code::new();
		while let Some(c) = get(self) {
			if check(self, c) {
				break;
			}
			code.push(c)
		}
		code
	}

	fn read_line(&mut self) -> String {
		self.read(
			Self::read_char_unchecked,
			|_, (c, ..)| c == b'\n',
		)
		.to_string()
	}

	fn read_identifier(&mut self) -> Code {
		self.read(Self::peek_char, |code, (c, ..)| {
			if c.is_ascii_alphanumeric() || c == b'_' {
				code.read_char_unchecked().unwrap();
				false
			} else {
				true
			}
		})
	}

	fn read_string(&mut self, c: CodeChar) -> Code {
		self.comment = CommentState::String;
		let mut skip_next = false;
		let start = self.read - 1;
		self.read(
			|code| {
				let stringc = code.read_char_unchecked();
				if stringc.is_none() {
					code.error(
						"Unterminated string",
						c.1,
						c.2,
						start..code.read,
						None
					)
				}
				stringc
			},
			|code, (stringc, ..)| {
				if stringc == b'\n' {
					false
				} else if skip_next {
					skip_next = false;
					false
				} else if stringc == c.0 {
					code.comment = CommentState::None;
					true
				} else {
					if stringc == b'\\' {
						skip_next = true;
					}
					false
				}
			},
		)
	}

	fn read_until_with(
		&mut self,
		end: u8,
		f: impl FnMut(&mut Self) -> Option<CodeChar>,
	) -> Option<Code> {
		let mut reached = false;
		let result = self.read(f, |_, (c, ..)| {
			if c == end {
				reached = true;
				true
			} else {
				false
			}
		});
		reached.then_some(result)
	}

	fn read_until(&mut self, end: u8) -> Code {
		let start = self.read - 1;
		self.read_until_with(end, Self::read_char).unwrap_or_else(|| {
			self.expected_before(
				&(end as char).to_string(),
				"<end>",
				self.line,
				self.column,
				start..self.read,
				None
			);
			Code::new()
		})
	}

	fn read_macro_args(&mut self) -> Code {
		let mut args = Code::new();
		args.push(self.read_char_unchecked().unwrap());
		while let Some(c) = self.peek_char() {
			match c.0 {
				b'(' => args.append(self.read_macro_args()),
				b')' => {
					args.push(self.read_char_unchecked().unwrap());
					return args;
				}
				b'\'' | b'"' | b'`' => {
					args.push(self.read_char_unchecked().unwrap());
					args.append(self.read_string(c));
					args.push(c);
				}
				_ => args.push(self.read_char_unchecked().unwrap()),
			}
		}
		self.expected_before(
			")",
			"<end>",
			self.line,
			self.column,
			self.read - 1..self.read,
			None
		);
		Code::new()
	}

	fn read_macro_block(&mut self) -> Result<(PPCode, PPVars), String> {
		let line = self.line;
		let len = self.code.len();
		let block = &mut self.code[self.read..len];
		let (block, ppvars, line, read) =
			preprocess_code(block, line, true, self.filename, &Options::default())?;
		self.line = line;
		self.read += read;
		Ok((block, ppvars))
	}

	fn skip_block(&mut self) {
		let start = self.read - 1;
		while let Some(c) = self.read_char() {
			match c.0 {
				b'{' => self.skip_block(),
				b'}' => return,
				b'\'' | b'"' | b'`' => {
					self.read_string(c);
				}
				_ => {}
			}
		}
		self.expected_before(
			"}",
			"<end>",
			self.line,
			self.column,
			start..self.read,
			None
		)
	}

	fn keep_block(&mut self, to_keep: bool) {
		self.last_if = to_keep;
		if to_keep {
			self.ends.push(self.cscope);
			self.cscope += 1;
		} else {
			self.skip_block()
		}
	}

	fn ifos(&mut self, end: u8) -> bool {
		let checked_os = self.read_until(end).trim();
		checked_os == self.options.env_targetos
	}

	fn iflua(&mut self, end: u8) -> bool {
		use crate::env::LuaVersion::*;
		let checked_lua_version = self.read_until(end).trim();
		let Some(target) = self.options.env_target else {
			return false;
		};
		match checked_lua_version.to_string().to_lowercase().as_str() {
			"luajit" | "jit" => target == LuaJIT,
			"lua54" | "lua5.4" | "lua 54" | "lua 5.4" | "54" | "5.4" => target == Lua54,
			"blua" => target == BLUA,
			_ => false,
		}
	}

	fn ifdef(&mut self, end: u8) -> bool {
		let to_check = self.read_until(end).trim();
		env::var_os(to_check.to_string()).is_some()
	}

	fn ifndef(&mut self, end: u8) -> bool {
		!self.ifdef(end)
	}

	fn ifcmp(&mut self, end: u8) -> bool {
		let Some(to_compare1) = env::var_os(self.read_identifier().to_string()) else {
			self.read_until(end);
			return false
		};
		self.skip_whitespace();
		let comp_pos = self.read - 1;
		let comparison = [
			self.read_char_unchecked(),
			self.read_char_unchecked()
		];
		if comparison.contains(&None) {
			let size = String::from_utf8_lossy(self.code).len();
			self.expected(
				"==' or '!=",
				"<end>",
				self.line,
				self.column,
				comp_pos..cmp::min(comp_pos + 2, size),
				None
			);
			return false;
		}
		let to_compare2 = self.read_until(end).trim();
		let comparison = comparison.map(|c| c.unwrap().0);
		match &comparison {
			b"==" => to_compare2 == to_compare1,
			b"!=" => to_compare2 != to_compare1,
			_ => {
				self.expected(
					"==' or '!=",
					&String::from_utf8_lossy(&comparison),
					self.line,
					self.column,
					comp_pos..comp_pos + 2,
					None
				);
				false
			}
		}
	}

	fn bool_op(&mut self, b: bool) -> bool {
		let mut result = !b;
		loop {
			if self.r#if() == b {
				result = b;
			}
			self.skip_whitespace();
			if let Some((b')', ..)) = self.peek_char_unchecked() {
				self.read_char_unchecked();
				break result;
			}
			self.assert_char(b',');
			self.skip_whitespace();
		}
	}

	fn r#if(&mut self) -> bool {
		let check = {
			let start = self.read - 1;
			let function = self.read_identifier().to_string();
			if !self.assert_char(b'(') {
				return false;
			}
			if function.is_empty() {
				self.expected_before(
					"<name>",
					"(",
					self.line,
					self.column,
					start..self.read,
					None
				);
				return false;
			}
			self.skip_whitespace();
			match function.as_str() {
				"all" => self.bool_op(false),
				"any" => self.bool_op(true),
				"os" => self.ifos(b')'),
				"lua" => self.iflua(b')'),
				"def" => self.ifdef(b')'),
				"ndef" => self.ifndef(b')'),
				"cmp" => self.ifcmp(b')'),
				"not" => {
					let result = self.r#if();
					self.assert_char(b')');
					!result
				}
				_ => {
					self.error(
						format!("Unknown function '{function}'"),
						self.line,
						self.column,
						start..self.read,
						None
					);
					return false;
				}
			}
		};
		self.skip_whitespace();
		check
	}

	fn get_version_number(&mut self, start: usize, version: &mut Split<char>, default : &str) -> u8 {
		let num = match version.next() {
			None => {
				self.error(
					"Incomplete version",
					self.line,
					self.column,
					start..self.read,
					Some("Version must be 'X.Y.Z'")
				);
				return u8::MAX;
			}
			Some(num) if num == "*" => default,
			Some(num) => num,
		};
		match num.parse::<u8>() {
			Ok(num) => num,
			Err(_) => {
				self.error(
					"Invalid version",
					self.line,
					self.column,
					start..self.read,
					Some("Version must be 'X.Y.Z'")
				);
				u8::MAX
			},
		}
	}
}

/// Reads a file and gives back the a list of preprocessed code blocks and the variables
///
/// # Errors
/// If the file cannot be read or the code cannot be preprocessed it will return an [`Err`] with the error message
///
/// # Examples
/// ```
/// use clue_core::{env::Options, preprocessor::read_file};
///
/// fn main() -> Result<(), String> {
///     let options = Options::default();
///     let (code, vars) = read_file(
///         "../examples/macro.clue",
///         &String::from("macro.clue"),
///         &options,
///     )?;
///
///     Ok(())
/// }
/// ```
pub fn read_file(
	path: impl Into<PathBuf>,
	filename: &String,
	options: &Options,
) -> Result<(PPCode, PPVars), String> {
	let mut code = check!(fs::read_to_string(path.into()));
	let result = unsafe {
		SYMBOLS.get_mut().unwrap().insert(filename.clone(), code.clone());
		preprocess_code(code.as_bytes_mut(), 1, false, filename, options)?
	};
	Ok((result.0, result.1))
}

/// Preprocesses code and gives back the a list of preprocessed code blocks and the variable
///
/// # Errors
/// If the code cannot be preprocessed it will return an [`Err`] with the error message
///
/// # Examples
/// ```
/// use clue_core::{preprocessor::preprocess_code, env::Options};
///
/// fn main() -> Result<(), String> {
///   let options = Options::default();
///   let mut code = include_str!("../../examples/macro.clue").to_owned();
///
///   let (code, vars, ..) = preprocess_code(&mut code.into_bytes(), 1, false, &String::from("macro.clue"), &options)?;
///
///   Ok(())
/// }
#[allow(clippy::blocks_in_if_conditions)]
pub fn preprocess_code(
	code: &mut [u8],
	line: usize,
	is_block: bool,
	filename: &String,
	options: &Options,
) -> Result<(PPCode, PPVars, usize, usize), String> {
	let mut output_dir: Option<PathBuf> = None;
	let mut finalcode = VecDeque::new();
	let mut currentcode = Code::with_capacity(code.len());
	let mut size = 0;
	let mut code = CodeFile::new(code, line, filename, is_block as u8, options);
	let mut variables = PPVars::new();
	let mut pseudos: Option<VecDeque<Code>> = None;
	let mut bitwise = false;
	'main: while let Some(c) = code.read_char() {
		if match c.0 {
			b'@' => {
				let start = code.read - 1;
				let directive_name = code.read_identifier().to_string();
				code.skip_whitespace();
				let else_if = directive_name.starts_with("else_if");
				let skip = else_if && code.last_if;
				let (directive, prev) = if else_if {
					(
						directive_name
							.strip_prefix("else_")
							.expect("else_if should start with else_"),
						!code.last_if,
					)
				} else {
					(directive_name.as_str(), true)
				};
				match directive {
					"ifos" => pp_if!(code, ifos, prev),
					"iflua" => pp_if!(code, iflua, prev),
					"ifdef" => pp_if!(code, ifdef, prev),
					"ifndef" => pp_if!(code, ifndef, prev),
					"ifcmp" => pp_if!(code, ifcmp, prev),
					"if" => {
						let errors = code.errors;
						let check = code.r#if();
						if errors < code.errors || !code.assert_char(b'{') {
							break;
						}
						code.keep_block(prev && check);
					}
					"else" => {
						if !code.assert_reach(b'{') {
							break;
						}
						code.keep_block(!code.last_if);
					}
					"import" => {
						if output_dir.is_none() {
							output_dir = Some(match options.env_outputname.as_ref() {
								Some(output_dir) => output_dir
									.parent()
									.map_or_else(
										|| output_dir.to_path_buf(),
										|output_dir| output_dir.to_path_buf()
									),
								None => match env::current_dir() {
									Ok(current_dir) => current_dir,
									Err(err) => {
										code.error(
											"Could not get the output directory!",
											c.1,
											c.2,
											code.read - 1..code.read,
											Some(&err.to_string())
										);
										break
									}
								}
							})
						}
						let output_dir = output_dir.as_ref().unwrap();
						let str_start = code.read_char_unchecked();
						let module = match str_start {
							Some((b'\'' | b'"' | b'`', ..)) => {
								code.read_string(str_start.expect("character should not be None"))
							}
							_ => {
								code.expected_before(
									"<path>",
									"<end>",
									c.1,
									c.2,
									code.read - 1..code.read,
									None
								);
								continue
							}
						}.to_string();
						let mod_end = code.read - 1;
						let name = code.read_line();
						let name = name.trim();
						let mut dirs = module.split('.');
						let mut module_path = output_dir.join(dirs.next().unwrap());
						for dir in dirs {
							module_path.push(dir);
						}
						module_path.set_extension("lua");
						let function = if module_path.exists() {
							"require"
						} else {
							"import"
						};
						let (name, start) = match name.strip_prefix("=>") {
							Some(name) =>{
								let mut trimmed_name = name.trim_start().to_owned();
								if trimmed_name.is_empty() {
									code.expected(
										"<name>",
										"<empty>",
										code.line,
										code.column,
										code.read - 1..code.read,
										None
									);
									continue
								}
								if trimmed_name.contains(|c| matches!(c, '$' | '@')) {
									let (codes, new_variables, ..) = preprocess_code(
										unsafe { trimmed_name.as_bytes_mut() },
										code.line,
										false,
										filename,
										options
									)?;
									for (key, value) in new_variables {
										variables.insert(key, value);
									}
									trimmed_name = preprocess_codes(
										0,
										codes,
										&variables,
										filename
									)?.to_string();
								}
								let start = if trimmed_name.contains(|c| matches!(c, '.' | '[')) {
									""
								} else {
									"local "
								};
								(trimmed_name, start)
							},
							None => (match module.rsplit_once('.') {
								Some((_, name)) => name,
								None => &module,
							}.trim().to_string(), "local ")
						};
						if name.is_empty() {
							code.expected(
								"<file name>",
								"<empty>",
								c.1,
								c.2,
								mod_end - module.len()..mod_end,
								None
							)
						}
						currentcode.append(Code::from((
							format_clue!(start, name, " = ", function, "(\"", module, "\")"),
							c.1,
							c.2,
						)));
					}
					"version" => {
						let start = code.read - 1;
						let full_wanted_version = code.read_line();
						let full_wanted_version = full_wanted_version.trim();
						#[allow(clippy::type_complexity)]
						let (mut wanted_version, check): (&str, &dyn Fn(&u8, &u8) -> bool) =
							match full_wanted_version.strip_prefix('=') {
								Some(wanted_version) => (wanted_version, &u8::ne),
								None => (full_wanted_version, &u8::lt),
							};
						if let Some(v) = full_wanted_version.strip_prefix(">=") {
							wanted_version = v;
							println!(
								"Note: \"@version directives should no longer start with '>='\""
							);
						}
						let wanted_version_iter = &mut wanted_version.split('.');
						const CURRENT_MAJOR: &str = env!("CARGO_PKG_VERSION_MAJOR");
						const CURRENT_MINOR: &str = env!("CARGO_PKG_VERSION_MINOR");
						const CURRENT_PATCH: &str = env!("CARGO_PKG_VERSION_PATCH");
						let errors = code.errors;
						let wanted_major = code.get_version_number(
							start,
							wanted_version_iter,
							CURRENT_MAJOR
						);
						if errors < code.errors {continue}
						let wanted_minor = code.get_version_number(
							start,
							wanted_version_iter,
							CURRENT_MINOR
						);
						if errors < code.errors {continue}
						let wanted_patch = code.get_version_number(
							start,
							wanted_version_iter,
							CURRENT_PATCH
						);
						if errors < code.errors {continue}
						let current_major: u8 = CURRENT_MAJOR.parse().unwrap();
						let current_minor: u8 = CURRENT_MINOR.parse().unwrap();
						let current_patch: u8 = CURRENT_PATCH.parse().unwrap();
						if check(&current_major, &wanted_major)
						|| check(&current_minor, &wanted_minor)
						|| check(&current_patch, &wanted_patch) {
							code.error(
								if full_wanted_version.starts_with('=') {
									format_clue!(
										"This code is only compatible with version '",
										wanted_version,
										"'"
									)
								} else {
									format_clue!(
										"This code is only compatible with versions not older than '",
										wanted_version,
										"'"
									)
								},
								c.1,
								c.2,
								start..code.read,
								None
							);
							break;
						}
					}
					"define" => {
						let name = code.read_identifier();
						let mut has_values = false;
						let value = code.read(
							CodeFile::read_char_unchecked,
							|_, (c, ..)| {
								if c == b'$' {
									has_values = true;
								}
								c == b'\n'
							},
						);
						let value = value.trim();
						variables.insert(
							name,
							if has_values {
								PPVar::ToProcess(value)
							} else {
								PPVar::Simple(value)
							},
						);
					}
					"macro" => {
						let name = code.read_identifier();
						code.assert_reach(b'(');
						let (vararg, args) = {
							let mut args = Vec::new();
							loop {
								code.skip_whitespace();
								let start = code.read - 1;
								if let Some((b'.', line, column)) = code.peek_char_unchecked() {
									if code.read(CodeFile::peek_char, |code, (c, ..)| {
										if c == b'.' {
											code.read_char_unchecked();
											false
										} else {
											true
										}
									}) == "..."
									{
										code.skip_whitespace();
										code.assert_char(b')');
										break (true, args);
									} else {
										code.expected(
											",",
											".",
											line,
											column,
											start..code.read - 1,
											None
										);
									}
								}
								let arg = code.read_identifier();
								code.skip_whitespace();
								if arg.is_empty() {
									if args.is_empty() {
										code.assert_char(b')');
										break (false, args);
									}
									let (got, line, column) = match code.read_char_unchecked() {
										Some((c, line, column)) => {
											((c as char).to_string(), line, column)
										}
										None => (String::from("<end>"), code.line, code.column),
									};
									code.expected(
										"<name>",
										&got,
										line,
										column,
										start..code.read,
										None
									);
									break 'main;
								}
								args.push(arg);
								if let Some((b')', ..)) = code.peek_char_unchecked() {
									code.read_char_unchecked();
									break (false, args);
								}
								if !code.assert_char(b',') {
									break 'main;
								}
							}
						};
						code.assert_reach(b'{');
						let (code, ppvars) = code.read_macro_block()?;
						variables.insert(
							name,
							PPVar::Macro {
								code,
								args,
								ppvars,
								vararg,
							},
						);
					}
					"error" => {
						let err = code.read_line();
						code.error(err, c.1, c.2, start..code.read, None)
					},
					"print" => println!("{}", code.read_line()),
					_ => {
						code.error(
							format!("Unknown directive '{directive_name}'"),
							c.1,
							c.2,
							start..start + directive_name.len() + 1,
							None
						)
					}
				}
				if skip {
					code.last_if = true;
				}
				false
			}
			b'$' if is_block && matches!(code.peek_char_unchecked(), Some((b'{', ..))) => {
				size += currentcode.len() + 8;
				finalcode.push_back((currentcode, false));
				let name = format_clue!("_vararg", variables.len().to_string());
				finalcode.push_back((Code::from((format_clue!("$", name), c.1, c.2)), true));
				code.read_char_unchecked();
				let (vararg_code, ppvars) = code.read_macro_block().unwrap();
				variables.extend(ppvars);
				variables.insert(Code::from((name, c.1, c.2)), PPVar::VarArgs(vararg_code));
				currentcode = Code::with_capacity(code.code.len() - code.read);
				false
			}
			b'$' => {
				let mut name = code.read_identifier();
				if name.len() <= 1 && matches!(name.last(), Some((b'1'..=b'9', ..)) | None) {
					let n = match name.pop() {
						Some((c, ..)) => (c - b'0') as usize,
						None => 1,
					};
					if pseudos.is_none() {
						let tocheck = code.code[code.checked..code.read].iter().rev().peekable();
						pseudos = Some(read_pseudos(tocheck, c.1, c.2));
						code.checked = code.read;
					}
					match pseudos.as_ref().unwrap().get(n - 1) {
						Some(name) => currentcode.append(name.clone()),
						None => currentcode.append(name.clone()),
					}
				} else {
					size += currentcode.len();
					finalcode.push_back((currentcode, false));
					name.push_start(c);
					if {
						if matches!(code.peek_char_unchecked(), Some((b'!', ..))) {
							name.push(code.read_char_unchecked().unwrap());
							matches!(code.peek_char_unchecked(), Some((b'(', ..)))
						} else {
							false
						}
					} {
						name.append(code.read_macro_args())
					}
					size += name.len();
					finalcode.push_back((name, true));
					currentcode = Code::with_capacity(code.code.len() - code.read);
				}
				false
			}
			b'\'' | b'"' | b'`' => {
				currentcode.push(c);
				currentcode.append(code.read_string(c));
				true
			}
			b'&' | b'|' => {
				if code.peek_char_unchecked().unwrap_or((b'\0', 0, 0)).0 == c.0 {
					currentcode.push(code.read_char_unchecked().unwrap());
				} else {
					bitwise = true;
				}
				true
			}
			b'^' => {
				let nextc = code.peek_char_unchecked();
				if nextc.is_some() && nextc.unwrap().0 == b'^' {
					bitwise = true;
					currentcode.push(code.read_char_unchecked().unwrap());
				}
				true
			}
			b'~' => {
				bitwise = true;
				true
			}
			b'>' | b'<' => {
				currentcode.push(c);
				if let Some((nc, ..)) = code.peek_char_unchecked() {
					match nc {
						b'=' => {
							currentcode.push(code.read_char_unchecked().unwrap());
						}
						nc if nc == c.0 => {
							currentcode.push(code.read_char_unchecked().unwrap());
							bitwise = true;
						}
						_ => {}
					}
				}
				false
			}
			b'=' => {
				currentcode.push(c);
				if let Some((nc, ..)) = code.peek_char_unchecked() {
					if matches!(nc, b'=' | b'>') {
						currentcode.push(code.read_char_unchecked().unwrap());
					} else {
						pseudos = None;
					}
				}
				false
			}
			b'!' => {
				currentcode.push(c);
				if let Some((nc, ..)) = code.peek_char_unchecked() {
					if nc == b'=' {
						currentcode.push(code.read_char_unchecked().unwrap());
					}
				}
				false
			}
			b'{' if code.cscope > 0 || is_block => {
				code.cscope += 1;
				true
			}
			b'}' if code.cscope > 0 => {
				code.cscope -= 1;
				if is_block && code.cscope == 0 {
					break;
				}
				if let Some(end) = code.ends.last() {
					if code.cscope != *end {
						true
					} else {
						code.ends.pop().unwrap();
						false
					}
				} else {
					true
				}
			}
			_ => true,
		} {
			currentcode.push(c)
		}
	}
	if code.cscope > 0 {
		code.expected_before(
			"}",
			"<end>",
			code.line,
			code.column,
			code.read - 1..code.read,
			None
		);
	}
	if !currentcode.is_empty() {
		size += currentcode.len();
		finalcode.push_back((currentcode, false))
	}
	if bitwise && options.env_jitbit.is_some() {
		let bit = options.env_jitbit.as_ref().unwrap();
		let mut loader = Code::from((format_clue!("local ", bit, " = require(\"", bit, "\");"), 1, 1));
		let first = finalcode.pop_front().unwrap();
		loader.append(first.0);
		finalcode.push_front((loader, first.1));
	}
	finish(code.errors, ((finalcode, size), variables, code.line, code.read))
}

fn skip_whitespace_backwards(code: &mut Peekable<Rev<std::slice::Iter<u8>>>) {
	while let Some(c) = code.peek() {
		if c.is_ascii_whitespace() {
			code.next();
		} else {
			break;
		}
	}
}

fn read_pseudos(
	mut code: Peekable<Rev<std::slice::Iter<u8>>>,
	line: usize,
	column: usize,
) -> VecDeque<Code> {
	let mut newpseudos = VecDeque::new();
	while {
		let Some(c) = code.next() else {
			return newpseudos;
		};
		match c {
			b'=' => {
				let Some(c) = code.next() else {
					return newpseudos;
				};
				match c {
					b'!' | b'=' | b'>' | b'<' => true,
					b'.' | b'&' | b'|' | b'?' => code.next().unwrap_or(&b'\0') != c,
					_ => false,
				}
			}
			b'>' if matches!(code.peek(), Some(b'=')) => {
				code.next().unwrap();
				true
			}
			b'\'' | b'"' | b'`' => {
				while {
					let Some(nextc) = code.next() else {
						return newpseudos;
					};
					if nextc == c {
						matches!(code.peek(), Some(b'\\'))
					} else {
						true
					}
				} {}
				true
			}
			_ => true,
		}
	} {}
	skip_whitespace_backwards(&mut code);
	while {
		let mut name = Code::new();
		let mut qscope = 0u8;
		let mut in_string = false;
		while {
			if let Some(c) = code.peek() {
				match c {
					b'\'' | b'"' | b'`' => {
						name.push_start((*code.next().unwrap(), line, column));
						if !matches!(code.peek(), Some(b'\\')) {
							in_string = !in_string;
						}
						true
					}
					_ if in_string => true,
					b'[' => {
						qscope = qscope.saturating_sub(1);
						true
					}
					_ if qscope > 0 => true,
					b']' => {
						qscope += 1;
						true
					}
					b'_' | b'.' | b':' => true,
					_ => c.is_ascii_alphanumeric(),
				}
			} else {
				false
			}
		} {
			name.push_start((*code.next().unwrap(), line, column))
		}
		newpseudos.push_front(name);
		skip_whitespace_backwards(&mut code);
		if let Some(c) = code.next() {
			*c == b','
		} else {
			false
		}
	} {}
	newpseudos
}

struct CodesInfo<'a> {
	codes: &'a VecDeque<(Code, bool)>,
	filename: &'a String,
	errors: u8,
}

impl ErrorMessaging for CodesInfo<'_> {
	fn get_filename(&self) -> &str {
		self.filename
	}

	fn is_first(&mut self, error: bool) -> bool {
		if error {
			self.errors += 1;
		}
		self.errors == 1
	}
}

struct ToProcessInfo<'a> {
	code: &'a Code,
	filename: &'a String,
	errors: u8,
}

impl ErrorMessaging for ToProcessInfo<'_> {
	fn get_filename(&self) -> &str {
		self.filename
	}

	fn is_first(&mut self, error: bool) -> bool {
		if error {
			self.errors += 1;
		}
		self.errors == 1
	}
}

/// Preprocesses the list code segments, expands the variables and returns the final code.
/// Take a stacklevel, a list of code segments, the variables, and a filename.
///
/// # Errors
/// Returns an error if a variable is not found.
///
/// # Examples
/// ```
/// use clue_core::{env::Options, preprocessor::*};
///
/// fn main() -> Result<(), String> {
///     let options = Options::default();
///     let filename = String::from("macro.clue");
///     let mut code = include_str!("../../examples/macro.clue").to_owned();
///
///     let (codes, variables, ..) = preprocess_code(
///         unsafe { code.as_bytes_mut() },
///         1,
///         false,
///         &filename,
///         &options,
///     )?;
///     let codes = preprocess_codes(0, codes, &variables, &filename)?;
///
///     Ok(())
/// }
/// ```
pub fn preprocess_codes(
	stacklevel: u8,
	codes: PPCode,
	variables: &PPVars,
	filename: &String,
) -> Result<Code, String> {
	let (mut codes, size) = codes;
	let mut i = CodesInfo {
		codes: &codes,
		filename,
		errors: 0,
	};
	let mut offset = 0;
	if codes.len() == 1 {
		Ok(codes.pop_back().unwrap().0)
	} else {
		let mut code = Code::with_capacity(size);
		for (codepart, uses_vars) in &codes {
			code.append(if *uses_vars {
				preprocess_variables(stacklevel, codepart, codepart.len(), offset, variables, &mut i, filename)?
			} else {
				codepart.clone()
			});
			offset += codepart.len();
		}
		finish(i.errors, code)
	}
}

/// Expand the variables in a [`Code`]
/// Takes a stacklevel, a code segment, the variables, and a filename.
///
/// # Errors
/// Returns an error if a variable is not found, if macro expansion is too deep,
/// if a macro is not found, or if a macro receives too many or too little arguments.
///
/// # Examples
/// See [`preprocess_codes`]
///
/// ```
/// use clue_core::{code::Code, env::Options, preprocessor::*};
///
/// fn main() -> Result<(), String> {
///     let options = Options::default();
///     let filename = String::from("macro.clue");
///     let mut code = include_str!("../../examples/macro.clue").to_owned();
///
///     let (codes, variables, ..) = preprocess_code(
///         unsafe { code.as_bytes_mut() },
///         1,
///         false,
///         &filename,
///         &options,
///     )?;
///     let codes: Code = codes
///         .0
///         .iter()
///         .flat_map(|code| preprocess_variables(0, &code.0, codes.1, &variables, &filename))
///         .fold(Code::new(), |mut acc, code| {
///             acc.append(code);
///             acc
///         });
///
///     Ok(())
/// }
/// ```
pub fn preprocess_variables(
	stacklevel: u8,
	code: &Code,
	size: usize,
	offset: usize,
	variables: &PPVars,
	i: &mut impl ErrorMessaging,
	filename: &String,
) -> Result<Code, String> {
	let mut result = Code::with_capacity(size);
	let mut chars = code.iter().peekable();
	while let Some(c) = chars.next() {
		match c.0 {
			b'$' => {
				let name = {
					let mut name = Code::with_capacity(cmp::min(size - 1, 8));
					while let Some((c, ..)) = chars.peek() {
						if !(c.is_ascii_alphanumeric() || *c == b'_') {
							break;
						}
						name.push(*chars.next().unwrap())
					}
					name
				};
				if let Ok(value) = env::var(name.to_string()) {
					result.push((b'"', c.1, c.2));
					for strc in value.as_bytes() {
						result.push((*strc, c.1, c.2));
					}
					result.push((b'"', c.1, c.2));
				} else if let Some(value) = variables.get(&name) {
					if stacklevel == u8::MAX {
						i.error(
							"Too many variables called (likely recursive)",
							c.1,
							c.2,
							offset..offset + size,
							None,
						);
						break;
					}
					result.append(match value {
						PPVar::Simple(value) => value.clone(),
						PPVar::ToProcess(value) => {
							let mut value_i = ToProcessInfo {
								code: &value,
								filename,
								errors: 0
							};
							let processed = preprocess_variables(
								stacklevel + 1,
								value,
								value.len(),
								0,
								variables,
								&mut value_i,
								filename,
							)?;
							for _ in 0..value_i.errors {
								i.is_first(true);
							}
							processed
						},
						PPVar::Macro {
							code,
							args,
							ppvars,
							vararg,
						} => {
							// TODO: See issue #87
							let macro_variables = {
								let mut macro_variables = variables.clone();
								macro_variables.extend(ppvars.clone());
								let is_called = matches!(chars.next(), Some((b'!', ..)));
								if !is_called || !matches!(chars.next(), Some((b'(', ..))) {
									let name = name.to_string();
									i.error(
										"Macro not called properly",
										c.1,
										c.2,
										offset..offset + size,
										Some(&format!(
											"Replace '${name}{}' with '${name}!()'",
											if is_called {
												"!"
											} else {
												""
											}
										)),
									);
									break;
								}
								let mut args = args.iter();
								let mut varargs = 0;
								let len = macro_variables.len();
								let mut arg_offset = offset + 3 + name.len();
								loop {
									let mut value = Code::new();
									let mut cscope = 1u8;
									let end = loop {
										let Some(c) = chars.next() else {
											i.expected_before(
												")",
												"<end>",
												c.1,
												c.2,
												offset..offset + size,
												None,
											);
											break b'\0';
										};
										match c.0 {
											b'(' => cscope += 1,
											b',' if cscope == 1 => break b',',
											b')' => {
												cscope -= 1;
												if cscope == 0 {
													break b')';
												}
											}
											_ => {}
										}
										value.push(*c)
									};
									let mut value_len = value.len();
									value = value.trim_start();
									arg_offset += value_len - value.len();
									value_len = value.len();
									value = value.trim_end();
									if value.is_empty() {
										if len == macro_variables.len() && end == b')' {
											break;
										} else {
											let end = (end as char).to_string();
											i.expected_before(
												"<value>",
												&end,
												c.1,
												c.2,
												offset..offset + size,
												None,
											);
										}
									}
									let ppvalue = PPVar::Simple(preprocess_variables(
										stacklevel + 1,
										&value,
										value.len(),
										arg_offset,
										variables,
										i,
										filename,
									)?);
									arg_offset += value_len + 1;
									if let Some(arg_name) = args.next() {
										macro_variables.insert(arg_name.clone(), ppvalue);
									} else if *vararg {
										varargs += 1;
										let mut arg_name = Code::with_capacity(varargs + 1);
										arg_name.push((b'_', c.1, c.2));
										for _ in 0..varargs {
											arg_name.push((b'v', c.1, c.2));
										}
										macro_variables.insert(arg_name, ppvalue);
									} else {
										i.error(
											"Too many arguments passed to macro",
											c.1,
											c.2,
											offset..offset + size,
											None,
										);
										break;
									}
									if end == b')' {
										break;
									}
								}
								while let Some(missed) = args.next() {
									i.error(
										format!(
											"Missing argument '{}' for macro",
											missed.to_string()
										),
										c.1,
										c.2,
										offset..offset + size,
										None,
									);
								}
								macro_variables
							};
							preprocess_codes(
								stacklevel + 1,
								code.clone(),
								&macro_variables,
								filename,
							)?
						}
						PPVar::VarArgs((codes, size)) => {
							let mut result = Code::with_capacity(size * 3);
							let mut variables = variables.clone();
							let mut name = Code::from((b"_v", c.1, c.2));
							while let Some(vararg) = variables.remove(&name) {
								variables.insert(Code::from((b"vararg", c.1, c.2)), vararg);
								result.append(preprocess_codes(
									stacklevel + 1,
									(codes.clone(), *size),
									&variables,
									filename,
								)?);
								name.push(*name.last().unwrap());
							}
							result
						}
					});
				} else {
					i.error(
						format_clue!("Value '", name.to_string(), "' not found"),
						c.1,
						c.2,
						offset..offset + size,
						None,
					);
				};
			}
			b'\'' | b'"' | b'`' => {
				result.push(*c);
				while let Some(stringc) = chars.next() {
					result.push(*stringc);
					let stringc = stringc.0;
					if stringc == b'\\' {
						if let Some(nextc) = chars.next() {
							result.push(*nextc)
						}
					} else if stringc == c.0 {
						break;
					}
				}
			}
			_ => result.push(*c),
		}
	}
	Ok(result)
}
