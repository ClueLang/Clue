use crate::{format_clue, check, code::{Code, CodeChar}};
use ahash::AHashMap;
use utf8_decode::decode;
use std::{
	collections::{linked_list::Iter, VecDeque},
	env,
	iter::{Peekable, Rev},
	str,
	path::Path,
	ffi::OsStr,
	fmt::Display,
	fs, u8::MAX,
};

pub type PPVars = AHashMap<Code, PPVar>;

#[derive(Clone)]
pub enum PPVar {
	Simple(Code)
}

fn error(msg: impl Into<String>, line: usize, filename: &String) -> String {
	println!("Error in file \"{filename}\" at line {line}!");
	msg.into()
}

fn expected_before(expected: &str, before: &str, line: usize, filename: &String) -> String {
	error(
		format_clue!("Expected '", expected, "' before '", before, "'"),
		line,
		filename,
	)
}

struct CodeFile<'a> {
	code: Vec<u8>,
	checked: usize,
	read: usize,
	peeked: Option<CodeChar>,
	line: usize,
	filename: &'a String,
	last_if: bool,
}

impl<'a> CodeFile<'a> {
	fn new(code: Vec<u8>, filename: &'a String) -> Self {
		Self {
			code,
			checked: 0,
			read: 0,
			peeked: None,
			line: 1,
			filename,
			last_if: false,
		}
	}

	fn is_ascii(&mut self, c: Option<CodeChar>) -> Result<Option<CodeChar>, String> {
		match c {
			None => Ok(None),
			Some(c) if c.0.is_ascii() => Ok(Some(c)),
			Some((_, line)) => {
				let c = check!(decode(&mut self.code.iter().skip(self.read - 1).copied()).unwrap());
				Err(error(format!("Invalid character '{c}'"), line, self.filename))
			}
		}
	}

	fn skip_whitespace(&mut self) -> Result<(), String> {
		while let Some((c, _)) = self.peek_char()? {
			if c.is_ascii_whitespace() {
				self.read_char().unwrap();
			} else {
				break;
			}
		}
		Ok(())
	}

	fn read_char_unchecked(&mut self) -> Option<CodeChar> {
		if self.peeked.is_some() {
			let peeked = self.peeked;
			self.peeked = None;
			peeked
		} else if let Some(c) = self.code.get(self.read).copied() {
			self.read += 1;
			let result = (c, self.line);
			if c == b'\n' {
				self.line += 1;
			}
			return Some(result);
		} else {
			return None;
		}
	}

	fn read_char(&mut self) -> Result<Option<CodeChar>, String> {
		let c = self.read_char_unchecked();
		self.is_ascii(c)
	}

	fn peek_char_unchecked(&mut self) -> Option<CodeChar> {
		if self.peeked.is_none() {
			self.peeked = self.read_char_unchecked();
		}
		self.peeked
	}

	fn peek_char(&mut self) -> Result<Option<CodeChar>, String> {
		let c = self.peek_char_unchecked();
		self.is_ascii(c)
	}

	fn read(
		&mut self,
		mut get: impl FnMut(&mut Self) -> Result<Option<CodeChar>, String>,
		mut check: impl FnMut(&mut Self, CodeChar) -> bool,
	) -> Result<Code, String> {
		let mut code = Code::new();
		while let Some(c) = get(self)? {
			if check(self, c) {
				break
			}
			code.push(c)
		}
		Ok(code)
	}

	fn read_line(&mut self) -> Result<Code, String> {
		self.read(Self::read_char, |_, (c, _)| c == b'\n')
	}

	fn read_identifier(&mut self) -> Result<Code, String> {
		self.read(Self::peek_char, |code, (c, _)| if c.is_ascii_alphanumeric() || c == b'_' {
			code.read_char_unchecked().unwrap();
			false
		} else {
			true
		})
	}

	fn read_string(&mut self, c: CodeChar) -> Result<Code, String> {
		let mut skip_next = false;
		self.read(|code| {
			let stringc = code.read_char_unchecked();
			if stringc.is_none() {
				Err(error("Unterminated string", c.1, self.filename))
			} else {
				Ok(stringc)
			}
		}, |_, (stringc, _)| {
			if stringc == b'\n' {
				false
			} else if skip_next {
				skip_next = false;
				false
			} else if !skip_next && stringc == c.0 {
				true
			} else {
				if stringc == b'\\' {
					skip_next = true;
				}
				false
			}
		})
	}

	fn skip_comment(&mut self, c: CodeChar, code: Option<&mut Code>) -> Result<bool, String> {
		Ok(if let Some((nc, _)) = self.peek_char()? {
			match nc {
				b'/' => {
					if self.read_until_unchecked(b'\n').is_some() {
						if let Some(code) = code {
							code.push((b'\n', c.1));
						}
					}
					false
				}
				b'*' => {
					self.read_char().unwrap();
					while {
						self.read_until_unchecked(b'*');
						if let Some((fc, _)) = self.read_char_unchecked() {
							fc != b'/'
						} else {
							return Err(error("Unterminated comment", c.1, self.filename))
						}
					} {}
					false
				}
				_ => true
			}
		} else {
			true
		})
	}

	fn read_until_with(
		&mut self,
		end: u8,
		f: impl FnMut(&mut Self) -> Result<Option<CodeChar>, String>
	) -> Result<Option<Code>, String> {
		let mut reached = false;
		let result = self.read(f, |_, (c, _)| {
			if c == end {
				reached = true;
				true
			} else {
				false
			}
		})?;
		Ok(if reached {
			Some(result)
		} else {
			None
		})
	}

	fn read_until_unchecked(&mut self, end: u8) -> Option<Code> {
		self.read_until_with(end, |s| Ok(s.read_char_unchecked())).unwrap()
	}

	fn read_until(&mut self, end: u8) -> Result<Code, String> {
		self.read_until_with(end, Self::read_char)?
			.ok_or_else(|| expected_before(&(end as char).to_string(), "<end>", self.line, self.filename))
	}

	fn skip_block(&mut self) -> Result<(), String> {
		while let Some(c) = self.read_char()? {
			match c.0 {
				b'{' => self.skip_block()?,
				b'}' => return Ok(()),
				b'\'' | b'"' | b'`' => {self.read_string(c)?;}
				b'/' => {self.skip_comment(c, None)?;}
				_ => {}
			}
		}
		Err(expected_before("}", "<end>", self.line, self.filename))
	}

	fn keep_block(&mut self, to_keep: bool) -> Result<u8, String> {
		self.last_if = to_keep;
		if to_keep {
			Ok(1)
		} else {
			self.skip_block()?;
			Ok(0)
		}
	}
}

pub fn read_file<P: AsRef<Path>>(
	path: P,
	filename: &String,
) -> Result<(Vec<(Code, bool)>, PPVars), String>
where
	P: AsRef<OsStr> + Display,
{
	preprocess_code(check!(fs::read(path)), filename)
}

#[allow(clippy::blocks_in_if_conditions)]
pub fn preprocess_code(
	code: Vec<u8>,
	filename: &String,
) -> Result<(Vec<(Code, bool)>, PPVars), String> {
	let mut finalcode = Vec::new();
	let mut currentcode = Code::new();
	let mut code = CodeFile::new(code, filename);
	let mut variables = AHashMap::new();
	let mut pseudos: Option<Vec<VecDeque<u8>>> = None;
	let mut cscope = 0;
	while let Some(c) = code.read_char()? {
		if match c.0 {
			b'@' => {
				let directive = code.read_identifier()?.to_string();
				code.skip_whitespace()?;
				match directive.as_str() {
					"ifos" => {
						let checked_os = code.read_until(b'{')?.trim();
						cscope += code.keep_block(checked_os == env::consts::OS)?;
					}
					"ifdef" => {
						let to_check = code.read_until(b'{')?.trim();
						cscope += code.keep_block(env::var(to_check.to_string()).is_ok())?;
					},
					"ifcmp" => todo!(),
					"if" => todo!(),
					"else" => {
						code.read_until(b'{')?;
						cscope += code.keep_block(!code.last_if)?;
					},
					"else_ifos" => {
						let checked_os = code.read_until(b'{')?.trim();
						cscope += code.keep_block(!code.last_if && checked_os == env::consts::OS)?;
					},
					"else_ifdef" => {
						let to_check = code.read_until(b'{')?.trim();
						cscope += code.keep_block(!code.last_if && env::var(to_check.to_string()).is_ok())?;
					},
					"else_ifcmp" => todo!(),
					"else_if" => todo!(),
					"define" => {
						let name = code.read_identifier()?;
						let value = code.read_line()?;
						variables.insert(name, PPVar::Simple(value.trim()));
					}
					"error" => {
						return Err(error(code.read_line()?.to_string(), c.1, filename))
					}
					"print" => {
						println!("{}", code.read_line()?.to_string());
					}
					_ => return Err(error(format!("Unknown directive '{directive}'"), c.1, filename)),
				}
				false
			}
			b'$' => {
				let mut name = code.read_identifier()?;
				if name.len() <= 1 && matches!(name.last(), Some((b'1'..=b'9', _)) | None) {
					let n = match name.pop() {
						Some((c, _)) => (c - b'0') as usize,
						None => 1,
					};
					if pseudos.is_none() {
						let tocheck = code.code[code.checked..code.read].iter().rev().peekable();
						pseudos = Some(read_pseudos(tocheck));
						code.checked = code.read;
					}
					let pseudos = pseudos.as_ref().unwrap();
					match pseudos.get(pseudos.len() - n) {
						Some(name) => currentcode.append(Code::from((name.iter(), c.1))),
						None => currentcode.append(Code::from(("nil", c.1)))
					}
				} else {
					finalcode.push((currentcode, false));
					name.push_start(c);
					finalcode.push((name, true));
					currentcode = Code::new();
				}
				false
			}
			b'\'' | b'"' | b'`' => {
				currentcode.push(c);
				currentcode.append(code.read_string(c)?);
				true
			}
			b'=' => {
				currentcode.push(c);
				if let Some((nc, _)) = code.peek_char()? {
					if matches!(nc, b'=' | b'>') {
						currentcode.push(code.read_char()?.unwrap());
					} else {
						pseudos = None;
					}
				}
				false
			}
			b'!' | b'>' | b'<' => {
				currentcode.push(c);
				if let Some((nc, _)) = code.peek_char()? {
					if nc == b'=' {
						currentcode.push(code.read_char()?.unwrap());
					}
				}
				false
			}
			b'/' => code.skip_comment(c, Some(&mut currentcode))?,
			b'{' if cscope > 0 => {cscope += 1; true}
			b'}' if cscope > 0 => {cscope -= 1; cscope != 0}
			_ => true,
		} {
			currentcode.push(c)
		}
	}
	if cscope > 0 {
		return Err(expected_before("}", "<end>", code.line, filename))
	}
	if !currentcode.is_empty() {
		finalcode.push((currentcode, false))
	}
	Ok((finalcode, variables))
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

fn read_pseudos(mut code: Peekable<Rev<std::slice::Iter<u8>>>) -> Vec<VecDeque<u8>> {
	let mut newpseudos = Vec::new();
	while {
		if let Some(c) = code.next() {
			if *c == b'=' {
				if let Some(c) = code.next() {
					matches!(c, b'!' | b'=')
				} else {
					return newpseudos;
				}
			} else {
				true
			}
		} else {
			return newpseudos;
		}
	} {}
	skip_whitespace_backwards(&mut code);
	while {
		let mut name = VecDeque::new();
		while {
			if let Some(c) = code.peek() {
				c.is_ascii_alphanumeric() || **c == b'_'
			} else {
				false
			}
		} {
			name.push_front(*code.next().unwrap())
		}
		newpseudos.push(name);
		skip_whitespace_backwards(&mut code);
		if let Some(c) = code.next() {
			*c == b','
		} else {
			false
		}
	} {}
	newpseudos
}

pub fn preprocess_variables(
	stacklevel: u8,
	mut chars: Peekable<Iter<CodeChar>>,
	variables: &PPVars,
	filename: &String,
) -> Result<Code, String> {
	let mut result = Code::new();
	while let Some(c) = chars.next() {
		match c.0 {
			b'$' => {
				let name = {
					let mut name = Code::new();
					while let Some((c, _)) = chars.peek() {
						if !(c.is_ascii_alphanumeric() || *c == b'_') {
							break
						}
						name.push(*chars.next().unwrap())
					}
					name
				};
				if let Ok(value) = env::var(name.to_string()) {
					result.push((b'"', c.1));
					for strc in value.as_bytes() {
						result.push((*strc, c.1));
					}
					result.push((b'"', c.1));
				} else if let Some(PPVar::Simple(value)) = variables.get(&name) {
					if stacklevel == MAX {
						return Err(error("Too many variables called (likely recursive)", c.1, filename));
					}
					result.append(preprocess_variables(
						stacklevel + 1,
						value.into_iter().peekable(),
						variables,
						filename
					)?)
				} else {
					return Err(error(
						format_clue!("Value '", name.to_string(), "' not found"),
						c.1,
						filename,
					));
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
						break
					}
				}
				//return Err(error("Unterminated string", c.1, filename));
			}
			/*'\'' | '"' | '`' => {
				code.push_back((*c, *line));
				while let Some((stringc, stringline)) = chars.next() {
					if *stringc == '\\' {
						if let Some((nextc, nextline)) = chars.peek() {
							if nextc == c {
								code.push_back((*stringc, *stringline));
								code.push_back((*nextc, *nextline));
								chars.next();
								continue;
							}
						}
					}
					code.push_back((*stringc, *stringline));
					if stringc == c {
						break
					}
				}
			}*/
			_ => result.push(*c),
		}
	}
	Ok(result)
}