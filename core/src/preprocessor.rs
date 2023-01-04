use crate::{format_clue, check};
use ahash::AHashMap;
use std::{
	collections::{linked_list::{IntoIter, Iter}, LinkedList, VecDeque},
	env,
	iter::{Peekable, Rev},
	str,
	path::Path,
	ffi::OsStr,
	fmt::Display,
	fs, hash::Hash, u8::MAX,
};

pub type CodeChar = (u8, usize);
pub type PPVars = AHashMap<Code, PPVar>;

#[derive(Clone)]
pub enum PPVar {
	Simple(Code)
}

fn error(msg: impl Into<String>, line: usize, filename: &String) -> String {
	println!("Error in file \"{filename}\" at line {line}!");
	msg.into()
}

fn expected(expected: &str, got: &str, line: usize, filename: &String) -> String {
	error(
		format_clue!("Expected '", expected, "', got '", got, "'"),
		line,
		filename,
	)
}

#[derive(Debug, Clone)]
pub struct Code {
	pub list: LinkedList<CodeChar>
}

impl<'a> IntoIterator for &'a Code {
    type Item = &'a CodeChar;
    type IntoIter = Iter<'a, CodeChar>;

    fn into_iter(self) -> Iter<'a, CodeChar> {
        self.list.iter()
    }
}

impl IntoIterator for Code {
    type Item = CodeChar;
    type IntoIter = IntoIter<CodeChar>;

    fn into_iter(self) -> IntoIter<CodeChar> {
        self.list.into_iter()
    }
}

impl ToString for Code {
	fn to_string(&self) -> String {
		let mut result = String::new();
        for (c, _) in self {
            result.push(*c as char)
        }
        result
	}
}

impl<'a> From<(std::collections::vec_deque::Iter<'a, u8>, usize)> for Code {
	fn from(value: (std::collections::vec_deque::Iter<u8>, usize)) -> Self {
		let (mut iter, line) = value;
		let mut result = Code::new();
		while let Some(c) = iter.next() {
			result.push((*c, line))
		}
		result
	}
}

impl<'a> From<(&'a str, usize)> for Code {
	fn from(value: (&'a str, usize)) -> Self {
		let (string, line) = value;
		let mut result = Code::new();
		for c in string.bytes() {
			result.push((c, line))
		}
		result
	}
}

impl PartialEq for Code {
	fn eq(&self, other: &Self) -> bool {
		let mut eq = false;
		self.len() == other.len() && {
			let mut other = other.into_iter();
			for (c, _) in self {
				eq = *c == other.next().unwrap().0
			}
			eq
		}
	}
}

impl Eq for Code {}

impl Hash for Code {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		for (c, _) in &self.list {
			c.hash(state)
		}
	}
}

impl Code {
	pub fn new() -> Self {
		Self { list: LinkedList::new() }
	}

	pub fn append(&mut self, mut other: Code) {
		self.list.append(&mut other.list)
	}

	fn is_empty(&self) -> bool {
		self.list.is_empty()
	}

	pub fn len(&self) -> usize {
		self.list.len()
	}

	fn last(&self) -> Option<&CodeChar> {
		self.list.back()
	}

	fn push(&mut self, c: CodeChar) {
		self.list.push_back(c);
	}

	fn pop(&mut self) -> Option<CodeChar> {
		self.list.pop_back()
	}

	fn trim(&mut self) {
		while let Some((c, _)) = self.list.front() {
			if c.is_ascii_whitespace() {
				self.list.pop_front();
			} else {
				break
			}
		}
		while let Some((c, _)) = self.list.back() {
			if c.is_ascii_whitespace() {
				self.list.pop_back();
			} else {
				break
			}
		}
	}
}

struct CodeFile<'a> {
	code: Vec<u8>,
	checked: usize,
	read: usize,
	peeked: Option<CodeChar>,
	line: usize,
	filename: &'a String
}

impl<'a> CodeFile<'a> {
	fn new(code: Vec<u8>, filename: &'a String) -> Self {
		Self {
			code,
			checked: 0,
			read: 0,
			peeked: None,
			line: 1,
			filename
		}
	}

	fn is_ascii(&self, c: Option<CodeChar>) -> Result<Option<CodeChar>, String> {
		match c {
			None => Ok(None),
			Some(c) if c.0.is_ascii() => Ok(Some(c)),
			Some((c, line)) => Err(error(format!("Invalid character '{c}'"), line, self.filename))
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
		} else {
			if let Some(c) = self.code.get(self.read).copied() {
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

	fn read_identifier(&mut self) -> Result<Code, String> {
		self.read(Self::peek_char, |code, (c, _)| if c.is_ascii_alphanumeric() || c == b'_' {
			code.read_char_unchecked().unwrap();
			false
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
			.ok_or_else(|| expected(&end.to_string(), "<end>", self.line, self.filename))
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

pub fn preprocess_code(
	code: Vec<u8>,
	filename: &String,
) -> Result<(Vec<(Code, bool)>, PPVars), String> {
	let mut finalcode = Vec::new();
	let mut currentcode = Code::new();
	let mut code = CodeFile::new(code, filename);
	let mut variables = AHashMap::new();
	let mut pseudos: Option<Vec<VecDeque<u8>>> = None;
	while let Some(c) = code.read_char()? {
		if match c.0 {
			b'@' => {
				let directive = code.read_until(b' ')?;
				match directive.to_string().as_str() {
					"define" => {
						code.skip_whitespace()?;
						let name = code.read_identifier()?;
						let mut value = code.read_until(b'\n')?;
						value.trim();
						variables.insert(name, PPVar::Simple(value));
					}
					_ => {
						currentcode.push(c);
						currentcode.append(directive)
					},
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
					currentcode = Code::new();
					currentcode.push(c);
					currentcode.append(name);
					finalcode.push((currentcode, true));
					currentcode = Code::new();
				}
				false
			}
			b'\'' | b'"' | b'`' => {
				currentcode.push(c);
				let mut skip_next = false;
				currentcode.append(code.read(|code| {
					let stringc = code.read_char()?;
					if stringc.is_none() {
						Err(error("Unterminated string", c.1, filename))
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
				})?);
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
			b'/' => {
				if let Some((nc, _)) = code.peek_char()? {
					match nc {
						b'/' => {
							if let Some(_) = code.read_until_unchecked(b'\n') {
								currentcode.push((b'\n', c.1));
							}
							false
						}
						b'*' => {
							code.read_char().unwrap();
							while {
								code.read_until_unchecked(b'*');
								if let Some((fc, _)) = code.read_char_unchecked() {
									fc != b'/'
								} else {
									return Err(error("Unterminated comment", c.1, filename))
								}
							} {}
							false
						}
						_ => true
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
				if let Ok(value) = env::var(&name.to_string()) {
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