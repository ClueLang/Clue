use std::{
	collections::{
		vec_deque::{IntoIter, Iter},
		VecDeque,
	},
	ffi::OsString,
	hash::Hash,
};

use utf8_decode::decode;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

pub type CodeChar = (u8, usize, usize);

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct Code {
	list: VecDeque<CodeChar>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CodeBytes {
	code: Code,
	line: usize,
	column: usize,
	read: usize,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub struct CodeChars {
	code: CodeBytes,
}

impl Iterator for CodeBytes {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		self.code.pop_start().map(|(c, line, column)| {
			self.read += 1;
			self.line = line;
			self.column = column;
			c
		})
	}
}

impl Iterator for CodeChars {
	type Item = char;

	fn next(&mut self) -> Option<Self::Item> {
		match decode(&mut self.code) {
			None => None,
			Some(Err(_)) => Some('\u{FFFD}'),
			Some(Ok(c)) => Some(c),
		}
	}
}

impl CodeChars {
	pub fn next_unwrapped(&mut self) -> char {
		self.next().unwrap_or('\0')
	}

	pub const fn line(&self) -> usize {
		self.code.line
	}

	pub const fn column(&self) -> usize {
		self.code.column
	}

	pub fn bytes_read(&mut self) -> usize {
		let read = self.code.read;
		self.code.read = 0;
		read
	}
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
		let mut result = String::with_capacity(self.len());
		for c in self.clone().chars() {
			result.push(c);
		}
		result
	}
}

impl<'a> From<(&'a [u8], usize, usize)> for Code {
	fn from(value: (&'a [u8], usize, usize)) -> Self {
		let (iter, line, mut column) = value;
		let mut result = Code::with_capacity(iter.len());
		for c in iter {
			result.push((*c, line, column));
			column += 1;
		}
		result
	}
}

impl<'a, const N: usize> From<(&'a [u8; N], usize, usize)> for Code {
	fn from(value: (&'a [u8; N], usize, usize)) -> Self {
		Code::from((value.0 as &[u8], value.1, value.2))
	}
}

impl<'a> From<(&'a str, usize, usize)> for Code {
	fn from(value: (&'a str, usize, usize)) -> Self {
		Code::from((value.0.as_bytes(), value.1, value.2))
	}
}

impl From<(String, usize, usize)> for Code {
	fn from(value: (String, usize, usize)) -> Self {
		Code::from((value.0.as_bytes(), value.1, value.2))
	}
}

impl PartialEq for Code {
	fn eq(&self, other: &Self) -> bool {
		self.len() == other.len() && {
			let mut other = other.into_iter();
			for (c, ..) in self {
				if *c != other.next().unwrap().0 {
					return false;
				}
			}
			true
		}
	}
}

impl PartialEq<str> for Code {
	fn eq(&self, other: &str) -> bool {
		self.len() == other.len() && {
			let mut other = other.bytes();
			for (c, ..) in self {
				if *c != other.next().unwrap() {
					return false;
				}
			}
			true
		}
	}
}

impl PartialEq<&str> for Code {
	fn eq(&self, other: &&str) -> bool {
		self == *other
	}
}

impl PartialEq<String> for Code {
	fn eq(&self, other: &String) -> bool {
		self == other.as_str()
	}
}

impl PartialEq<OsString> for Code {
	fn eq(&self, other: &OsString) -> bool {
		*self == other.clone().into_string().unwrap()
	}
}

impl Eq for Code {}

impl Hash for Code {
	fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
		for (c, ..) in &self.list {
			c.hash(state)
		}
	}
}

impl Code {
	pub fn new() -> Self {
		Self {
			list: VecDeque::new(),
		}
	}

	pub fn with_capacity(capacity: usize) -> Self {
		Self {
			list: VecDeque::with_capacity(capacity),
		}
	}

	pub fn append(&mut self, mut other: Self) {
		self.list.append(&mut other.list)
	}

	pub fn is_empty(&self) -> bool {
		self.list.is_empty()
	}

	pub fn len(&self) -> usize {
		self.list.len()
	}

	pub fn last(&self) -> Option<&CodeChar> {
		self.list.back()
	}

	pub fn push(&mut self, c: CodeChar) {
		self.list.push_back(c);
	}

	pub fn pop(&mut self) -> Option<CodeChar> {
		self.list.pop_back()
	}

	pub fn push_start(&mut self, c: CodeChar) {
		self.list.push_front(c)
	}
	pub fn pop_start(&mut self) -> Option<CodeChar> {
		self.list.pop_front()
	}

	pub fn iter(&self) -> Iter<CodeChar> {
		self.list.iter()
	}

	pub const fn bytes(self) -> CodeBytes {
		CodeBytes {
			code: self,
			line: 0,
			column: 0,
			read: 0,
		}
	}

	pub const fn chars(self) -> CodeChars {
		CodeChars { code: self.bytes() }
	}

	pub fn trim(mut self) -> Self {
		while let Some((c, ..)) = self.list.front() {
			if c.is_ascii_whitespace() {
				self.list.pop_front();
			} else {
				break;
			}
		}
		while let Some((c, ..)) = self.list.back() {
			if c.is_ascii_whitespace() {
				self.list.pop_back();
			} else {
				break;
			}
		}
		self
	}
}
