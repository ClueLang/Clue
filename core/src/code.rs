//! This module contains the [`Code`] data structure and its iterators.
//!
//! The [`Code`] data structure is used to store the characters and their positions in the source code
//! also serves the purpose of efficient insertion and removal of characters in the preprocessor stage.

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

/// A simple tuple for storing the position (line and column) of a character.
pub type Position = (usize, usize);

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A data structure used for storing a single character and its position.
pub struct CodeChar {
	pub value: u8,
	pub position: Position
}

impl From<(u8, Position)> for CodeChar {
	fn from(value: (u8, Position)) -> Self {
		CodeChar {
			value: value.0,
			position: value.1,
		}
	}
}

#[derive(Debug, Clone, Default)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A data structure used for storing characters and their positions.
/// Internally it uses [`VecDeque`] for efficient insertion and removal.
pub struct Code {
	list: VecDeque<CodeChar>,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// An iterator over the bytes in a [`Code`].
/// This iterator will return each individual byte as a [`u8`]
pub struct CodeBytes {
	code: Code,
	line: usize,
	column: usize,
	read: usize,
}

#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// An iterator over the characters in a [`Code`].
/// This iterator will return each individual character as a [`char`]
/// If the character is invalid, it will return the Unicode replacement character (U+FFFD)
pub struct CodeChars {
	code: CodeBytes,
}

impl Iterator for CodeBytes {
	type Item = u8;

	/// Returns an [`Option`] with the next byte in the [`Code`].
	fn next(&mut self) -> Option<Self::Item> {
		self.code.pop_start().map(|CodeChar { value, position: (line, column)}| {
			self.read += 1;
			self.line = line;
			self.column = column;
			value
		})
	}
}

impl Iterator for CodeChars {
	type Item = char;

	/// Advances the iterator and returns the next value.
	///
	/// Returns [`None`] when iteration is finished.
	/// If the next character is invalid, it will return the Unicode replacement character (U+FFFD)
	fn next(&mut self) -> Option<Self::Item> {
		match decode(&mut self.code) {
			None => None,
			Some(Err(_)) => Some('\u{FFFD}'), // U+FFFD Unicode replacement character
			Some(Ok(c)) => Some(c),
		}
	}
}

impl CodeChars {
	/// Returns the next character in the [`Code`].
	/// If there are no characters left, it will return a null charachter ('\0')
	pub fn next_unwrapped(&mut self) -> char {
		self.next().unwrap_or('\0')
	}

	/// Returns the line the last character was on.
	pub const fn line(&self) -> usize {
		self.code.line
	}

	/// Returns the column the last character was on.
	pub const fn column(&self) -> usize {
		self.code.column
	}

	/// Returns the number of bytes read by the iterator since the last time this function was called.
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

impl Into<String> for Code {
	fn into(self) -> String {
		self.to_string()
	}
}

impl Into<String> for &Code {
	fn into(self) -> String {
		self.to_string()
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

impl<'a> From<(&'a [u8], Position)> for Code {
	fn from(value: (&'a [u8], Position)) -> Self {
		let (iter, (line, mut column)) = value;
		let mut result = Code::with_capacity(iter.len());
		for c in iter {
			result.push((*c, (line, column)).into());
			column += 1;
		}
		result
	}
}

impl<'a, const N: usize> From<(&'a [u8; N], Position)> for Code {
	fn from(value: (&'a [u8; N], Position)) -> Self {
		Code::from((value.0 as &[u8], value.1))
	}
}

impl<'a> From<(&'a str, Position)> for Code {
	fn from(value: (&'a str, Position)) -> Self {
		Code::from((value.0.as_bytes(), value.1))
	}
}

impl From<(String, Position)> for Code {
	fn from(value: (String, Position)) -> Self {
		Code::from((value.0.as_bytes(), value.1))
	}
}

impl PartialEq for Code {
	fn eq(&self, other: &Self) -> bool {
		self.len() == other.len() && {
			let mut other = other.into_iter();
			for CodeChar { value, .. } in self {
				if *value != other.next().unwrap().value {
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
			for CodeChar { value, .. } in self {
				if *value != other.next().unwrap() {
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
		for CodeChar { value, .. } in &self.list {
			value.hash(state)
		}
	}
}

impl Code {
	/// Creates a new [`Code`] with no characters.
	/// This is equivalent to [`VecDeque::new`].
	pub fn new() -> Self {
		Self {
			list: VecDeque::new(),
		}
	}

	/// Creates a new [`Code`] with the given capacity.
	/// This is equivalent to [`VecDeque::with_capacity`].
	pub fn with_capacity(capacity: usize) -> Self {
		Self {
			list: VecDeque::with_capacity(capacity),
		}
	}

	/// Appends the given [`Code`] to the end of this [`Code`].
	/// This is equivalent to [`VecDeque::append`].
	pub fn append(&mut self, mut other: Self) {
		self.list.append(&mut other.list)
	}

	/// Returns `true` if the [`Code`] has no characters.
	/// This is equivalent to [`VecDeque::is_empty`].
	pub fn is_empty(&self) -> bool {
		self.list.is_empty()
	}

	/// Returns the number of characters in the [`Code`].
	/// This is equivalent to [`VecDeque::len`].
	pub fn len(&self) -> usize {
		self.list.len()
	}

	/// Returns the first character in the [`Code`].
	/// This is equivalent to [`VecDeque::front`].
	pub fn first(&self) -> Option<&CodeChar> {
		self.list.front()
	}

	/// Returns the last character in the [`Code`].
	/// This is equivalent to [`VecDeque::back`].
	pub fn last(&self) -> Option<&CodeChar> {
		self.list.back()
	}

	/// Pushes the given character to the end of the [`Code`].
	/// This is equivalent to [`VecDeque::push_back`].
	pub fn push(&mut self, c: CodeChar) {
		self.list.push_back(c);
	}

	/// Pops the last character from the [`Code`].
	/// This is equivalent to [`VecDeque::pop_back`].
	pub fn pop(&mut self) -> Option<CodeChar> {
		self.list.pop_back()
	}

	/// Pushes the given character to the start of the [`Code`].
	/// This is equivalent to [`VecDeque::push_front`].
	pub fn push_start(&mut self, c: CodeChar) {
		self.list.push_front(c)
	}

	/// Pops the first character from the [`Code`].
	/// This is equivalent to [`VecDeque::pop_front`].
	pub fn pop_start(&mut self) -> Option<CodeChar> {
		self.list.pop_front()
	}

	/// Returns an iterator over the characters in the [`Code`] without consuming it.
	pub fn iter(&self) -> Iter<CodeChar> {
		self.list.iter()
	}

	/// Returns an iterator over the bytes in the [`Code`] consuming it.
	pub const fn bytes(self) -> CodeBytes {
		CodeBytes {
			code: self,
			line: 0,
			column: 0,
			read: 0,
		}
	}

	/// Returns an iterator over the characters in the [`Code`] consuming it.
	pub const fn chars(self) -> CodeChars {
		CodeChars { code: self.bytes() }
	}

	/// Trims whitespaces from the start of the [`Code`].
	pub fn trim_start(mut self) -> Self {
		while let Some(CodeChar { value, .. }) = self.list.front() {
			if value.is_ascii_whitespace() {
				self.list.pop_front();
			} else {
				break;
			}
		}
		self
	}

	/// Trims whitespaces from the end of the [`Code`].
	pub fn trim_end(mut self) -> Self {
		while let Some(CodeChar { value, .. }) = self.list.back() {
			if value.is_ascii_whitespace() {
				self.list.pop_back();
			} else {
				break;
			}
		}
		self
	}

	/// Trims whitespaces from the start and end of the [`Code`].
	pub fn trim(self) -> Self {
		self.trim_start().trim_end()
	}

	/// Provides a reference to the character at the given index.
	pub fn get(&self, index: usize) -> Option<&CodeChar> {
		self.list.get(index)
	}
}
