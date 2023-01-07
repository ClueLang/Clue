use std::{
	collections::linked_list::{LinkedList, Iter, IntoIter},
	hash::Hash, ffi::OsString,
};

use utf8_decode::Decoder;

pub type CodeChar = (u8, usize);

#[derive(Debug, Clone, Default)]
pub struct Code {
	list: LinkedList<CodeChar>
}

pub struct CodeBytes {
	code: Code,
}

impl Iterator for CodeBytes {
	type Item = u8;

	fn next(&mut self) -> Option<Self::Item> {
		self.code.pop_start().map(|(c, _)| c)
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
			result.push(c.unwrap_or('\u{FFFD}'));
		}
		result
	}
}

impl<'a> From<(std::collections::vec_deque::Iter<'a, u8>, usize)> for Code {
	fn from(value: (std::collections::vec_deque::Iter<u8>, usize)) -> Self {
		let (iter, line) = value;
		let mut result = Code::new();
		for c in iter {
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
		self.len() == other.len() && {
			let mut other = other.into_iter();
			for (c, _) in self {
				if *c != other.next().unwrap().0 {
					return false;
				}
			}
			true
		}
	}
}

impl PartialEq<&str> for Code {
	fn eq(&self, other: &&str) -> bool {
		self.len() == other.len() && {
			let mut other = other.bytes();
			for (c, _) in self {
				if *c != other.next().unwrap() {
					return false;
				}
			}
			true
		}
	}
}

impl PartialEq<OsString> for Code {
	fn eq(&self, other: &OsString) -> bool {
		self.len() == other.len() && {
			let other = other.clone().into_string().unwrap();
			let mut other = other.bytes();
			for (c, _) in self {
				if *c != other.next().unwrap() {
					return false;
				}
			}
			true
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

	pub fn bytes(self) -> CodeBytes {
		CodeBytes { code: self }
	}

	pub fn chars(self) -> Decoder<CodeBytes> {
		Decoder::new(self.bytes())
	}

	pub fn trim(mut self) -> Self {
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
		self
	}
}