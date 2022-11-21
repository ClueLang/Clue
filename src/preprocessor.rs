use std::{iter::Peekable, str::Chars};

use crate::format_clue;

pub type LinkedString = std::collections::LinkedList<char>;

fn error(msg: String, line: usize, filename: &String) -> String {
	println!("Error in file \"{filename}\" at line {line}!");
	msg
}

fn read_word(chars: &mut Peekable<Chars>) -> String {
	let mut word = String::new();
	while {
		if let Some(c) = chars.peek() {
			!c.is_whitespace()
		} else {
			false
		}
	} {
		word.push(chars.next().unwrap())
	}
	word
}

pub fn preprocess_code(rawcode: String, filename: &String) -> Result<LinkedString, String> {
	let mut code = LinkedString::new();
	let mut line = 1usize;
	let chars = &mut rawcode.chars().peekable();
	while let Some(c) = chars.next() {
		code.push_back(c);
		match c {
			'\n' => line += 1,
			'@' => {
				let directive = read_word(chars);
				match directive {
					_ => return Err(error(
						format_clue!("Unknown directive '", directive, "'"),
						line, filename
					))
				}
			},
			_ => continue,
		}
	}
	Ok(code)
}