use std::{iter::Peekable, str, str::Chars, env};

use crate::format_clue;

pub type LinkedString = std::collections::LinkedList<char>;
type CodeChars<'a, 'b> = &'a mut Peekable<Chars<'b>>;

fn error(msg: impl Into<String>, line: usize, filename: &String) -> String {
	println!("Error in file \"{filename}\" at line {line}!");
	msg.into()
}

fn expected(expected: &str, got: &str, line: usize, filename: &String) -> String {
	error(format_clue!("Expected '", expected, "', got '", got, "'"), line, filename)
}

fn expected_before(expected: &str, before: &str, line: usize, filename: &String) -> String {
	error(format_clue!("Expected '", expected, "' before '", before, "'"), line, filename)
}

fn skip_whitespace(chars: CodeChars) {
	while {
		if let Some(c) = chars.peek() {
			c.is_whitespace()
		} else {
			false
		}
	} {
		chars.next();
	}
}

fn reach(
	chars: CodeChars,
	end: char,
	line: usize,
	filename: &String
) -> Result<(), String> {
	skip_whitespace(chars);
	if let Some(c) = chars.next() {
		if end != c {
			Err(expected(&end.to_string(), &c.to_string(), line, filename))
		} else {
			Ok(())
		}
	} else {
		Err(expected_before(&end.to_string(), "<end>", line, filename))
	}
}

fn read_word(chars: CodeChars) -> String {
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

fn assert_word(chars: CodeChars, line: usize, filename: &String) -> Result<String, String> {
	skip_whitespace(chars);
	let word = read_word(chars);
	if word.is_empty() {
		Err(error("Word expected", line, filename))
	} else {
		Ok(word)
	}
}

fn read_block(
	chars: CodeChars,
	mut line: usize,
	filename: &String
) -> Result<String, String> {
	reach(chars, '{', line, filename)?;
	let mut block = String::new();
	let mut cscope = 1u8;
	while let Some(c) = chars.next() {
		block.push(c);
		match c {
			'\n' => line += 1,
			'{' => cscope += 1,
			'}' => {
				cscope -= 1;
				if cscope == 0 {
					block.pop();
					return Ok(block)
				}
			}
			_ => {}
		}
	}
	Err(expected_before("}", "<end>", line, filename))
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
				code.pop_back();
				let directive = read_word(chars);
				match directive.as_str() {
					"ifdef" => {
						let var = assert_word(chars, line, filename)?;
						let block = read_block(chars, line, filename)?;
						if env::var(var).is_ok() {
							code.append(&mut preprocess_code(block, filename)?);
						}
					}
					"" => return Err(error("Expected directive name", line, filename)),
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