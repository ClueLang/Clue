use std::{env, iter::Peekable, str, str::Chars};

use crate::format_clue;

pub type LinkedString = std::collections::LinkedList<char>;
type CodeChars<'a, 'b> = &'a mut Peekable<Chars<'b>>;

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

fn expected_before(expected: &str, before: &str, line: usize, filename: &String) -> String {
	error(
		format_clue!("Expected '", expected, "' before '", before, "'"),
		line,
		filename,
	)
}

fn skip_whitespace(chars: CodeChars, line: &mut usize) {
	while let Some(c) = chars.peek() {
		if c.is_whitespace() {
			if *c == '\n' {
				*line += 1;
			}
			chars.next();
		} else {
			break;
		}
	}
}

fn reach(chars: CodeChars, end: char, line: &mut usize, filename: &String) -> Result<(), String> {
	skip_whitespace(chars, line);
	if let Some(c) = chars.next() {
		if end != c {
			Err(expected(&end.to_string(), &c.to_string(), *line, filename))
		} else {
			Ok(())
		}
	} else {
		Err(expected_before(&end.to_string(), "<end>", *line, filename))
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

fn assert_word(chars: CodeChars, line: &mut usize, filename: &String) -> Result<String, String> {
	skip_whitespace(chars, line);
	let word = read_word(chars);
	if word.is_empty() {
		Err(error("Word expected", *line, filename))
	} else {
		Ok(word)
	}
}

fn read_arg(chars: CodeChars, line: &mut usize, filename: &String) -> Result<LinkedString, String> {
	reach(chars, '"', line, filename)?;
	let mut arg = String::new();
	while {
		if let Some(c) = chars.peek() {
			*c != '"'
		} else {
			return Err(expected_before("\"", "<end>", *line, filename))
		}
	} {
		arg.push(chars.next().unwrap())
	}
	chars.next();
	preprocess_code(arg, filename)
}

fn read_block(chars: CodeChars, mut line: usize, filename: &String) -> Result<String, String> {
	reach(chars, '{', &mut line, filename)?;
	let mut block = String::new();
	let mut cscope = 1u8;
	for c in chars.by_ref() {
		block.push(c);
		match c {
			'\n' => line += 1,
			'{' => cscope += 1,
			'}' => {
				cscope -= 1;
				if cscope == 0 {
					block.pop();
					return Ok(block);
				}
			}
			_ => {}
		}
	}
	Err(expected_before("}", "<end>", line, filename))
}

fn keep_block(
	chars: CodeChars,
	code: &mut LinkedString,
	cond: bool,
	line: usize,
	filename: &String,
) -> Result<bool, String> {
	let block = read_block(chars, line, filename)?;
	if cond {
		code.append(&mut preprocess_code(block, filename)?);
	}
	Ok(cond)
}

pub fn preprocess_code(rawcode: String, filename: &String) -> Result<LinkedString, String> {
	let mut code = LinkedString::new();
	let mut line = 1usize;
	let mut prev = false;
	let mut prevline = 1usize;
	let chars = &mut rawcode.chars().peekable();
	while let Some(c) = chars.next() {
		code.push_back(c);
		match c {
			'\n' => {
				for _ in 0..line - prevline {
					code.push_back('\n');
				}
				line += 1;
				prevline = line;
			}
			'@' => {
				code.pop_back();
				let directive = read_word(chars);
				prev = match directive.as_str() {
					"ifos" => {
						let target_os = assert_word(chars, &mut line, filename)?.to_ascii_lowercase();
						keep_block(
							chars,
							&mut code,
							env::consts::OS == target_os,
							line,
							filename,
						)?
					}
					"ifdef" => {
						let var = assert_word(chars, &mut line, filename)?;
						keep_block(chars, &mut code, env::var(var).is_ok(), line, filename)?
					}
					"ifcmp" => {
						let arg1 = read_arg(chars, &mut line, filename)?;
						let condition = assert_word(chars, &mut line, filename)?;
						let arg2 = read_arg(chars, &mut line, filename)?;
						let result = match condition.as_str() {
							"==" => arg1 == arg2,
							"!=" => arg1 != arg2,
							_ => return Err(expected("==", &condition, line, filename))
						};
						keep_block(chars, &mut code, result, line, filename)?
					}
					"else" => keep_block(chars, &mut code, !prev, line, filename)?,
					"" => return Err(error("Expected directive name", line, filename)),
					_ => {
						return Err(error(
							format_clue!("Unknown directive '", directive, "'"),
							line,
							filename,
						))
					}
				}
			}
			'/' => {
				if let Some(nextc) = chars.peek() {
					match *nextc {
						'/' => {
							code.pop_back();
							chars.next();
							while let Some(c) = chars.peek() {
								if *c == '\n' {
									break;
								}
								chars.next();
							}
						}
						'*' => {
							code.pop_back();
							chars.next();
							while {
								let word = assert_word(chars, &mut line, filename);
								word.is_err() || !word.unwrap().ends_with("*/")
							} {
								if chars.peek().is_none() {
									return Err(error("Unterminated comment", line, filename));
								}
							}
						}
						_ => {}
					}
				}
			}
			_ => {}
		}
	}
	Ok(code)
}
