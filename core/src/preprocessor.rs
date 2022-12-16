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

fn read_with(chars: CodeChars, mut f: impl FnMut(&char) -> bool) -> String {
	let mut result = String::new();
	while {
		if let Some(c) = chars.peek() {
			f(c)
		} else {
			false
		}
	} {
		result.push(chars.next().unwrap())
	}
	result
}

fn read_word(chars: CodeChars) -> String {
	read_with(chars, |c| !c.is_whitespace())
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

fn read_until(chars: CodeChars, end: char, line: &mut usize, filename: &String) -> Result<String, String> {
	let arg = read_with(chars, |c| *c != end);
	if chars.next().is_none() {
		return Err(expected_before(&end.to_string(), "<end>", *line, filename))
	}
	Ok(arg)
}

fn read_arg(chars: CodeChars, line: &mut usize, filename: &String) -> Result<(LinkedString, bool), String> {
	reach(chars, '"', line, filename)?;
	preprocess_code(read_until(chars, '"', line, filename)?, Vec::new(), line, filename)
}

fn read_block(chars: CodeChars, line: &mut usize, filename: &String) -> Result<(usize, String), String> {
	reach(chars, '{', line, filename)?;
	let mut block = String::new();
	let mut cscope = 1u8;
	for c in chars.by_ref() {
		block.push(c);
		match c {
			'{' => cscope += 1,
			'}' => {
				cscope -= 1;
				if cscope == 0 {
					block.pop();
					return Ok((*line, block));
				}
			}
			_ => {}
		}
	}
	Err(expected_before("}", "<end>", *line, filename))
}

fn keep_block(
	chars: CodeChars,
	code: &mut LinkedString,
	cond: bool,
	line: &mut usize,
	filename: &String,
) -> Result<bool, String> {
	let (mut line, block) = read_block(chars, line, filename)?;
	code.append(&mut if cond {
		preprocess_code(block, Vec::new(), &mut line, filename)?.0
	} else {
		let mut lines = LinkedString::new();
		for _ in 0..block.matches('\n').count() {
			lines.push_back('\n');
		}
		lines
	});
	Ok(cond)
}

pub fn to_preprocess(code: &str) -> bool {
	code.contains('@') || code.contains('$')
}

pub fn preprocess_code(
	rawcode: String,
	pseudos: Vec<LinkedString>,
	line: &mut usize,
	filename: &String
) -> Result<(LinkedString, bool), String> {
	let mut code = LinkedString::new();
	let mut prev = true;
	let mut prevline = *line;
	let chars = &mut rawcode.chars().peekable();
	while let Some(c) = chars.next() {
		match c {
			'\n' => {
				for _ in 0..=*line - prevline {
					code.push_back('\n');
				}
				*line += 1;
				prevline = *line;
			}
			'@' => {
				let directive = read_word(chars);
				prev = match directive.as_str() {
					"ifos" => {
						let target_os = assert_word(chars, line, filename)?.to_ascii_lowercase();
						keep_block(chars, &mut code, env::consts::OS == target_os, line, filename)?
					}
					"ifdef" => {
						let var = assert_word(chars, line, filename)?;
						keep_block(chars, &mut code, env::var(var).is_ok(), line, filename)?
					}
					"ifcmp" => {
						let arg1 = read_arg(chars, line, filename)?;
						let condition = assert_word(chars, line, filename)?;
						let arg2 = read_arg(chars, line, filename)?;
						let result = match condition.as_str() {
							"==" => arg1 == arg2,
							"!=" => arg1 != arg2,
							_ => return Err(expected("==", &condition, *line, filename))
						};
						keep_block(chars, &mut code, result, line, filename)?
					}
					"if" => todo!(),
					"else" => keep_block(chars, &mut code, !prev, line, filename)?,
					"define" => {
						let name = assert_word(chars, line, filename)?;
						if name.contains('=') {
							return Err(error("The value's name cannot contain '='", *line, filename));
						} else if name.ends_with('!') {
							return Err(error("The value's name cannot end with '!'", *line, filename));
						}
						let mut value = read_arg(chars, line, filename)?.0.iter().collect::<String>();
						value.retain(|c| !matches!(c, '\r' | '\n' | '\t'));
						env::set_var(format_clue!("_CLUE_", name), value);
						true
					},
					"error" => {
						let msg = read_arg(chars, line, filename)?.0.iter().collect::<String>();
						return Err(error(&msg, *line, filename))
					},
					"warning" => {
						let (msg, result) = read_arg(chars, line, filename)?;
						println!("Warning: \"{}\"", msg.iter().collect::<String>());
						result
					},
					"print" => {
						let (msg, result) = read_arg(chars, line, filename)?;
						println!("{}", msg.iter().collect::<String>());
						result
					},
					"execute" => todo!(),
					"eval" => todo!(),
					"include" => todo!(),
					"macro" => todo!(),
					"" => return Err(error("Expected directive name", *line, filename)),
					_ => {
						return Err(error(
							format_clue!("Unknown directive '", directive, "'"),
							*line,
							filename,
						))
					}
				};
			}
			'$' => {
				let name = read_with(chars, |c| c.is_ascii_alphanumeric() || *c == '_');
				if name.is_empty() {
					return Err(error("Expected '<name>'", *line, filename))
				} else if let Ok(index) = name.parse::<usize>() {
					let mut var = pseudos.get(index)
						.cloned()
						.unwrap_or_else(|| LinkedString::from(['n', 'i', 'l']));
					code.append(&mut var);
				} else {
					let mut value = {
						let value = if let Ok(value) = env::var(&name) {
							value
						} else if let Ok(value) = env::var(format_clue!("_CLUE_", name)) {
							value
						} else {
							return Err(error(
								format_clue!("Value '", name, "' not found"),
								*line,
								filename
							));
						};
						if to_preprocess(&value) {
							preprocess_code(value, Vec::new(), line, filename)?.0
						} else {
							value.chars().collect()
						}
					};
					code.append(&mut value);
				}
			}
			'/' => {
				if let Some(nextc) = chars.peek() {
					match *nextc {
						'/' => {
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
								let word = assert_word(chars, line, filename);
								word.is_err() || !word.unwrap().ends_with("*/")
							} {
								if chars.peek().is_none() {
									return Err(error("Unterminated comment", *line, filename));
								}
							}
						}
						_ => code.push_back('/'),
					}
				}
			}
			_ => code.push_back(c),
		}
	}
	Ok((code, prev))
}
