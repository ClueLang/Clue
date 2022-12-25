use crate::{format_clue, scanner::CharExt};
use ahash::AHashMap;
use std::{
	collections::linked_list::Iter,
	env,
	iter::{Peekable, Rev},
	str,
	str::Chars,
};

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

fn assert_name(chars: CodeChars, line: &mut usize, filename: &String) -> Result<String, String> {
	let name = assert_word(chars, line, filename)?;
	if name.contains('=') {
		return Err(error(
			"The value's name cannot contain '='",
			*line,
			filename,
		));
	}
	Ok(name)
}

fn read_until(
	chars: CodeChars,
	end: char,
	line: &mut usize,
	filename: &String,
) -> Result<String, String> {
	let arg = read_with(chars, |c| *c != end);
	if chars.next().is_none() {
		return Err(expected_before(&end.to_string(), "<end>", *line, filename));
	}
	Ok(arg)
}

fn read_arg(
	chars: CodeChars,
	line: &mut usize,
	filename: &String,
) -> Result<(LinkedString, bool), String> {
	reach(chars, '"', line, filename)?;
	let mut rawarg = read_until(chars, '"', line, filename)?;
	rawarg.retain(|c| !matches!(c, '\r' | '\n' | '\t'));
	let (arg, result) = preprocess_code(rawarg, None, AHashMap::new(), line, filename)?;
	Ok((arg, result))
}

fn read_block(
	chars: CodeChars,
	line: &mut usize,
	filename: &String,
) -> Result<(usize, String), String> {
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
		preprocess_code(block, None, AHashMap::new(), &mut line, filename)?.0
	} else {
		let mut lines = LinkedString::new();
		for _ in 0..block.matches('\n').count() {
			lines.push_back('\n');
		}
		lines
	});
	Ok(cond)
}

fn skip_whitespace_backwards(code: &mut Peekable<Rev<Iter<char>>>) {
	while let Some(c) = code.peek() {
		if c.is_whitespace() {
			code.next();
		} else {
			break;
		}
	}
}

fn read_pseudos(mut code: Peekable<Rev<Iter<char>>>) -> Vec<LinkedString> {
	let mut newpseudos: Vec<LinkedString> = Vec::new();
	while {
		if let Some(c) = code.next() {
			if *c == '=' {
				if let Some(c) = code.next() {
					matches!(c, '!' | '=')
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
		let mut name = LinkedString::new();
		while {
			if let Some(c) = code.peek() {
				c.is_identifier()
			} else {
				false
			}
		} {
			name.push_front(*code.next().unwrap())
		}
		newpseudos.push(name);
		skip_whitespace_backwards(&mut code);
		if let Some(c) = code.next() {
			*c == ','
		} else {
			false
		}
	} {}
	newpseudos
}

pub fn to_preprocess(code: &str) -> bool {
	let mut code = code.as_bytes().iter();
	while let Some(c) = code.next() {
		match *c as char {
			'\\' => {
				code.next();
			}
			'$' | '@' => return true,
			_ => {}
		}
	}
	false
}

pub fn preprocess_code(
	rawcode: String,
	mut pseudos: Option<Vec<LinkedString>>,
	mut values: AHashMap<String, LinkedString>,
	line: &mut usize,
	filename: &String,
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
						keep_block(
							chars,
							&mut code,
							env::consts::OS == target_os,
							line,
							filename,
						)?
					}
					"ifdef" => {
						let var = assert_word(chars, line, filename)?;
						let conditon = values.contains_key(&var) || env::var(var).is_ok();
						keep_block(chars, &mut code, conditon, line, filename)?
					}
					"ifcmp" => {
						let arg1 = read_arg(chars, line, filename)?.0;
						let condition = assert_word(chars, line, filename)?;
						let arg2 = read_arg(chars, line, filename)?.0;
						let result = match condition.as_str() {
							"==" => arg1 == arg2,
							"!=" => arg1 != arg2,
							_ => return Err(expected("==", &condition, *line, filename)),
						};
						keep_block(chars, &mut code, result, line, filename)?
					}
					"if" => todo!(),
					"else" => keep_block(chars, &mut code, !prev, line, filename)?,
					"define" => {
						let name = assert_name(chars, line, filename)?;
						let value = read_arg(chars, line, filename)?.0;
						values.insert(name, value);
						true
					}
					"undef" => {
						let name = assert_name(chars, line, filename)?;
						values.remove(&name).is_some()
					}
					"error" => {
						let msg = read_arg(chars, line, filename)?.0;
						return Err(error(msg.iter().collect::<String>(), *line, filename));
					}
					"warning" => {
						let (msg, result) = read_arg(chars, line, filename)?;
						println!("Warning: \"{}\"", msg.iter().collect::<String>());
						result
					}
					"print" => {
						let (msg, result) = read_arg(chars, line, filename)?;
						println!("{}", msg.iter().collect::<String>());
						result
					}
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
				let name = {
					let name = read_with(chars, char::is_identifier);
					if name.is_empty() {
						String::from("1")
					} else {
						name
					}
				};
				if let Ok(index) = name.parse::<usize>() {
					if pseudos.is_none() {
						pseudos = Some(read_pseudos(code.iter().rev().peekable()));
					}
					let pseudos = pseudos.as_ref().unwrap();
					let mut var = pseudos
						.get(pseudos.len() - index)
						.cloned()
						.unwrap_or_else(|| LinkedString::from(['n', 'i', 'l']));
					code.append(&mut var);
				} else {
					let mut value = if let Some(value) = values.get(&name) {
						value.clone()
					} else if let Ok(value) = env::var(&name) {
						value.chars().collect()
					} else {
						return Err(error(
							format_clue!("Value '", name, "' not found"),
							*line,
							filename,
						));
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
			'\\' => {
				code.push_back(if let Some(nc) = chars.peek() {
					if matches!(nc, '@' | '$') {
						chars.next().unwrap()
					} else {
						'\\'
					}
				} else {
					'\\'
				});
			}
			'=' => {
				code.push_back('=');
				if let Some(nc) = chars.peek() {
					if matches!(nc, '=' | '>') {
						code.push_back(chars.next().unwrap());
					} else {
						pseudos = None;
					}
				}
			}
			'!' | '>' | '<' => {
				code.push_back(c);
				if let Some(nc) = chars.peek() {
					if *nc == '=' {
						code.push_back(chars.next().unwrap());
					}
				}
			}
			_ => code.push_back(c),
		}
	}
	Ok((code, prev))
}
