use std::{env, iter::Peekable, str, str::Chars};

use crate::format_clue;

pub type LinkedString = std::collections::LinkedList<char>;
type CodeChars<'a, 'b> = &'a mut Peekable<Chars<'b>>;
type Line<'a> = &'a mut usize;
type PreProcessResult = (LinkedString, bool);

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

fn skip_whitespace(chars: CodeChars, line: Line) {
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

fn reach(chars: CodeChars, end: char, line: Line, filename: &String) -> Result<(), String> {
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

fn assert_word(chars: CodeChars, line: Line, filename: &String) -> Result<String, String> {
	skip_whitespace(chars, line);
	let word = read_word(chars);
	if word.is_empty() {
		Err(error("Word expected", *line, filename))
	} else {
		Ok(word)
	}
}

fn read_until(chars: CodeChars, end: char, line: Line, filename: &String) -> Result<String, String> {
	let mut arg = String::new();
	while {
		if let Some(c) = chars.peek() {
			*c != end
		} else {
			return Err(expected_before(&end.to_string(), "<end>", *line, filename))
		}
	} {
		arg.push(chars.next().unwrap())
	}
	chars.next();
	Ok(arg)
}

fn read_arg(chars: CodeChars, line: Line, filename: &String) -> Result<PreProcessResult, String> {
	reach(chars, '"', line, filename)?;
	preprocess_code(read_until(chars, '"', line, filename)?, line, filename)
}

fn read_block(chars: CodeChars, line: Line, filename: &String) -> Result<(usize, String), String> {
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
	line: Line,
	filename: &String,
) -> Result<bool, String> {
	let (mut line, block) = read_block(chars, line, filename)?;
	code.append(&mut if cond {
		preprocess_code(block, &mut line, filename)?.0
	} else {
		let mut lines = LinkedString::new();
		for _ in 0..block.matches('\n').count() {
			lines.push_back('\n');
		}
		lines
	});
	Ok(cond)
}

fn handle_directive(
	chars: CodeChars,
	code: &mut LinkedString,
	prev: bool,
	directive: &str,
	line: Line,
	filename: &String
) -> Result<bool, String> {
	Ok(match directive {
		"ifos" => {
			let target_os = assert_word(chars, line, filename)?.to_ascii_lowercase();
			keep_block(chars, code, env::consts::OS == target_os, line, filename)?
		}
		"ifdef" => {
			let var = assert_word(chars, line, filename)?;
			keep_block(chars, code, env::var(var).is_ok(), line, filename)?
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
			keep_block(chars, code, result, line, filename)?
		}
		"if" => todo!(),
		"else" => keep_block(chars, code, !prev, line, filename)?,
		"define" => {
			let name = assert_word(chars, line, filename)?;
			if name.contains('=') {
				return Err(error("The value's name cannot contain '='", *line, filename));
			} else if name.ends_with('!') {
				return Err(error("The value's name cannot end with '!'", *line, filename));
			}
			let value = {
				skip_whitespace(chars, line);
				let c = chars.peek();
				let valuef = match c {
					Some(c) if *c == '\'' => |
						chars: CodeChars,
						end: char,
						line: Line,
						filename: &String
					|  -> Result<String, String> {
						chars.next();
						read_until(chars, end, line, filename)
					},
					Some(c) if *c == '"' => |
						chars: CodeChars,
						_: char,
						line: Line,
						filename: &String
					| -> Result<String, String> {
						Ok(read_arg(chars, line, filename)?.0
							.iter()
							.collect::<String>())
					},
					Some(c) => return Err(expected("'' or '\"", &c.to_string(), *line, filename)),
					None => return Err(expected("'' or '\"", "<end>", *line, filename))
				};
				let c = *c.unwrap();
				valuef(chars, c, line, filename)?
			};
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
	})
}

pub fn preprocess_code(rawcode: String, line: Line, filename: &String) -> Result<PreProcessResult, String> {
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
				prev = handle_directive(chars, &mut code, prev, directive.as_str(), line, filename)?;
			}
			'$' => {
				
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
