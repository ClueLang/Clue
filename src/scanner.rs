#![allow(non_camel_case_types)]

use self::TokenType::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED, SQUARE_BRACKET_OPEN,
	SQUARE_BRACKET_CLOSED, CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, SEMICOLON, NOT, AND, OR, DOLLAR, PLUS, MINUS, STAR, SLASH,
	PERCENTUAL, CARET, HASHTAG, SAFE_DOUBLE_COLON, DOUBLE_COLON,
	DOT, TWODOTS, TREDOTS, SAFEDOT, SAFE_SQUARE_BRACKET, PROTECTED_GET,
	BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, LEFT_SHIFT, RIGHT_SHIFT,
	TERNARY_THEN, TERNARY_ELSE, ARROW,

	//definition and comparison
	DEFINE, DEFINE_AND, DEFINE_OR, INCREASE, DECREASE, MULTIPLY, DIVIDE,
	EXPONENTIATE, CONCATENATE, MODULATE, EQUAL, NOT_EQUAL,
	BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL,

	//literals
	IDENTIFIER, NUMBER, STRING,

	//keywords
	IF, ELSEIF, ELSE, FOR, OF, IN, WITH, WHILE, META, GLOBAL, UNTIL,
	LOCAL, FN, METHOD, RETURN, TRUE, FALSE, NIL, LOOP, STATIC, ENUM,
	CONTINUE, BREAK, TRY, CATCH, MATCH, DEFAULT,

	EOF,
}

#[derive(Clone, Debug)]
pub struct Token {
	pub kind: TokenType,
	pub lexeme: String,
	pub line: usize,
}

impl Token {
	pub fn new(kind: TokenType, lexeme: String, line: usize) -> Token {
		Token { kind, lexeme, line }
	}
}

struct CodeInfo {
	line: usize,
	start: usize,
	current: usize,
	size: usize,
	code: Vec<char>,
	filename: String,
	tokens: Vec<Token>,
	errored: bool,
}

impl CodeInfo {
	fn new(code: String, filename: String) -> CodeInfo {
		let chars = code.chars();
		CodeInfo {
			line: 1,
			start: 0,
			current: 0,
			size: chars.clone().count(),
			code: chars.collect(),
			filename,
			tokens: Vec::new(),
			errored: false,
		}
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> char {
		if pos >= self.size {
			return 0 as char;
		}
		self.code[pos]
	}

	fn readNext(&mut self) -> char {
		let prev: char = self.at(self.current);
		self.current += 1;
		prev
	}

	fn compare(&mut self, expected: char) -> bool {
		if self.ended() {
			return false;
		}
		if self.at(self.current) != expected {
			return false;
		}
		self.current += 1;
		true
	}

	fn peek(&self, pos: usize) -> char {
		let pos: usize = self.current + pos;
		if pos >= self.size {
			return 0 as char;
		}
		self.at(pos)
	}

	//isNumber: c.is_ascii_digit()
	//isChar: c.is_ascii_alphabetic()
	//isCharOrNumber: c.is_ascii_alphanumeric()

	fn substr(&self, start: usize, end: usize) -> String {
		let mut result: String = String::new();
		for i in start..end {
			if i >= self.size {
				break;
			}
			result.push(self.at(i));
		}
		result
	}

	fn addLiteralToken(&mut self, kind: TokenType, literal: String) {
		self.tokens.push(Token::new(kind, literal, self.line));
	}

	fn addToken(&mut self, kind: TokenType) {
		let lexeme: String = self.substr(self.start, self.current);
		self.tokens.push(Token::new(kind, lexeme, self.line));
	}

	fn compareAndAdd(&mut self, c: char, kt: TokenType, kf: TokenType) {
		let kind: TokenType = match self.compare(c) {
			true => kt,
			false => kf,
		};
		self.addToken(kind);
	}

	fn matchAndAdd(&mut self, c1: char, k1: TokenType, c2: char, k2: TokenType, kd: TokenType) {
		let kind: TokenType = match self.compare(c1) {
			true => k1,
			false => match self.compare(c2) {
				true => k2,
				false => kd,
			},
		};
		self.addToken(kind);
	}

	fn warning(&mut self, message: impl Into<String>) {
		println!(
			"Error in file \"{}\" at line {}!\nError: \"{}\"\n",
			self.filename,
			self.line,
			message.into()
		);
		self.errored = true;
	}

	fn readNumber(&mut self, check: impl Fn(&char) -> bool, simple: bool) {
		let start = self.current;
		while check(&self.peek(0)) {
			self.current += 1
		}
		if self.peek(0) == '.' && check(&self.peek(1)) {
			self.current += 1;
			while check(&self.peek(0)) {
				self.current += 1
			}
		}
		if simple {
			let c = self.peek(0);
			if c == 'e' || c == 'E' {
				let c = self.peek(1);
				if !c.is_ascii_digit() {
					if c == '-' && self.peek(2).is_ascii_digit() {
						self.current += 1;
					} else {
						self.warning("Malformed number");
					}
				}
				self.current += 1;
				while self.peek(0).is_ascii_digit() {
					self.current += 1
				}
			}
		} else if self.current == start {
			self.warning("Malformed number");
		}
		let llcheck = self.substr(self.current, self.current + 2);
		if llcheck == "LL" {
			self.current += 2;
		} else if llcheck == "UL" {
			if self.peek(2) == 'L' {
				self.current += 3;
			} else {
				self.warning("Malformed number");
			}
		}
		self.addLiteralToken(NUMBER, self.substr(self.start, self.current));
	}

	fn readString(&mut self, strend: char) {
		let mut aline = self.line;
		while !self.ended() && self.peek(0) != strend {
			if self.peek(0) == '\n' {
				aline += 1
			};
			self.current += 1;
		}
		if self.ended() {
			self.warning("Unterminated string");
		} else {
			self.current += 1;
			let mut literal: String = self.substr(self.start + 1, self.current - 1);
			literal.retain(|c| !matches!(c, '\r' | '\n' | '\t'));
			self.addLiteralToken(STRING, literal);
		}
		self.line = aline;
	}

	fn reserved(&mut self, keyword: &str, msg: &str) -> TokenType {
		self.warning(format!(
			"'{}' is a reserved keyword in Lua and it cannot be used as a variable, {}",
			keyword, msg
		));
		IDENTIFIER
	}
}

pub fn ScanCode(code: String, filename: String) -> Result<Vec<Token>, String> {
	let mut i: CodeInfo = CodeInfo::new(code, filename);
	while !i.ended() {
		i.start = i.current;
		let c: char = i.readNext();
		match c {
			'(' => i.addToken(ROUND_BRACKET_OPEN),
			')' => i.addToken(ROUND_BRACKET_CLOSED),
			'[' => i.addToken(SQUARE_BRACKET_OPEN),
			']' => i.addToken(SQUARE_BRACKET_CLOSED),
			'{' => i.addToken(CURLY_BRACKET_OPEN),
			'}' => i.addToken(CURLY_BRACKET_CLOSED),
			',' => i.addToken(COMMA),
			'.' => {
				if i.peek(0) == '.' {
					i.current += 1;
					let f: char = i.peek(0);
					if f == '.' {
						i.current += 1;
						i.addToken(TREDOTS);
					} else if f == '=' {
						i.current += 1;
						i.addToken(CONCATENATE);
					} else {
						i.addToken(TWODOTS);
					}
				} else {
					i.addToken(DOT);
				}
			}
			';' => i.addToken(SEMICOLON),
			'+' => i.compareAndAdd('=', INCREASE, PLUS),
			'-' => i.compareAndAdd('=', DECREASE, MINUS),
			'*' => i.compareAndAdd('=', MULTIPLY, STAR),
			'^' => i.matchAndAdd('=', EXPONENTIATE, '^', BIT_XOR, CARET),
			'#' => i.addToken(HASHTAG),
			'/' => match i.peek(0) {
				'/' => {
					while i.peek(0) != '\n' && !i.ended() {
						i.current += 1
					}
				}
				'*' => {
					while !(i.ended() || i.peek(0) == '*' && i.peek(1) == '/') {
						if i.peek(0) == '\n' {
							i.line += 1
						}
						i.current += 1;
					}
					if i.ended() {
						i.warning("Unterminated comment");
					} else {
						i.current += 2;
					}
				}
				'=' => {
					i.current += 1;
					i.addToken(DIVIDE);
				}
				_ => i.addToken(SLASH),
			},
			'%' => i.compareAndAdd('=', MODULATE, PERCENTUAL),
			'!' => i.compareAndAdd('=', NOT_EQUAL, NOT),
			'~' => i.addToken(BIT_NOT),
			'=' => i.matchAndAdd('=', EQUAL, '>', ARROW, DEFINE),
			'<' => i.matchAndAdd('=', SMALLER_EQUAL, '<', LEFT_SHIFT, SMALLER),
			'>' => i.matchAndAdd('=', BIGGER_EQUAL, '>', RIGHT_SHIFT, BIGGER),
			'?' => {
				let kind = match i.readNext() {
					'=' => DEFINE_AND,
					'>' => {
						i.warning("?> is deprecated");
						PROTECTED_GET
					}
					'.' => SAFEDOT,
					':' => {
						if i.compare(':') {
							SAFE_DOUBLE_COLON
						} else {
							i.current -= 1;
							continue;
						}
					}
					'[' => SAFE_SQUARE_BRACKET,
					_ => {
						i.current -= 1;
						TERNARY_THEN
					}
				};
				i.addToken(kind);
			}
			'&' => i.compareAndAdd('&', AND, BIT_AND),
			':' => i.matchAndAdd(':', DOUBLE_COLON, '=', DEFINE_OR, TERNARY_ELSE),
			'|' => i.compareAndAdd('|', OR, BIT_OR),
			'$' => i.addToken(DOLLAR),
			' ' | '\r' | '\t' => {}
			'\n' => i.line += 1,
			'"' | '\'' => i.readString(c),
			_ => {
				if c.is_ascii_digit() {
					if c == '0' {
						match i.peek(0) {
							'x' | 'X' => {
								i.current += 1;
								i.readNumber(
									|c| {
										let c = *c;
										c.is_ascii_digit()
											|| ('a'..='f').contains(&c) || ('A'..='F').contains(&c)
									},
									false,
								);
							}
							'b' | 'B' => {
								i.current += 1;
								i.readNumber(
									|c| {
										let c = *c;
										c == '0' || c == '1'
									},
									false,
								);
							}
							_ => i.readNumber(char::is_ascii_digit, true),
						}
					} else {
						i.readNumber(char::is_ascii_digit, true);
					}
				} else if c.is_ascii_alphabetic() || c == '_' {
					while {
						let c = i.peek(0);
						c.is_ascii_alphanumeric() || c == '_'
					} {
						i.current += 1
					}
					let string: String = i.substr(i.start, i.current);
					let kind: TokenType = match string.as_str() {
						"if" => IF,
						"elseif" => ELSEIF,
						"else" => ELSE,
						"for" => FOR,
						"of" => OF,
						"in" => IN,
						"with" => WITH,
						"while" => WHILE,
						"meta" => META,
						"global" => GLOBAL,
						"until" => UNTIL,
						"local" => LOCAL,
						"fn" => FN,
						"method" => METHOD,
						"return" => RETURN,
						"true" => TRUE,
						"false" => FALSE,
						"nil" => NIL,
						"loop" => LOOP,
						"static" => STATIC,
						"enum" => ENUM,
						"continue" => CONTINUE,
						"break" => BREAK,
						"try" => TRY,
						"catch" => CATCH,
						"match" => MATCH,
						"default" => DEFAULT,
						"and" => i.reserved("and", "'and' operators in Clue are made with '&&'"),
						"not" => i.reserved("not", "'not' operators in Clue are made with '!'"),
						"or" => i.reserved("or", "'or' operators in Clue are made with '||'"),
						"do" => i.reserved("do", "'do ... end' blocks in Clue are made like this: '{{ ... }}'"),
						"end" => i.reserved("end", "code blocks in Clue are closed with '}}'"),
						"function" => i.reserved("function", "functions in Clue are defined with the 'fn' keyword"),
						"repeat" => i.reserved("repeat", "'repeat ... until x' loops in Clue are made like this: 'loop {{ ... }} until x'"),
						"then" => i.reserved("then", "code blocks in Clue are opened with '{{'"),
						_ => IDENTIFIER
					};
					i.addToken(kind);
				} else {
					i.warning(format!("Unexpected character '{}'", c).as_str());
				}
			}
		}
	}
	if i.errored {
		return Err(String::from(
			"Cannot continue until the above errors are fixed",
		));
	}
	i.addLiteralToken(EOF, String::from("<end>"));
	Ok(i.tokens)
}
