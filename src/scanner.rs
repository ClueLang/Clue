#![allow(non_camel_case_types)]

use self::TokenType::*;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED, SQUARE_BRACKET_OPEN,
	SQUARE_BRACKET_CLOSED, CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, SEMICOLON, NOT, AND, OR, DOLLAR, PLUS, MINUS, STAR, SLASH,
	PERCENTUAL, CARET, HASHTAG, SAFE_DOUBLE_COLON, DOUBLE_COLON, AT,
	DOT, TWODOTS, THREEDOTS, SAFEDOT, SAFE_SQUARE_BRACKET, PROTECTED_GET,
	BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, LEFT_SHIFT, RIGHT_SHIFT,
	TERNARY_THEN, TERNARY_ELSE, ARROW, FLOOR_DIVISION,

	//definition and comparison
	DEFINE, DEFINE_AND, DEFINE_OR, INCREASE, DECREASE, MULTIPLY, DIVIDE,
	EXPONENTIATE, CONCATENATE, MODULATE, EQUAL, NOT_EQUAL,
	BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL,

	//literals
	IDENTIFIER, NUMBER, STRING,

	//keywords
	IF, ELSEIF, ELSE, FOR, OF, IN, WITH, WHILE, META, GLOBAL, UNTIL,
	LOCAL, FN, METHOD, RETURN, TRUE, FALSE, NIL, LOOP, STATIC, ENUM,
	CONTINUE, BREAK, TRY, CATCH, MATCH, DEFAULT, MACRO, STRUCT, EXTERN,
	CONSTRUCTOR,

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
	last: TokenType,
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
			last: EOF,
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

	fn advance(&mut self) -> char {
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
		self.at(pos)
	}

	fn lookBack(&self, pos: usize) -> char {
		let pos: usize = self.current - pos - 1;
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

	fn add_literal_token(&mut self, kind: TokenType, literal: String) {
		self.tokens.push(Token::new(kind, literal, self.line));
	}

	fn add_token(&mut self, kind: TokenType) {
		let lexeme: String = self.substr(self.start, self.current);
		self.last = kind;
		self.tokens.push(Token::new(kind, lexeme, self.line));
	}

	fn compare_and_add(&mut self, c: char, kt: TokenType, kf: TokenType) {
		let kind: TokenType = match self.compare(c) {
			true => kt,
			false => kf,
		};
		self.add_token(kind);
	}

	fn match_and_add(&mut self, c1: char, k1: TokenType, c2: char, k2: TokenType, kd: TokenType) {
		let kind: TokenType = match self.compare(c1) {
			true => k1,
			false => match self.compare(c2) {
				true => k2,
				false => kd,
			},
		};
		self.add_token(kind);
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

	fn read_number(&mut self, check: impl Fn(&char) -> bool, simple: bool) {
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
		self.add_token(NUMBER);
	}

	fn read_string(&mut self, strend: char) {
		let mut aline = self.line;
		while !self.ended() && (self.peek(0) != strend || self.lookBack(0) == '\\') {
			if self.peek(0) == '\n' {
				aline += 1
			};
			self.current += 1;
			println!("{}", self.peek(0));
		}
		if self.ended() {
			self.warning("Unterminated string");
		} else {
			self.current += 1;
			let mut literal: String = self.substr(self.start, self.current);
			literal.retain(|c| !matches!(c, '\r' | '\n' | '\t'));
			self.add_literal_token(STRING, literal);
		}
		self.line = aline;
	}

	fn read_raw_string(&mut self) {
		let mut aline = self.line;
		while !self.ended() && (self.peek(0) != '`' || self.lookBack(0) == '\\') {
			if self.peek(0) == '\n' {
				aline += 1
			};
			self.current += 1;
		}
		if self.ended() {
			self.warning("Unterminated string");
		} else {
			self.current += 1;
			let literal: String = self.substr(self.start + 1, self.current - 1);
			self.add_literal_token(STRING, format!("[[{}]]", literal.replace("\\`", "`")));
		}
		self.line = aline
	}

	fn readIdentifier(&mut self) -> String {
		while {
			let c = self.peek(0);
			c.is_ascii_alphanumeric() || c == '_'
		} {
			self.current += 1
		}
		self.substr(self.start, self.current)
	}

	fn reserved(&mut self, keyword: &str, msg: &str) -> TokenType {
		self.warning(format!(
			"'{}' is a reserved keyword in Lua and it cannot be used as a variable, {}",
			keyword, msg
		));
		IDENTIFIER
	}
}

pub fn scan_code(code: String, filename: String) -> Result<Vec<Token>, String> {
	let mut i: CodeInfo = CodeInfo::new(code, filename);
	while !i.ended() {
		i.start = i.current;
		let c: char = i.advance();
		match c {
			'(' => i.add_token(ROUND_BRACKET_OPEN),
			')' => i.add_token(ROUND_BRACKET_CLOSED),
			'[' => i.add_token(SQUARE_BRACKET_OPEN),
			']' => i.add_token(SQUARE_BRACKET_CLOSED),
			'{' => i.add_token(CURLY_BRACKET_OPEN),
			'}' => i.add_token(CURLY_BRACKET_CLOSED),
			',' => i.add_token(COMMA),
			'.' => {
				if i.peek(0) == '.' {
					i.current += 1;
					let f: char = i.peek(0);
					if f == '.' {
						i.current += 1;
						i.add_token(THREEDOTS);
					} else if f == '=' {
						i.current += 1;
						i.add_token(CONCATENATE);
					} else {
						i.add_token(TWODOTS);
					}
				} else {
					i.add_token(DOT);
				}
			}
			';' => i.add_token(SEMICOLON),
			'+' => i.compare_and_add('=', INCREASE, PLUS),
			'-' => i.compare_and_add('=', DECREASE, MINUS),
			'*' => i.compare_and_add('=', MULTIPLY, STAR),
			'^' => i.match_and_add('=', EXPONENTIATE, '^', BIT_XOR, CARET),
			'#' => i.add_token(HASHTAG),
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
				_ => i.match_and_add('=', DIVIDE, '_', FLOOR_DIVISION, SLASH)
			},
			'%' => i.compare_and_add('=', MODULATE, PERCENTUAL),
			'!' => i.compare_and_add('=', NOT_EQUAL, NOT),
			'~' => i.add_token(BIT_NOT),
			'=' => i.match_and_add('=', EQUAL, '>', ARROW, DEFINE),
			'<' => i.match_and_add('=', SMALLER_EQUAL, '<', LEFT_SHIFT, SMALLER),
			'>' => i.match_and_add('=', BIGGER_EQUAL, '>', RIGHT_SHIFT, BIGGER),
			'?' => {
				let kind = match i.advance() {
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
				i.add_token(kind);
			}
			'&' => i.compare_and_add('&', AND, BIT_AND),
			':' => i.match_and_add(':', DOUBLE_COLON, '=', DEFINE_OR, TERNARY_ELSE),
			'|' => i.compare_and_add('|', OR, BIT_OR),
			'$' => i.add_token(DOLLAR),
			'@' => i.add_token(AT),
			' ' | '\r' | '\t' => {}
			'\n' => i.line += 1,
			'"' | '\'' => i.read_string(c),
			'`' => i.read_raw_string(),
			_ => {
				if c.is_ascii_digit() {
					if c == '0' {
						match i.peek(0) {
							'x' | 'X' => {
								i.current += 1;
								i.read_number(
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
								i.read_number(
									|c| {
										let c = *c;
										c == '0' || c == '1'
									},
									false,
								);
							}
							_ => i.read_number(char::is_ascii_digit, true),
						}
					} else {
						i.read_number(char::is_ascii_digit, true);
					}
				} else if c.is_ascii_alphabetic() || c == '_' {
					let kind: TokenType = match i.readIdentifier().as_str() {
						"and" => i.reserved("and", "'and' operators in Clue are made with '&&'"),
						"not" => i.reserved("not", "'not' operators in Clue are made with '!'"),
						"or" => i.reserved("or", "'or' operators in Clue are made with '||'"),
						"do" => i.reserved("do", "'do ... end' blocks in Clue are made like this: '{ ... }'"),
						"end" => i.reserved("end", "code blocks in Clue are closed with '}'"),
						"function" => i.reserved("function", "functions in Clue are defined with the 'fn' keyword"),
						"repeat" => i.reserved("repeat", "'repeat ... until x' loops in Clue are made like this: 'loop { ... } until x'"),
						"then" => i.reserved("then", "code blocks in Clue are opened with '{'"),
						"if" => IF,
						"elseif" => ELSEIF,
						"else" => ELSE,
						"for" => FOR,
						"in" => IN,
						"while" => WHILE,
						"until" => UNTIL,
						"local" => LOCAL,
						"return" => RETURN,
						"true" => TRUE,
						"false" => FALSE,
						"nil" => NIL,
						"break" => BREAK,
						_ if match i.last {
							DOT | SAFEDOT | DOUBLE_COLON | SAFE_DOUBLE_COLON => true,
							_ => false
						} => IDENTIFIER,
						"of" => OF,
						"with" => WITH,
						"meta" => META,
						"global" => GLOBAL,
						"fn" => FN,
						"method" => METHOD,
						"loop" => LOOP,
						"static" => STATIC,
						"enum" => ENUM,
						"continue" => CONTINUE,
						"try" => TRY,
						"catch" => CATCH,
						"match" => MATCH,
						"default" => DEFAULT,
						"macro" => MACRO,
						"constructor" => {i.warning("The struct constructor is reserved for Clue 3.X and cannot be used."); CONSTRUCTOR},
						"struct" => {i.warning("The struct keyword is reserved for Clue 3.X and cannot be used."); STRUCT},
						"extern" => {i.warning("The extern keyword is reserved for Clue 3.0 and cannot be used."); EXTERN},
						_ => IDENTIFIER
					};
					i.add_token(kind);
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
	i.add_literal_token(EOF, String::from("<end>"));
	Ok(i.tokens)
}
