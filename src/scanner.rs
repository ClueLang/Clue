#![allow(non_camel_case_types)]

use self::TokenType::*;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED,
	SQUARE_BRACKET_OPEN, SQUARE_BRACKET_CLOSED,
	CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, SEMICOLON, NOT, AND, OR, DOLLAR,
	PLUS, MINUS, STAR, SLASH, PERCENTUAL, CARET,
	HASHTAG, METHOD, DOT, TWODOTS, TREDOTS,
	BIT_AND, BIT_OR, BIT_XOR, BIT_NOT,
	LEFT_SHIFT, RIGHT_SHIFT, PROTECTED_GET,
	
	//definition and comparison
	DEFINE, DEFINEIF, INCREASE, DECREASE, MULTIPLY, DIVIDE, EXPONENTIATE, CONCATENATE,
	EQUAL, NOT_EQUAL, BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL,
	
	//literals
	IDENTIFIER, NUMBER, STRING,
	
	//keywords
	IF, ELSEIF, ELSE, FOR, OF, IN, WITH, WHILE, META, GLOBAL, EXTERN,
	UNTIL, GOTO, LOCAL, FUNCTION, RETURN, THIS, TRUE, FALSE, NIL, LOOP,
	
	EOF
}

#[derive(Clone, Debug)]
pub struct Token {
	pub kind: TokenType,
	pub lexeme: String,
	pub line: u32
}

impl Token {
	pub fn new(kind: TokenType, lexeme: String, line: u32) -> Token {
		Token {
			kind: kind,
			lexeme: String::from(lexeme),
			line: line
		}
	}

	pub fn isOp(&self) -> bool {
		match self.kind {
			PLUS | MINUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS | AND | OR |
			BIT_AND | BIT_OR | BIT_XOR | BIT_NOT | LEFT_SHIFT | RIGHT_SHIFT => true,
			_ => false
		}
	}

	pub fn isVar(&self, current: usize, start: usize, end: usize, nt: &Token) -> bool {
		current - 1 == start || current == end || nt.isOp() || nt.kind == SQUARE_BRACKET_CLOSED || nt.kind == ROUND_BRACKET_CLOSED
	}
}

struct CodeInfo {
	line: u32,
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
			filename: filename,
			tokens: Vec::new(),
			errored: false
		}
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> char {
		if pos >= self.size {return 0 as char}
		self.code[pos]
	}

	fn readNext(&mut self) -> char {
		let prev: char = self.at(self.current);
		self.current += 1;
		prev
	}

	fn compare(&mut self, expected: char) -> bool {
		if self.ended() {return false;}
		if self.at(self.current) != expected {return false;}
		self.current = self.current + 1;
		true
	}

	fn peekFar(&self, pos: usize) -> char {
		let pos: usize = self.current + pos;
		if pos >= self.size {return 0 as char;}
		self.at(pos)
	}

	fn peek(&self) -> char {
		self.peekFar(0)
	}

	//isNumber: c.is_ascii_digit()
	//isChar: c.is_ascii_alphabetic()
	//isCharOrNumber: c.is_ascii_alphanumeric()

	fn substr(&self, start: usize, end: usize) -> String {
		let mut result: String = String::new();
		for i in start..end {
			if i >= self.size {break}
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
			false => kf
		};
		self.addToken(kind);
	}

	fn matchAndAdd(&mut self, c1: char, k1: TokenType, c2: char, k2: TokenType, kd: TokenType) {
		let kind: TokenType = match self.compare(c1) {
			true => k1,
			false => match self.compare(c2) {
				true => k2,
				false => kd
			}
		};
		self.addToken(kind);
	}

	fn warning(&mut self, message: &str) {
		println!("Error in file \"{}\" at line [{}]!\nError: \"{}\"\n", self.filename, self.line, message);
		self.errored = true;
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
				if i.peek() == '.' {
					i.current += 1;
					let f: char = i.peekFar(1);
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
			},
			';' => i.addToken(SEMICOLON),
			'+' => i.compareAndAdd('=', INCREASE, PLUS),
			'-' => i.compareAndAdd('=', DECREASE, MINUS),
			'*' => i.compareAndAdd('=', MULTIPLY, STAR),
			'^' => i.matchAndAdd('=', EXPONENTIATE, '^', BIT_XOR, CARET),
			'#' => i.addToken(HASHTAG),
			'/' => {
				match i.peek() {
					'/' => while i.peek() != '\n' && !i.ended() {i.current += 1},
					'*' => {
						while !i.ended() && !(i.peek() == '*' && i.peekFar(1) == '/') {
							if i.peek() == '\n' {i.line += 1}
							i.current += 1;
						}
						if i.ended() {
							i.warning("Unterminated comment");
						} else {
							i.current += 2;
						}
					},
					'=' => i.addToken(DIVIDE),
					_ => i.addToken(SLASH)
				}
			},
			'%' => i.addToken(PERCENTUAL),
			'!' => i.compareAndAdd('=', NOT_EQUAL, NOT),
			'~' => i.addToken(BIT_NOT),
			'=' => i.compareAndAdd('=', EQUAL, DEFINE),
			'<' => i.matchAndAdd('=', SMALLER_EQUAL, '<', LEFT_SHIFT, SMALLER),
			'>' => i.matchAndAdd('=', BIGGER_EQUAL, '>', RIGHT_SHIFT, BIGGER),
			'?' => i.matchAndAdd('=', DEFINEIF, '>', PROTECTED_GET, AND),
			'&' => i.compareAndAdd('&', AND, BIT_AND),
			':' => i.compareAndAdd(':', METHOD, OR),
			'|' => i.compareAndAdd('|', OR, BIT_OR),
			'$' => i.addToken(DOLLAR),
			' ' | '\r' | '\t' => {},
			'\n' => i.line += 1,
			'"' => {
				while !i.ended() && i.peek() != '"' {
					if i.peek() == '\n' {i.line += 1};
					i.current += 1;
				}
				if i.ended() {
					i.warning("Unterminated string");
				} else {
					i.current += 1;
					let literal: String = i.substr(i.start + 1, i.current - 1);
					i.addLiteralToken(STRING, literal);
				}
			}
			_ => {
				if c.is_ascii_digit() {
					while i.peek().is_ascii_digit() {i.current += 1}
					if i.peek() == '.' && i.peekFar(1).is_ascii_digit() {
						i.current += 1;
						while i.peek().is_ascii_digit() {i.current += 1}
					}
					i.addLiteralToken(NUMBER, i.substr(i.start, i.current));
				} else if c.is_ascii_alphabetic() {
					while i.peek().is_ascii_alphanumeric() {i.current += 1}
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
						"extern" => EXTERN,
						"until" => UNTIL,
						"goto" => GOTO,
						"local" => LOCAL,
						"fn" | "function" => FUNCTION,
						"return" => RETURN,
						"this" => THIS,
						"true" => TRUE,
						"false" => FALSE,
						"nil" => NIL,
						"loop" => LOOP,
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
		return Err(String::from("Cannot continue until the above errors are fixed"));
	}
	i.addLiteralToken(EOF, String::new());
	Ok(i.tokens)
}