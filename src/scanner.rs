#![allow(non_camel_case_types)]

use self::TokenType::*;
use ahash::AHashMap;
use lazy_static::lazy_static;
/*
Slowest: 3491020 ns
Fastest: 64360 ns
Average: 110609 ns/iter
Top 1% : 69556 ns/iter
*/

type SymbolsMap = AHashMap<char, SymbolType>;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[rustfmt::skip]
pub enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED, SQUARE_BRACKET_OPEN,
	SQUARE_BRACKET_CLOSED, CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, SEMICOLON, NOT, AND, OR, DOLLAR, PLUS, MINUS, STAR, SLASH,
	PERCENTUAL, CARET, HASHTAG, SAFE_DOUBLE_COLON, DOUBLE_COLON, AT,
	DOT, TWODOTS, THREEDOTS, SAFEDOT, SAFE_SQUARE_BRACKET, SAFE_EXPRESSION,
	BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, LEFT_SHIFT, RIGHT_SHIFT,
	QUESTION_MARK, COLON, ARROW, FLOOR_DIVISION,

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
	pub fn new(kind: TokenType, lexeme: impl Into<String>, line: usize) -> Token {
		Token {
			kind,
			lexeme: lexeme.into(),
			line,
		}
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

	fn look_back(&self, pos: usize) -> char {
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

	fn warning(&mut self, message: impl Into<String>) {
		println!(
			"Error in file \"{}\" at line {}!\nError: \"{}\"\n",
			self.filename,
			self.line,
			message.into()
		);
		self.errored = true;
	}

	fn reserved(&mut self, keyword: &str, msg: &str) -> TokenType {
		self.warning(format!(
			"'{}' is a reserved keyword in Lua and it cannot be used as a variable, {}",
			keyword, msg
		));
		IDENTIFIER
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
		while !self.ended() && self.peek(0) != strend {
			if self.peek(0) == '\\' {
				self.current += 1;
			} else if self.peek(0) == '\n' {
				aline += 1
			};
			self.current += 1;
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
		while !self.ended() && (self.peek(0) != '`' || self.look_back(0) == '\\') {
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
			let mut brackets = String::new();
			let mut must = literal.ends_with(']');
			while must || literal.contains(&format!("]{brackets}]")) {
				brackets += "=";
				must = false;
			}
			self.add_literal_token(
				STRING,
				format!(
					"[{}[{}]{}]",
					brackets,
					literal.replace("\\`", "`"),
					brackets
				),
			);
		}
		self.line = aline
	}

	fn read_identifier(&mut self) -> String {
		while {
			let c = self.peek(0);
			c.is_ascii_alphanumeric() || c == '_'
		} {
			self.current += 1
		}
		self.substr(self.start, self.current)
	}

	fn read_comment(&mut self) {
		while self.peek(0) != '\n' && !self.ended() {
			self.current += 1
		}
	}

	fn read_multiline_comment(&mut self) {
		while !(self.ended() || self.peek(0) == '*' && self.peek(1) == '/') {
			if self.peek(0) == '\n' {
				self.line += 1
			}
			self.current += 1;
		}
		if self.ended() {
			self.warning("Unterminated comment");
		} else {
			self.current += 2;
		}
	}

	fn scan_char(&mut self, symbols: &SymbolsMap, c: &char) -> bool {
		if let Some(token) = symbols.get(c) {
			match token {
				SymbolType::JUST(kind) => self.add_token(*kind),
				SymbolType::SYMBOLS(symbols, default) => {
					let nextc = self.advance();
					if !self.scan_char(symbols, &nextc) {
						self.current -= 1;
						self.add_token(*default);
					}
				},
				SymbolType::FUNCTION(f) => f(self),
			}
			true
		} else {
			false
		}
	}
}

enum SymbolType {
	JUST(TokenType),
	FUNCTION(fn(&mut CodeInfo)),
	SYMBOLS(SymbolsMap, TokenType),
}

lazy_static! {
static ref SYMBOLS: SymbolsMap = AHashMap::from([
	('(', SymbolType::JUST(ROUND_BRACKET_OPEN)),
	(')', SymbolType::JUST(ROUND_BRACKET_CLOSED)),
	('[', SymbolType::JUST(SQUARE_BRACKET_OPEN)),
	(']', SymbolType::JUST(SQUARE_BRACKET_CLOSED)),
	('{', SymbolType::JUST(CURLY_BRACKET_OPEN)),
	('}', SymbolType::JUST(CURLY_BRACKET_CLOSED)),
	(',', SymbolType::JUST(COMMA)),
	('.', SymbolType::SYMBOLS(AHashMap::from([
		('.', SymbolType::SYMBOLS(AHashMap::from([
			('.', SymbolType::JUST(THREEDOTS)),
			('=', SymbolType::JUST(CONCATENATE)),
		]), TWODOTS))
	]), DOT)),
	(';', SymbolType::JUST(SEMICOLON)),
	('+', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(INCREASE)),
	]), PLUS)),
	('-', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(DECREASE)),
	]), MINUS)),
	('*', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(MULTIPLY)),
	]), STAR)),
	('^', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(EXPONENTIATE)),
		('^', SymbolType::JUST(BIT_XOR)),
	]), CARET)),
	('#', SymbolType::JUST(HASHTAG)),
	('/', SymbolType::SYMBOLS(AHashMap::from([
		('/', SymbolType::FUNCTION(CodeInfo::read_comment)),
		('*', SymbolType::FUNCTION(CodeInfo::read_multiline_comment)),
		('=', SymbolType::JUST(DIVIDE)),
		('_', SymbolType::JUST(FLOOR_DIVISION)),
	]), SLASH)),
	('%', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(MODULATE)),
	]), PERCENTUAL)),
	('!', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(NOT_EQUAL)),
	]), NOT)),
	('~', SymbolType::JUST(BIT_NOT)),
	('=', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(EQUAL)),
		('>', SymbolType::JUST(ARROW)),
	]), DEFINE)),
	('<', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(SMALLER_EQUAL)),
		('<', SymbolType::JUST(LEFT_SHIFT)),
	]), SMALLER)),
	('>', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::JUST(BIGGER_EQUAL)),
		('>', SymbolType::JUST(RIGHT_SHIFT)),
	]), BIGGER)),
	('?', SymbolType::SYMBOLS(AHashMap::from([
		('=', SymbolType::FUNCTION(|i| i.warning("'?=' is deprecated and was replaced with '&&='"))),
		('>', SymbolType::JUST(SAFE_EXPRESSION)),
		('.', SymbolType::JUST(SAFEDOT)),
		(':', SymbolType::FUNCTION(|i| {
			if i.compare(':') {
				i.add_token(SAFE_DOUBLE_COLON);
			} else {
				i.current -= 1;
			}
		})),
		('[', SymbolType::JUST(SAFE_SQUARE_BRACKET)),
	]), QUESTION_MARK)),
	('&', SymbolType::SYMBOLS(AHashMap::from([
		('&', SymbolType::JUST(AND)),
	]), BIT_AND)),
	(':', SymbolType::SYMBOLS(AHashMap::from([
		(':', SymbolType::JUST(DOUBLE_COLON)),
		('=', SymbolType::FUNCTION(|i| i.warning("':=' is deprecated and was replaced with '||='"))),
	]), COLON)),
	('|', SymbolType::SYMBOLS(AHashMap::from([
		('|', SymbolType::JUST(OR)),
	]), BIT_OR)),
	('\n', SymbolType::FUNCTION(|i| i.line += 1)),
	('"', SymbolType::FUNCTION(|i| i.read_string('"'))),
	('\'', SymbolType::FUNCTION(|i| i.read_string('\''))),
	('`', SymbolType::FUNCTION(CodeInfo::read_raw_string))
]);
}

pub fn scan_code(code: String, filename: String) -> Result<Vec<Token>, String> {
	let mut i: CodeInfo = CodeInfo::new(code, filename);
	while !i.ended() {
		i.start = i.current;
		let c: char = i.advance();
		if !i.scan_char(&SYMBOLS, &c) {
			if c.is_whitespace() {
				continue
			} if c.is_ascii_digit() {
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
				let kind: TokenType = match i.read_identifier().as_str() {
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
					_ if matches!(i.last, DOT | SAFEDOT | DOUBLE_COLON | SAFE_DOUBLE_COLON) => IDENTIFIER,
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
					"macro" => {println!("Note: the macro keyword will be replaced by @define in 3.0!"); MACRO},
					"constructor" => {i.warning("The struct constructor is reserved for Clue 4.0 and cannot be used."); CONSTRUCTOR},
					"struct" => {i.warning("The struct keyword is reserved for Clue 4.0 and cannot be used."); STRUCT},
					"extern" => {i.warning("The extern keyword is reserved for Clue 4.0 and cannot be used."); EXTERN},
					_ => IDENTIFIER
				};
				i.add_token(kind);
			} else {
				i.warning(format!("Unexpected character '{c}'").as_str());
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
