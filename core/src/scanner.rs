#![allow(non_camel_case_types)]
#![allow(clippy::upper_case_acronyms)]

use crate::{format_clue, code::{Code, CodeChars}};

use self::TokenType::*;
use ahash::AHashMap;
use std::{cmp, fmt};
use lazy_static::lazy_static;

type SymbolsMap = Vec<Option<SymbolType>>;

fn generate_map(elements: &[(char, SymbolType)]) -> SymbolsMap {
	let mut map: SymbolsMap = vec![None; 127];
	let mut biggestkey = 0usize;
	for (key, value) in elements {
		let key = *key as usize;
		map[key] = Some(value.clone());
		biggestkey = cmp::max(biggestkey, key);
	}
	map.truncate(biggestkey + 1);
	map
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[rustfmt::skip]
pub enum TokenType {
	//symbols
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED, SQUARE_BRACKET_OPEN,
	SQUARE_BRACKET_CLOSED, CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	COMMA, SEMICOLON, NOT, AND, OR, PLUS, MINUS, STAR, SLASH,
	PERCENTUAL, CARET, HASHTAG, SAFE_DOUBLE_COLON, DOUBLE_COLON, AT,
	DOT, TWODOTS, THREEDOTS, SAFEDOT, SAFE_SQUARE_BRACKET, SAFE_EXPRESSION,
	BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, LEFT_SHIFT, RIGHT_SHIFT,
	QUESTION_MARK, COLON, ARROW, FLOOR_DIVISION, COALESCE,

	//definition and comparison
	DEFINE, DEFINE_AND, DEFINE_OR, INCREASE, DECREASE, MULTIPLY, DIVIDE,
	DEFINE_COALESCE, EXPONENTIATE, CONCATENATE, MODULATE,
	BIGGER, BIGGER_EQUAL, SMALLER, SMALLER_EQUAL, EQUAL, NOT_EQUAL,

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

struct CodeInfo<'a> {
	line: usize,
	start: usize,
	current: usize,
	size: usize,
	code: CodeChars,
	read: Vec<char>,
	filename: &'a String,
	tokens: Vec<Token>,
	last: TokenType,
	errored: bool,
}

impl<'a> CodeInfo<'a> {
	fn new(code: Code, filename: &'a String) -> Self {
		let size = code.len() + 2;
		let mut code = code.chars();
		let mut read = Vec::with_capacity(size);
		read.push(code.next_unwrapped());
		read.push(code.next_unwrapped());
		Self {
			line: 1,
			start: 0,
			current: 0,
			size,
			code,
			read,
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
		self.read[pos]
	}

	fn advance(&mut self) -> char {
		self.read.push(self.code.next_unwrapped());
		self.line = self.code.line();
		let prev = self.at(self.current);
		let read = self.code.bytes_read();
		if read > 0 {
			self.size -= read - 1
		}
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
		self.advance();
		true
	}

	fn peek(&self, pos: usize) -> char {
		let pos: usize = self.current + pos;
		self.at(pos)
	}

	fn look_back(&self) -> char {
		self.at(self.current - 1)
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
			self.advance();
		}
		if self.peek(0) == '.' && check(&self.peek(1)) {
			self.advance();
			while check(&self.peek(0)) {
				self.advance();
			}
		}
		if simple {
			let c = self.peek(0);
			if c == 'e' || c == 'E' {
				let c = self.peek(1);
				if !c.is_ascii_digit() {
					if c == '-' && self.peek(2).is_ascii_digit() {
						self.advance();
					} else {
						self.warning("Malformed number");
					}
				}
				self.advance();
				while self.peek(0).is_ascii_digit() {
					self.advance();
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

	fn read_string_contents(&mut self, strend: char) -> bool {
		while !self.ended() && self.peek(0) != strend {
			self.advance();
			if self.look_back() == '\\' {
				self.advance();
			}
		}
		if self.ended() {
			self.warning("Unterminated string");
			false
		} else {
			true
		}
	}

	fn read_string(&mut self, strend: char) {
		if self.read_string_contents(strend) {
			self.advance();
			let mut literal = self.substr(self.start, self.current);
			literal.retain(|c| !matches!(c, '\r' | '\n' | '\t'));
			self.add_literal_token(STRING, literal);
		}
	}

	fn read_raw_string(&mut self) {
		if self.read_string_contents('`') {
			self.advance();
			let literal = self.substr(self.start + 1, self.current - 1);
			let mut brackets = String::new();
			let mut must = literal.ends_with(']');
			while must || literal.contains(&format_clue!("]", brackets, "]")) {
				brackets += "=";
				must = false;
			}
			self.add_literal_token(
				STRING,
				format_clue!("[", brackets, "[", literal.replace("\\`", "`"), "]", brackets, "]")
			);
		}
	}

	fn read_identifier(&mut self) -> String {
		while {
			let c = self.peek(0);
			c.is_ascii_alphanumeric() || c == '_'
		} {
			self.advance();
		}
		self.substr(self.start, self.current)
	}

	fn scan_char(&mut self, symbols: &SymbolsMap, c: char) -> bool {
		if let Some(Some(token)) = symbols.get(c as usize) {
			match token {
				SymbolType::JUST(kind) => self.add_token(*kind),
				SymbolType::SYMBOLS(symbols, default) => {
					let nextc = self.advance();
					if !self.scan_char(symbols, nextc) {
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

#[derive(Clone)]
enum SymbolType {
	JUST(TokenType),
	FUNCTION(fn(&mut CodeInfo)),
	SYMBOLS(SymbolsMap, TokenType),
}

impl fmt::Debug for SymbolType {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
			SymbolType::JUST(kind) => format!("{kind:?}"),
			SymbolType::FUNCTION(_) => String::from("FUNCTION({...})"),
			SymbolType::SYMBOLS(symbols, kind) => format!("({symbols:#?}, {kind:?})")
		})
    }
}

enum KeywordType {
	JUST(TokenType),
	LUA(TokenType),
	ERROR(&'static str),
	RESERVED(&'static str),
}

lazy_static! {
	static ref SYMBOLS: SymbolsMap = generate_map(&[
		('(', SymbolType::JUST(ROUND_BRACKET_OPEN)),
		(')', SymbolType::JUST(ROUND_BRACKET_CLOSED)),
		('[', SymbolType::JUST(SQUARE_BRACKET_OPEN)),
		(']', SymbolType::JUST(SQUARE_BRACKET_CLOSED)),
		('{', SymbolType::JUST(CURLY_BRACKET_OPEN)),
		('}', SymbolType::JUST(CURLY_BRACKET_CLOSED)),
		(',', SymbolType::JUST(COMMA)),
		('.', SymbolType::SYMBOLS(generate_map(&[
			('.', SymbolType::SYMBOLS(generate_map(&[
				('.', SymbolType::JUST(THREEDOTS)),
				('=', SymbolType::JUST(CONCATENATE)),
			]), TWODOTS))
		]), DOT)),
		(';', SymbolType::JUST(SEMICOLON)),
		('+', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(INCREASE)),
		]), PLUS)),
		('-', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(DECREASE)),
		]), MINUS)),
		('*', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(MULTIPLY)),
		]), STAR)),
		('^', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(EXPONENTIATE)),
			('^', SymbolType::JUST(BIT_XOR)),
		]), CARET)),
		('#', SymbolType::JUST(HASHTAG)),
		('/', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(DIVIDE)),
			('_', SymbolType::JUST(FLOOR_DIVISION)),
		]), SLASH)),
		('%', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(MODULATE)),
		]), PERCENTUAL)),
		('!', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(NOT_EQUAL)),
		]), NOT)),
		('~', SymbolType::JUST(BIT_NOT)),
		('=', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(EQUAL)),
			('>', SymbolType::JUST(ARROW)),
		]), DEFINE)),
		('<', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(SMALLER_EQUAL)),
			('<', SymbolType::JUST(LEFT_SHIFT)),
		]), SMALLER)),
		('>', SymbolType::SYMBOLS(generate_map(&[
			('=', SymbolType::JUST(BIGGER_EQUAL)),
			('>', SymbolType::JUST(RIGHT_SHIFT)),
		]), BIGGER)),
		('?', SymbolType::SYMBOLS(generate_map(&[
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
			('?', SymbolType::SYMBOLS(generate_map(&[
				('=', SymbolType::JUST(DEFINE_COALESCE))
			]), COALESCE))
		]), QUESTION_MARK)),
		('&', SymbolType::SYMBOLS(generate_map(&[
			('&', SymbolType::SYMBOLS(generate_map(&[
				('=', SymbolType::JUST(DEFINE_AND))
			]), AND)),
		]), BIT_AND)),
		(':', SymbolType::SYMBOLS(generate_map(&[
			(':', SymbolType::JUST(DOUBLE_COLON)),
			('=', SymbolType::FUNCTION(|i| i.warning("':=' is deprecated and was replaced with '||='"))),
		]), COLON)),
		('|', SymbolType::SYMBOLS(generate_map(&[
			('|', SymbolType::SYMBOLS(generate_map(&[
				('=', SymbolType::JUST(DEFINE_OR))
			]), OR)),
		]), BIT_OR)),
		('"', SymbolType::FUNCTION(|i| i.read_string('"'))),
		('\'', SymbolType::FUNCTION(|i| i.read_string('\''))),
		('`', SymbolType::FUNCTION(|i| i.read_raw_string()))
	]);

	static ref KEYWORDS: AHashMap<&'static str, KeywordType> = AHashMap::from([
		("and", KeywordType::RESERVED("'and' operators in Clue are made with '&&'")),
		("not", KeywordType::RESERVED("'not' operators in Clue are made with '!'")),
		("or", KeywordType::RESERVED("'or' operators in Clue are made with '||'")),
		("do", KeywordType::RESERVED("'do ... end' blocks in Clue are made like this: '{ ... }'")),
		("end", KeywordType::RESERVED("code blocks in Clue are closed with '}'")),
		("function", KeywordType::RESERVED("functions in Clue are defined with the 'fn' keyword")),
		("repeat", KeywordType::RESERVED("'repeat ... until x' loops in Clue are made like this: 'loop { ... } until x'")),
		("then", KeywordType::RESERVED("code blocks in Clue are opened with '{'")),
		("if", KeywordType::LUA(IF)),
		("elseif", KeywordType::LUA(ELSEIF)),
		("else", KeywordType::LUA(ELSE)),
		("for", KeywordType::LUA(FOR)),
		("in", KeywordType::LUA(IN)),
		("while", KeywordType::LUA(WHILE)),
		("until", KeywordType::LUA(UNTIL)),
		("local", KeywordType::LUA(LOCAL)),
		("return", KeywordType::LUA(RETURN)),
		("true", KeywordType::LUA(TRUE)),
		("false", KeywordType::LUA(FALSE)),
		("nil", KeywordType::LUA(NIL)),
		("break", KeywordType::LUA(BREAK)),
		("of", KeywordType::JUST(OF)),
		("with", KeywordType::JUST(WITH)),
		("meta", KeywordType::JUST(META)),
		("global", KeywordType::JUST(GLOBAL)),
		("fn", KeywordType::JUST(FN)),
		("method", KeywordType::JUST(METHOD)),
		("loop", KeywordType::JUST(LOOP)),
		("static", KeywordType::JUST(STATIC)),
		("enum", KeywordType::JUST(ENUM)),
		("continue", KeywordType::JUST(CONTINUE)),
		("try", KeywordType::JUST(TRY)),
		("catch", KeywordType::JUST(CATCH)),
		("match", KeywordType::JUST(MATCH)),
		("default", KeywordType::JUST(DEFAULT)),
		("macro", KeywordType::ERROR("'macro' is deprecated and was replaced with '@define'")),
		("constructor", KeywordType::ERROR("'constructor' is reserved for Clue 4.0 and cannnot be used.")),
		("struct", KeywordType::ERROR("'struct' is reserved for Clue 4.0 and cannot be used")),
		("extern", KeywordType::ERROR("'extern' is reserved for Clue 4.0 and cannot be used")),
	]);
}

pub fn scan_code(code: Code, filename: &String) -> Result<Vec<Token>, String> {
	let mut i: CodeInfo = CodeInfo::new(code, filename);
	while !i.ended() && i.peek(0) != '\0' {
		i.start = i.current;
		let c = i.advance();
		if !i.scan_char(&SYMBOLS, c) {
			if c.is_whitespace() {
				continue
			} else if c.is_ascii_digit() {
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
				let ident = i.read_identifier();
				let kind = if let Some(keyword) = KEYWORDS.get(ident.as_str()) {
					match keyword {
						KeywordType::LUA(kind) => *kind,
						KeywordType::RESERVED(e) => i.reserved(&ident, e),
						_ if matches!(i.last, DOT | SAFEDOT | DOUBLE_COLON | SAFE_DOUBLE_COLON) => IDENTIFIER,
						KeywordType::JUST(kind) => *kind,
						KeywordType::ERROR(e) => {i.warning(*e); IDENTIFIER}
					}
				} else {
					IDENTIFIER
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
