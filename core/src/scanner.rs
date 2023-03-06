#![allow(non_camel_case_types)]
#![allow(clippy::upper_case_acronyms)]

use crate::{
	code::{Code, CodeChars},
	format_clue,
};

use self::TokenType::*;
use ahash::AHashMap;
use lazy_static::lazy_static;
use std::fmt;

type SymbolsMap<'a> = [Option<&'a SymbolType<'a>>; 127];

const fn generate_map<'a>(elements: &'a [(char, SymbolType)]) -> SymbolsMap<'a> {
	let mut map = [None; 127];
	let mut i = 0;
	while i < elements.len() {
		let (key, value) = &elements[i];
		let key = *key as usize;
		map[key] = Some(value);
		i += 1;
	}
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
	pub fn new(kind: TokenType, lexeme: impl Into<String>, line: usize) -> Self {
		Self {
			kind,
			lexeme: lexeme.into(),
			line,
		}
	}
}

pub struct BorrowedToken {
	token: *const Token,
}

impl BorrowedToken {
	pub const fn new(token: *const Token) -> Self {
		Self { token }
	}

	pub const fn token(&self) -> &Token {
		// SAFETY: This is safe because the pointer is guaranteed to be valid
		unsafe { &(*self.token) }
	}

	pub const fn kind(&self) -> TokenType {
		self.token().kind
	}

	pub fn lexeme(&self) -> String {
		self.token().lexeme.clone()
	}

	pub const fn line(&self) -> usize {
		self.token().line
	}

	pub fn into_owned(&self) -> Token {
		self.token().clone()
	}
}

struct CodeInfo<'a> {
	line: usize,
	start: usize,
	current: usize,
	size: usize,
	code: CodeChars,
	read: Vec<(char, usize)>,
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
		read.push((code.next_unwrapped(), code.line()));
		read.push((code.next_unwrapped(), code.line()));
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

	const fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> char {
		self.read[pos].0
	}

	fn advance(&mut self) -> char {
		self.read.push((self.code.next_unwrapped(), self.code.line()));
		let (prev, line) = self.read[self.current];
		self.line = line;
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
			"'{keyword}' is a reserved keyword in Lua and it cannot be used as a variable, {msg}",
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
				format_clue!(
					"[",
					brackets,
					"[",
					literal.replace("\\`", "`"),
					"]",
					brackets,
					"]"
				),
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
				SymbolType::Just(kind) => self.add_token(*kind),
				SymbolType::Symbols(symbols, default) => {
					let nextc = self.advance();
					if !self.scan_char(symbols, nextc) {
						self.current -= 1;
						self.add_token(*default);
					}
				}
				SymbolType::Function(f) => f(self),
			}
			true
		} else {
			false
		}
	}
}

#[derive(Clone)]
enum SymbolType<'a> {
	Just(TokenType),
	Function(fn(&mut CodeInfo)),
	Symbols(SymbolsMap<'a>, TokenType),
}

impl<'a> fmt::Debug for SymbolType<'a> {
	fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
		write!(
			f,
			"{}",
			match self {
				SymbolType::Just(kind) => format!("{kind:?}"),
				SymbolType::Function(_) => String::from("FUNCTION({...})"),
				SymbolType::Symbols(symbols, kind) => format!("({symbols:#?}, {kind:?})"),
			}
		)
	}
}

enum KeywordType {
	Just(TokenType),
	Lua(TokenType),
	Error(&'static str),
	Reserved(&'static str),
}

const SYMBOLS: SymbolsMap = generate_map(&[
	('(', SymbolType::Just(ROUND_BRACKET_OPEN)),
	(')', SymbolType::Just(ROUND_BRACKET_CLOSED)),
	('[', SymbolType::Just(SQUARE_BRACKET_OPEN)),
	(']', SymbolType::Just(SQUARE_BRACKET_CLOSED)),
	('{', SymbolType::Just(CURLY_BRACKET_OPEN)),
	('}', SymbolType::Just(CURLY_BRACKET_CLOSED)),
	(',', SymbolType::Just(COMMA)),
	(
		'.',
		SymbolType::Symbols(
			generate_map(&[(
				'.',
				SymbolType::Symbols(
					generate_map(&[
						('.', SymbolType::Just(THREEDOTS)),
						('=', SymbolType::Just(CONCATENATE)),
					]),
					TWODOTS,
				),
			)]),
			DOT,
		),
	),
	(';', SymbolType::Just(SEMICOLON)),
	(
		'+',
		SymbolType::Symbols(generate_map(&[('=', SymbolType::Just(INCREASE))]), PLUS),
	),
	(
		'-',
		SymbolType::Symbols(generate_map(&[('=', SymbolType::Just(DECREASE))]), MINUS),
	),
	(
		'*',
		SymbolType::Symbols(generate_map(&[('=', SymbolType::Just(MULTIPLY))]), STAR),
	),
	(
		'^',
		SymbolType::Symbols(
			generate_map(&[
				('=', SymbolType::Just(EXPONENTIATE)),
				('^', SymbolType::Just(BIT_XOR)),
			]),
			CARET,
		),
	),
	('#', SymbolType::Just(HASHTAG)),
	(
		'/',
		SymbolType::Symbols(
			generate_map(&[
				('=', SymbolType::Just(DIVIDE)),
				('_', SymbolType::Just(FLOOR_DIVISION)),
			]),
			SLASH,
		),
	),
	(
		'%',
		SymbolType::Symbols(
			generate_map(&[('=', SymbolType::Just(MODULATE))]),
			PERCENTUAL,
		),
	),
	(
		'!',
		SymbolType::Symbols(generate_map(&[('=', SymbolType::Just(NOT_EQUAL))]), NOT),
	),
	('~', SymbolType::Just(BIT_NOT)),
	(
		'=',
		SymbolType::Symbols(
			generate_map(&[
				('=', SymbolType::Just(EQUAL)),
				('>', SymbolType::Just(ARROW)),
			]),
			DEFINE,
		),
	),
	(
		'<',
		SymbolType::Symbols(
			generate_map(&[
				('=', SymbolType::Just(SMALLER_EQUAL)),
				('<', SymbolType::Just(LEFT_SHIFT)),
			]),
			SMALLER,
		),
	),
	(
		'>',
		SymbolType::Symbols(
			generate_map(&[
				('=', SymbolType::Just(BIGGER_EQUAL)),
				('>', SymbolType::Just(RIGHT_SHIFT)),
			]),
			BIGGER,
		),
	),
	(
		'?',
		SymbolType::Symbols(
			generate_map(&[
				(
					'=',
					SymbolType::Function(|i| {
						i.warning("'?=' is deprecated and was replaced with '&&='")
					}),
				),
				('>', SymbolType::Just(SAFE_EXPRESSION)),
				('.', SymbolType::Just(SAFEDOT)),
				(
					':',
					SymbolType::Function(|i| {
						if i.compare(':') {
							i.add_token(SAFE_DOUBLE_COLON);
						} else {
							i.current -= 1;
						}
					}),
				),
				('[', SymbolType::Just(SAFE_SQUARE_BRACKET)),
				(
					'?',
					SymbolType::Symbols(
						generate_map(&[('=', SymbolType::Just(DEFINE_COALESCE))]),
						COALESCE,
					),
				),
			]),
			QUESTION_MARK,
		),
	),
	(
		'&',
		SymbolType::Symbols(
			generate_map(&[(
				'&',
				SymbolType::Symbols(generate_map(&[('=', SymbolType::Just(DEFINE_AND))]), AND),
			)]),
			BIT_AND,
		),
	),
	(
		':',
		SymbolType::Symbols(
			generate_map(&[
				(':', SymbolType::Just(DOUBLE_COLON)),
				(
					'=',
					SymbolType::Function(|i| {
						i.warning("':=' is deprecated and was replaced with '||='")
					}),
				),
			]),
			COLON,
		),
	),
	(
		'|',
		SymbolType::Symbols(
			generate_map(&[(
				'|',
				SymbolType::Symbols(generate_map(&[('=', SymbolType::Just(DEFINE_OR))]), OR),
			)]),
			BIT_OR,
		),
	),
	('"', SymbolType::Function(|i| i.read_string('"'))),
	('\'', SymbolType::Function(|i| i.read_string('\''))),
	('`', SymbolType::Function(|i| i.read_raw_string())),
]);

lazy_static! {
	static ref KEYWORDS: AHashMap<&'static str, KeywordType> = AHashMap::from([
		(
			"and",
			KeywordType::Reserved("'and' operators in Clue are made with '&&'")
		),
		(
			"not",
			KeywordType::Reserved("'not' operators in Clue are made with '!'")
		),
		(
			"or",
			KeywordType::Reserved("'or' operators in Clue are made with '||'")
		),
		(
			"do",
			KeywordType::Reserved("'do ... end' blocks in Clue are made like this: '{ ... }'")
		),
		(
			"end",
			KeywordType::Reserved("code blocks in Clue are closed with '}'")
		),
		(
			"function",
			KeywordType::Reserved("functions in Clue are defined with the 'fn' keyword")
		),
		(
			"repeat",
			KeywordType::Reserved(
				"'repeat ... until x' loops in Clue are made like this: 'loop { ... } until x'"
			)
		),
		(
			"then",
			KeywordType::Reserved("code blocks in Clue are opened with '{'")
		),
		("if", KeywordType::Lua(IF)),
		("elseif", KeywordType::Lua(ELSEIF)),
		("else", KeywordType::Lua(ELSE)),
		("for", KeywordType::Lua(FOR)),
		("in", KeywordType::Lua(IN)),
		("while", KeywordType::Lua(WHILE)),
		("until", KeywordType::Lua(UNTIL)),
		("local", KeywordType::Lua(LOCAL)),
		("return", KeywordType::Lua(RETURN)),
		("true", KeywordType::Lua(TRUE)),
		("false", KeywordType::Lua(FALSE)),
		("nil", KeywordType::Lua(NIL)),
		("break", KeywordType::Lua(BREAK)),
		("of", KeywordType::Just(OF)),
		("with", KeywordType::Just(WITH)),
		("meta", KeywordType::Just(META)),
		("global", KeywordType::Just(GLOBAL)),
		("fn", KeywordType::Just(FN)),
		("method", KeywordType::Just(METHOD)),
		("loop", KeywordType::Just(LOOP)),
		("static", KeywordType::Just(STATIC)),
		("enum", KeywordType::Just(ENUM)),
		("continue", KeywordType::Just(CONTINUE)),
		("try", KeywordType::Just(TRY)),
		("catch", KeywordType::Just(CATCH)),
		("match", KeywordType::Just(MATCH)),
		("default", KeywordType::Just(DEFAULT)),
		(
			"macro",
			KeywordType::Error("'macro' is deprecated and was replaced with '@define'")
		),
		(
			"constructor",
			KeywordType::Error("'constructor' is reserved for Clue 4.0 and cannnot be used.")
		),
		(
			"struct",
			KeywordType::Error("'struct' is reserved for Clue 4.0 and cannot be used")
		),
		(
			"extern",
			KeywordType::Error("'extern' is reserved for Clue 4.0 and cannot be used")
		),
	]);
}

pub fn scan_code(code: Code, filename: &String) -> Result<Vec<Token>, String> {
	let mut i: CodeInfo = CodeInfo::new(code, filename);
	while !i.ended() && i.peek(0) != '\0' {
		i.start = i.current;
		let c = i.advance();
		if !i.scan_char(&SYMBOLS, c) {
			if c.is_whitespace() {
				continue;
			} else if c.is_ascii_digit() {
				if c == '0' {
					match i.peek(0) {
						'x' | 'X' => {
							i.current += 1;
							i.read_number(
								|c| {
									c.is_ascii_digit()
										|| ('a'..='f').contains(c) || ('A'..='F').contains(c)
								},
								false,
							);
						}
						'b' | 'B' => {
							i.current += 1;
							i.read_number(|&c| c == '0' || c == '1', false);
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
						KeywordType::Lua(kind) => *kind,
						KeywordType::Reserved(e) => i.reserved(&ident, e),
						_ if matches!(i.last, DOT | SAFEDOT | DOUBLE_COLON | SAFE_DOUBLE_COLON) => {
							IDENTIFIER
						}
						KeywordType::Just(kind) => *kind,
						KeywordType::Error(e) => {
							i.warning(*e);
							IDENTIFIER
						}
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
