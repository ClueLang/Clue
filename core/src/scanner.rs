//! The scanner is the second step of the compilation process, it takes the preprocessed source code and turns it
//! into a list of tokens
//!
//! It exposes a single function, [`scan_code`], which takes a [`Code`] and returns a [`Vec`] of [`Token`]

#![allow(non_camel_case_types)]
#![allow(clippy::upper_case_acronyms)]

use crate::{
	code::{Code, CodeChars},
	format_clue,
};

use self::TokenType::*;
use phf::phf_map;
use std::fmt;

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

type SymbolsMap = [Option<&'static SymbolType>; 94];

const fn generate_map(elements: &'static [(char, SymbolType)]) -> SymbolsMap {
	let mut map = [None; 94];
	let mut i = 0;
	while i < elements.len() {
		let (key, value) = &elements[i];
		let key = *key as usize;
		map[key - '!' as usize] = Some(value);
		i += 1;
	}
	map
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
#[rustfmt::skip]
/// The token's type, used to represent keywords, literals, symbols, etc...
pub enum TokenType {
	//symbols (NOTE: SAFE_* tokens must equal to their normal self + 6)
	ROUND_BRACKET_OPEN, ROUND_BRACKET_CLOSED, SQUARE_BRACKET_OPEN,
	SQUARE_BRACKET_CLOSED, CURLY_BRACKET_OPEN, CURLY_BRACKET_CLOSED,
	SAFE_CALL, COMMA, SAFE_SQUARE_BRACKET, SEMICOLON, QUESTION_MARK,
	DOT, DOUBLE_COLON, TWODOTS, COLON, THREEDOTS, ARROW, SAFE_DOT,
	SAFE_DOUBLE_COLON, NOT, BIT_NOT, STAR, SLASH, PERCENTUAL,
	PLUS, MINUS, LEFT_SHIFT, RIGHT_SHIFT, SMALLER, SMALLER_EQUAL,
	BIGGER, BIGGER_EQUAL, EQUAL, NOT_EQUAL, BIT_AND, BIT_XOR, BIT_OR,
	AND, OR, FLOOR_DIVISION, CARET, HASHTAG, COALESCE,
	DEFINE, DEFINE_AND, DEFINE_OR, INCREASE, DECREASE, MULTIPLY, DIVIDE,
	DEFINE_COALESCE, EXPONENTIATE, CONCATENATE, MODULATE,

	//literals
	IDENTIFIER, NUMBER, STRING,

	//keywords
	IF, ELSEIF, ELSE, FOR, OF, IN, WITH, WHILE, META, GLOBAL, UNTIL,
	LOCAL, FN, METHOD, RETURN, TRUE, FALSE, NIL, LOOP, STATIC, ENUM,
	CONTINUE, BREAK, TRY, CATCH, MATCH, DEFAULT, STRUCT, EXTERN, CONSTRUCTOR,

	EOF,
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// Represents a token with its type, its literal string and the location in the file.
/// The type is represented by a [`TokenType`]
pub struct Token {
	/// The token's type.
	pub kind: TokenType,

	/// The literal token, e.g. for `1` it's `"1"`, for `local` it's `"local"` and for `+` it's `"+"`.
	pub lexeme: String,

	/// The line where the token is located.
	pub line: usize,

	/// The column where the token is located.
	pub column: usize,
}

impl Token {
	/// Creates a new [`Token`] given its [`TokenType`], its literal token, the line and column where it is located.
	/// The literal token is the literal value of the token, e.g. for `1` it's `"1"`, for `local` it's `"local"` and for `+` it's `"+"`.
	pub fn new(kind: TokenType, lexeme: impl Into<String>, line: usize, column: usize) -> Self {
		Self {
			kind,
			lexeme: lexeme.into(),
			line,
			column,
		}
	}

	pub fn is_op(&self) -> bool {
		self.kind >= NOT && self.kind <= OR
	}
}

/// A token that has a raw pointer to a [`Token`].
pub struct BorrowedToken {
	token: *const Token,
}

impl BorrowedToken {
	/// Creates a new [`BorrowedToken`] from the raw pointer to a [`Token`].
	pub const fn new(token: *const Token) -> Self {
		Self { token }
	}

	/// Returns the token
	pub const fn token(&self) -> &Token {
		// SAFETY: This is safe because the pointer is guaranteed to be valid.
		unsafe { &(*self.token) }
	}

	/// Returns the [`TokenType`].
	pub const fn kind(&self) -> TokenType {
		self.token().kind
	}

	/// Returns the `clone`d literal token.
	pub fn lexeme(&self) -> String {
		self.token().lexeme.clone()
	}

	/// Returns the line where the token is located.
	pub const fn line(&self) -> usize {
		self.token().line
	}

	/// Returns the column where the token is located.
	pub const fn column(&self) -> usize {
		self.token().column
	}

	/// Clones the inner [`Token`] and returns it.
	pub fn into_owned(&self) -> Token {
		self.token().clone()
	}
}

struct CodeInfo<'a> {
	line: usize,
	column: usize,
	start: usize,
	current: usize,
	size: usize,
	code: CodeChars,
	read: Vec<(char, usize, usize)>,
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
		read.push((code.next_unwrapped(), code.line(), code.column()));
		read.push((code.next_unwrapped(), code.line(), code.column()));
		Self {
			line: 1,
			column: 1,
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
		self.read.push((
			self.code.next_unwrapped(),
			self.code.line(),
			self.code.column(),
		));
		let (prev, line, ..) = self.read[self.current];
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
		self.tokens
			.push(Token::new(kind, literal, self.line, self.column));
	}

	fn add_token(&mut self, kind: TokenType) {
		let lexeme: String = self.substr(self.start, self.current);
		self.last = kind;
		self.tokens
			.push(Token::new(kind, lexeme, self.line, self.column));
	}

	fn warning(&mut self, message: impl Into<String>) {
		eprintln!(
			"Error in {}:{}:{}!\nError: \"{}\"\n",
			self.filename,
			self.line,
			self.column,
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
			self.advance();
			self.advance();
		} else if llcheck == "UL" {
			if self.peek(2) == 'L' {
				self.advance();
				self.advance();
				self.advance();
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
		let Some(c) = (c as usize).checked_sub('!' as usize) else {
			return false;
		};
		if let Some(Some(token)) = symbols.get(c) {
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

	fn update_column(&mut self) {
		self.column = self.read[self.current].2
	}
}

#[derive(Clone)]
enum SymbolType {
	Just(TokenType),
	Function(fn(&mut CodeInfo)),
	Symbols(SymbolsMap, TokenType),
}

impl fmt::Debug for SymbolType {
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
				('.', SymbolType::Just(SAFE_DOT)),
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
				('(', SymbolType::Just(SAFE_CALL)),
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

static KEYWORDS: phf::Map<&'static [u8], KeywordType> = phf_map! {
	b"and" => KeywordType::Reserved("'and' operators in Clue are made with '&&'"),
	b"not" => KeywordType::Reserved("'not' operators in Clue are made with '!'"),
	b"or" => KeywordType::Reserved("'or' operators in Clue are made with '||'"),
	b"do" => KeywordType::Reserved("'do ... end' blocks in Clue are made like this: '{ ... }'"),
	b"end" => KeywordType::Reserved("code blocks in Clue are closed with '}'"),
	b"function" => KeywordType::Reserved("functions in Clue are defined with the 'fn' keyword"),
	b"repeat" => KeywordType::Reserved(
		"'repeat ... until x' loops in Clue are made like this: 'loop { ... } until x'"
	),
	b"then" => KeywordType::Reserved("code blocks in Clue are opened with '{'"),
	b"if" => KeywordType::Lua(IF),
	b"elseif" => KeywordType::Lua(ELSEIF),
	b"else" => KeywordType::Lua(ELSE),
	b"for" => KeywordType::Lua(FOR),
	b"in" => KeywordType::Lua(IN),
	b"while" => KeywordType::Lua(WHILE),
	b"until" => KeywordType::Lua(UNTIL),
	b"local" => KeywordType::Lua(LOCAL),
	b"return" => KeywordType::Lua(RETURN),
	b"true" => KeywordType::Lua(TRUE),
	b"false" => KeywordType::Lua(FALSE),
	b"nil" => KeywordType::Lua(NIL),
	b"break" => KeywordType::Lua(BREAK),
	b"of" => KeywordType::Just(OF),
	b"with" => KeywordType::Just(WITH),
	b"meta" => KeywordType::Just(META),
	b"global" => KeywordType::Just(GLOBAL),
	b"fn" => KeywordType::Just(FN),
	b"method" => KeywordType::Just(METHOD),
	b"loop" => KeywordType::Just(LOOP),
	b"static" => KeywordType::Just(STATIC),
	b"enum" => KeywordType::Just(ENUM),
	b"continue" => KeywordType::Just(CONTINUE),
	b"try" => KeywordType::Just(TRY),
	b"catch" => KeywordType::Just(CATCH),
	b"match" => KeywordType::Just(MATCH),
	b"default" => KeywordType::Just(DEFAULT),
	b"constructor" => KeywordType::Error("'constructor' is reserved for Clue 4.0 and cannnot be used."),
	b"struct" => KeywordType::Error("'struct' is reserved for Clue 4.0 and cannot be used"),
	b"extern" =>KeywordType::Error("'extern' is reserved for Clue 4.0 and cannot be used"),
};

/// Scans the code and returns a [`Vec`] of [`Token`]s
/// It takes a preprocessed code and a filename as arguments
///
/// # Errors
/// If the code is invalid, it will return an [`Err`] with the error message
///
/// # Examples
/// ```
/// use clue_core::{env::Options, preprocessor::*, scanner::*};
///
/// fn main() -> Result<(), String> {
///     let options = Options::default();
///     let filename = String::from("fizzbuzz.clue");
///     let mut code = include_str!("../../examples/fizzbuzz.clue").to_owned();
///
///     let (codes, variables, ..) = preprocess_code(
///         unsafe { code.as_bytes_mut() },
///         1,
///         false,
///         &filename,
///         &options,
///     )?;
///     let codes = preprocess_codes(0, codes, &variables, &filename)?;
///     let tokens = scan_code(codes, &filename)?;
///
///     Ok(())
/// }
/// ```
pub fn scan_code(code: Code, filename: &String) -> Result<Vec<Token>, String> {
	let mut i: CodeInfo = CodeInfo::new(code, filename);
	while !i.ended() && i.peek(0) != '\0' {
		i.start = i.current;
		i.update_column();
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
				let kind = if let Some(keyword) = KEYWORDS.get(ident.as_bytes()) {
					match keyword {
						KeywordType::Lua(kind) => *kind,
						KeywordType::Reserved(e) => i.reserved(&ident, e),
						_ if matches!(
							i.last,
							DOT | SAFE_DOT | DOUBLE_COLON | SAFE_DOUBLE_COLON
						) =>
						{
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

#[cfg(test)]
mod tests {
	use super::TokenType::*;

	macro_rules! assert_safe_token {
		($normal:ident, $safe:ident) => {
			assert_eq!($normal as u8, ($safe as u8).wrapping_sub(6))
		};
	}

	#[test]
	fn check_safe_tokens() {
		assert_safe_token!(ROUND_BRACKET_OPEN, SAFE_CALL);
		assert_safe_token!(SQUARE_BRACKET_OPEN, SAFE_SQUARE_BRACKET);
		assert_safe_token!(DOT, SAFE_DOT);
		assert_safe_token!(DOUBLE_COLON, SAFE_DOUBLE_COLON);
	}
}
