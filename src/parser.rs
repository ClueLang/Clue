use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use self::ComplexToken::*;
use std::cmp;

/*macro_rules! parser_check {
	($i: expr, $tocheck: expr, $emsg: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(_) => return Err($i.error($emsg))
		}
	};

	($i: expr, $tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(_) => return Err($i.error(String::from("Something unexpected happened")))
		}
	};
}*/

struct Expression {
	tokens: Vec<ComplexToken>,
	line: u32
}

pub enum ComplexToken {
	VALUE {
		value: String,
		line: u32
	},
	CALL {
		name: Expression,
		args: Vec<Expression>,
		line: u32
	},
}

struct ParserInfo {
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	ctokens: Vec<ComplexToken>
}

impl ParserInfo {
	fn new(tokens: Vec<Token>, filename: String) -> ParserInfo {
		ParserInfo {
			current: 0,
			size: tokens.len(),
			tokens: tokens,
			filename: filename,
			ctokens: Vec::new()
		}
	}

	fn getLine(&self) -> u32 {
		self.at(self.current).line
	}

	fn error(&self, msg: String) -> String {
		println!("Error in file \"{}\" at line [{}]!", self.filename, self.getLine());
		msg
	}

	fn expected(&self, expected: &str, before: &str) -> String {
		self.error(format!("Expected token '{}' before '{}'.", expected, before))
	}

	fn unexpected(&self, str: &str) -> String {
		self.error(format!("Unexpected token '{}'.", str))
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> Token {
		self.tokens[cmp::min(pos, self.size - 1)].clone()
	}

	fn advance(&mut self) -> Token {
		let prev: Token = self.at(self.current);
		self.current += 1;
		prev
	}

	fn peekFar(&self, pos: usize) -> Token {
		let pos: usize = self.current + pos;
		self.at(pos)
	}

	fn peek(&self) -> Token {
		self.peekFar(0)
	}

	fn compare(&mut self, expected: TokenType) -> bool {
		if self.ended() {return false;}
		if self.at(self.current).kind != expected {return false;}
		true
	}

	fn buildCall(&mut self, name: Expression) -> Result<ComplexToken, String> {
		let mut args: Vec<Expression> = Vec::new();
		let mut start: usize;
		let mut pscope: u8 = 0;
		loop {
			self.current += 1;
			start = self.current;
			while self.compare(COMMA) {
				let t: Token = self.advance();
				match t.kind {
					ROUND_BRACKET_OPEN => {pscope += 1}
					ROUND_BRACKET_CLOSED => {
						if pscope == 0 {
							break;
						}
					}
					EOF => {return Err(self.expected(")", "<eof>"))}
					_ => {}
				}
			}
			args.push(self.buildExpression(start, self.current - 1)?);
			if pscope == 0 && self.compare(ROUND_BRACKET_CLOSED) {
				break;
			}
		}
		Ok(CALL {
			name: name,
			args: args,
			line: self.getLine()
		})
	}

	fn buildExpression(&mut self, start: usize, end: usize) -> Result<Expression, String> {
		let mut expr = Expression {
			tokens: Vec::new(),
			line: self.at(start).line
		};
		self.current = start;
		while self.current <= end {
			let t: Token = self.at(self.current);
			match t.kind {
				IDENTIFIER => {
					let line: u32 = self.getLine();
					if self.compare(ROUND_BRACKET_OPEN) {
						expr.tokens.push(self.buildCall(Expression {
							tokens: vec![VALUE {
								value: t.lexeme,
								line: line
							}],
							line: line
						})?);
					}
				},
				_ => {return Err(self.unexpected(t.lexeme.as_str()))}
			}
			self.current += 1;
		}
		Ok(expr)
	}
}

pub fn ParseTokens(tokens: Vec<Token>, filename: String) -> Result<Vec<ComplexToken>, String> {
	let mut i: ParserInfo = ParserInfo::new(tokens, filename);
	while !i.ended() {
		let t: Token = i.advance();
		match t.kind {
			IF => {

			},
			_ => {}
		}
	}
	Ok(i.ctokens)
}