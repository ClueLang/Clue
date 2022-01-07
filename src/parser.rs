use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use self::ComplexToken::*;

macro_rules! parser_check {
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
}

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
	line: u32,
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	ctokens: Vec<ComplexToken>
}

impl ParserInfo {
	fn new(tokens: Vec<Token>, filename: String) -> ParserInfo {
		ParserInfo {
			line: 1,
			current: 0,
			size: tokens.len(),
			tokens: tokens,
			filename: filename,
			ctokens: Vec::new()
		}
	}

	fn error(&self, msg: String) -> String {
		println!("Error in file \"{}\" at line [{}]!", self.filename, self.line);
		msg
	}

	fn unexpected(&self, str: &str) -> String {
		self.error(format!("Unexpected token '{}'.", str))
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> Result<Token, ()> {
		if pos >= self.size {return Err(());}
		Ok(self.tokens[pos].clone())
	}

	fn advance(&mut self) -> Result<Token, ()> {
		let prev: Token = self.at(self.current)?;
		self.current += 1;
		Ok(prev)
	}

	fn peekFar(&self, pos: usize) -> Result<Token, ()> {
		let pos: usize = self.current + pos;
		if pos >= self.size {return Err(());}
		self.at(pos)
	}

	fn peek(&self) -> Result<Token, ()> {
		self.peekFar(0)
	}

	fn compare(&mut self, expected: TokenType) -> Result<bool, ()> {
		if self.ended() {return Ok(false);}
		if self.at(self.current)?.kind != expected {return Ok(false);}
		self.current = self.current + 1;
		Ok(true)
	}

	fn buildCall(&mut self, name: Expression) -> Result<ComplexToken, String> {
		let call: ComplexToken = CALL {
			name: name,
			args: Vec::new(),
			line: self.line
		};

		Ok(call)
	}

	fn buildExpression(&mut self, start: usize, end: usize) -> Result<Expression, String> {
		let mut expr = Expression {
			tokens: Vec::new(),
			line: self.line
		};
		self.current = start;
		while self.current <= end {
			let t: Token = parser_check!(self, self.at(self.current));
			self.line = t.line;
			match t.kind {
				IDENTIFIER => {
					if parser_check!(self, self.peek()).kind == ROUND_BRACKET_OPEN {
						
						expr.tokens.push(self.buildCall(Expression {
							tokens: vec![VALUE {
								value: t.lexeme,
								line: self.line
							}],
							line: self.line
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
		let t: Token = parser_check!(i, i.advance());
		i.line = t.line;
		match t.kind {
			IF => {

			}
		}
	}
	Ok(i.ctokens)
}