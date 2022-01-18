use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use self::ComplexToken::*;
use std::cmp;

type Expression = Vec<ComplexToken>;

#[derive(Debug, Clone)]
pub enum ComplexToken {
	VALUE {
		value: String,
		kind: TokenType,
		line: u32
	},

	VARIABLE {
		local: bool,
		values: Vec<(String, Expression)>
	},

	ALTER {
		kind: TokenType,
		values: Vec<(String, Expression)>
	},

	CHAR {
		kind: TokenType,
		line: u32,
	},

	CALL {
		name: Expression,
		args: Vec<Expression>,
	},
}

struct ParserInfo {
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	expr: Expression
}

impl ParserInfo {
	fn new(tokens: Vec<Token>, filename: String) -> ParserInfo {
		ParserInfo {
			current: 0,
			size: tokens.len() - 1,
			tokens: tokens,
			filename: filename,
			expr: Vec::new()
		}
	}

	fn getLine(&self) -> u32 {
		self.at(self.current - 1).line
	}

	fn error(&self, msg: String) -> String {
		println!("Error in file \"{}\" at line [{}]!", self.filename, self.getLine());
		msg
	}

	fn expected(&self, expected: &str, got: &str) -> String {
		self.error(format!("Expected '{}', got '{}'.", expected, got))
	}

	fn expectedBefore(&self, expected: &str, before: &str) -> String {
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
		let prev: Token = self.peek();
		self.current += 1;
		prev
	}

	fn peekFar(&self, pos: usize) -> Token {
		let pos: usize = self.current + pos;
		self.at(pos)
	}

	fn lookBack(&self, pos: usize) -> Token {
		let pos: usize = self.current - pos - 1;
		self.at(pos)
	}

	fn peek(&self) -> Token {
		self.peekFar(0)
	}

	fn compare(&mut self, expected: TokenType) -> bool {
		if self.ended() {return false;}
		if self.peek().kind != expected {return false;}
		true
	}

	fn buildCall(&mut self, name: Expression) -> Result<ComplexToken, String> {
		let mut args: Vec<Expression> = Vec::new();
		let mut start: usize = self.current + 2;
		let mut pscope: u8 = 0;
		let mut ended: bool = false;
		loop {
			loop {
				let t: Token = self.advance();
				match t.kind {
					ROUND_BRACKET_OPEN => {pscope += 1}
					ROUND_BRACKET_CLOSED => {
						pscope -= 1;
						if pscope == 0 {
							ended = true;
							self.current -= 1;
							break;
						}
					}
					COMMA => {
						if pscope > 1 {continue}
						self.current -= 1;
						break;
					}
					EOF => {return Err(self.expectedBefore(")", "<eof>"))}
					_ => {}
				}
			}
			if start == self.current {
				if args.len() > 0 {
					return Err(self.error(String::from("Invalid empty function argument found.")))
				}
				break
			}
			args.push(self.buildExpression(start, self.current)?);
			if ended {
				break;
			}
			self.current += 1;
			start = self.current;
		}
		Ok(CALL {
			name: name,
			args: args,
		})
	}

	fn buildExpression(&mut self, start: usize, end: usize) -> Result<Expression, String> {
		let mut expr = Expression::new();
		let mut pscope: u8 = 0;
		let mut qscope: u8 = 0;
		self.current = start;
		while self.current < end {
			let t: Token = self.advance();
			match t.kind {
				IDENTIFIER => {
					let line: u32 = self.getLine();
					if self.compare(ROUND_BRACKET_OPEN) {
						self.current -= 1;
						let mut fname = Expression::new();
						fname.push(VALUE {
							value: t.lexeme,
							kind: IDENTIFIER,
							line: line
						});
						expr.push(self.buildCall(fname)?);
						self.current += 1;
					} else if self.current == end || self.peek().isOp() {
						expr.push(VALUE {
							value: t.lexeme,
							kind: IDENTIFIER,
							line: line
						})
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				PLUS | MINUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS => {
					if self.current - 1 == start {
						return Err(self.error(format!("Operator '{}' not expected at the start of expression.", t.lexeme)))
					}
					if self.current == end {
						return Err(self.error(format!("Operator '{}' not expected at the end of expression.", t.lexeme)))
					}
					let pt: TokenType = self.lookBack(1).kind;
					let nt: TokenType = self.peek().kind ;
					if pt == TRUE || nt == TRUE || pt == FALSE || nt == FALSE {
						return Err(self.error(format!("Operator '{}' cannot operate with booleans.", t.lexeme)))
					}
					if pt != NUMBER && pt != IDENTIFIER && pt != STRING && pt != CURLY_BRACKET_CLOSED && pt != SQUARE_BRACKET_CLOSED && pt != NEW {
						return Err(self.error(format!("Operator '{}' has invalid left hand token.", t.lexeme)))
					}
					if nt != NUMBER && nt != IDENTIFIER && pt != STRING && nt != CURLY_BRACKET_OPEN && pt != CURLY_BRACKET_CLOSED {
						return Err(self.error(format!("Operator '{}' has invalid right hand token.", t.lexeme)))
					}
					expr.push(CHAR {
						kind: t.kind,
						line: self.getLine()
					})
				}
				NUMBER | STRING | TRUE | FALSE | NIL => {
					if self.current - 1 == start || self.current == end || self.peek().isOp() {
						expr.push(VALUE {
							value: t.lexeme,
							kind: t.kind,
							line: self.getLine()
						})
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				CURLY_BRACKET_OPEN => {
					pscope += 1;
					expr.push(CHAR {
						kind: CURLY_BRACKET_OPEN,
						line: self.getLine()
					})
				}
				CURLY_BRACKET_CLOSED => {
					if pscope == 0 {
						return Err(self.unexpected(")"))
					}
					pscope -= 1;
					expr.push(CHAR {
						kind: CURLY_BRACKET_CLOSED,
						line: self.getLine()
					})
				}
				SQUARE_BRACKET_OPEN => {
					qscope += 1;
					expr.push(CHAR {
						kind: SQUARE_BRACKET_OPEN,
						line: self.getLine()
					})
				}
				SQUARE_BRACKET_CLOSED => {
					if qscope == 0 {
						return Err(self.unexpected("]"))
					}
					qscope -= 1;
					expr.push(CHAR {
						kind: SQUARE_BRACKET_CLOSED,
						line: self.getLine()
					})
				}
				_ => {return Err(self.unexpected(t.lexeme.as_str()))}
			}
		}
		if expr.len() == 0 {
			return Err(self.unexpected(self.at(end).lexeme.as_str()))
		}
		if pscope > 0 {
			return Err(self.expectedBefore(")", self.at(end).lexeme.as_str()))
		}
		if qscope > 0 {
			return Err(self.expectedBefore(")", self.at(end).lexeme.as_str()))
		}
		Ok(expr)
	}
}

pub fn ParseTokens(tokens: Vec<Token>, filename: String) -> Result<Expression, String> {
	let mut i: ParserInfo = ParserInfo::new(tokens, filename);
	while !i.ended() {
		let t: Token = i.advance();
		match t.kind {
			LOCAL | GLOBAL => {
				let mut names: Vec<String> = Vec::new();
				loop {
					let pname = i.advance();
					if pname.kind != IDENTIFIER {
						return Err(i.expected("<name>", &pname.lexeme));
					}
					names.push(pname.lexeme);
					if !i.compare(COMMA) {
						break
					}
					i.current += 1;
				}
				let check = i.advance();
				let areInit: bool;
				match check.kind {
					DEFINE => {areInit = true},
					SEMICOLON => {areInit = false},
					_ => {return Err(i.expected("=", &check.lexeme))}
				};
				drop(check);
				let mut values: Vec<Expression> = Vec::new();
				if areInit {
					loop {
						let start = i.current;
						while match i.peek().kind {
							COMMA | SEMICOLON => false,
							_ => true
						} {
							i.current += 1;
						}
						values.push(i.buildExpression(start, i.current)?);
						match i.peek().kind {
							COMMA => {i.current += 1}
							SEMICOLON => {break}
							_ => {}
						}
					}
				}
				let mut tuples: Vec<(String, Expression)> = Vec::new();
				let mut it: usize = 0;
				for name in names.iter() {
					tuples.push((String::from(name), match values.get(it) {
						Some(value) => value.clone(),
						None => {
							let mut default = Expression::new();
							default.push(VALUE {
								kind: NIL,
								line: i.getLine(),
								value: "nil".to_string()
							});
							default
						}
					}));
					it += 1;
				}
				i.expr.push(VARIABLE {
					local: t.kind == LOCAL,
					values: tuples
				});
				i.current += 1;
			}
			IDENTIFIER => {
				let line: u32 = i.getLine();
				let p = i.peek();
				match p.kind {
					ROUND_BRACKET_OPEN => {
						i.current -= 1;
						let mut fname = Expression::new();
						fname.push(VALUE {
							value: t.lexeme,
							kind: IDENTIFIER,
							line: line
						});
						let call = i.buildCall(fname)?;
						i.expr.push(call);
						i.current += 1;
					}
					_ => {
						let mut names: Vec<String> = Vec::new();
						names.push(t.lexeme);
						loop {
							if !i.compare(COMMA) {
								break
							}
							i.current += 1;
							let pname = i.advance();
							if pname.kind != IDENTIFIER {
								return Err(i.expected("<name>", &pname.lexeme));
							}
							names.push(pname.lexeme);
						}
						let checkt = i.advance();
						let check = checkt.kind.clone() as u8;
						if check < DEFINE as u8 || check > CONCATENATE as u8 {
							return Err(i.expected("=", &checkt.lexeme))
						}
						drop(check);
						let mut values: Vec<Expression> = Vec::new();
						loop {
							let start = i.current;
							while match i.peek().kind {
								COMMA | SEMICOLON => false,
								_ => true
							} {
								i.current += 1;
							}
							values.push(i.buildExpression(start, i.current)?);
							match i.peek().kind {
								COMMA => {i.current += 1}
								SEMICOLON => {break}
								_ => {}
							}
						}
						if names.len() != values.len() {
							return Err(i.expectedBefore("<expr>", &i.peek().lexeme))
						}
						let mut tuples: Vec<(String, Expression)> = Vec::new();
						let mut it: usize = 0;
						for name in names.iter() {
							tuples.push((String::from(name), values.get(it).unwrap().clone()));
							it += 1;
						}
						i.expr.push(ALTER {
							kind: checkt.kind,
							values: tuples
						});
						i.current += 1;
					}
				}
			}
			_ => {return Err(i.unexpected(t.lexeme.as_str()))}
		}
	}
	Ok(i.expr)
}