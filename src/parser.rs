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
		values: Vec<(Expression, Expression)>
	},

	CHAR {
		kind: TokenType,
		lexeme: String,
		line: u32,
	},

	CALL {
		name: Expression,
		args: Vec<Expression>,
	},

	FUNCTION {
		args: Vec<String>,
		code: Expression
	},

	PSEUDO {
		num: u8,
		line: u32
	},

	TABLE {
		values: Vec<(Expression, Expression)>,
		metas: Vec<(String, Expression)>
	},

	PGET (Expression)
}

fn IsVar(current: usize, end: usize, nt: &Token) -> bool {
	current >= end || nt.isOp() || nt.kind == ROUND_BRACKET_CLOSED
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
		self.error(format!("Expected '{}', got '{}'", expected, got))
	}

	fn expectedBefore(&self, expected: &str, before: &str) -> String {
		self.error(format!("Expected token '{}' before '{}'", expected, before))
	}

	fn unexpected(&self, str: &str) -> String {
		self.error(format!("Unexpected token '{}'", str))
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> Token {
		self.tokens[cmp::min(pos, self.size)].clone()
	}

	fn advance(&mut self) -> Token {
		self.current += 1;
		self.lookBack(0)
	}

	fn peek(&self, pos: usize) -> Token {
		let pos: usize = self.current + pos;
		self.at(pos)
	}

	fn lookBack(&self, pos: usize) -> Token {
		let pos: usize = self.current - pos - 1;
		self.at(pos)
	}

	fn compare(&self, expected: TokenType) -> bool {
		if self.ended() {return false;}
		if self.peek(0).kind != expected {return false;}
		true
	}

	fn advanceIf(&mut self, expected: TokenType) -> bool {
		if self.ended() {return false;}
		if self.peek(0).kind != expected {return false;}
		self.current += 1;
		true
	}

	fn buildCall(&mut self, name: Expression) -> Result<ComplexToken, String> {
		let mut args: Vec<Expression> = Vec::new();
		let mut start: usize = self.current + 2;
		let mut pscope: u8 = 0;
		let mut ended: bool = false;
		loop {
			loop {
				let t = self.advance();
				match t.kind {
					ROUND_BRACKET_OPEN => {pscope += 1}
					ROUND_BRACKET_CLOSED => {
						if pscope == 0 {continue}
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
					return Err(self.error(String::from("Invalid empty function argument found")))
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

	fn findExpressions(&mut self) -> Result<Vec<Expression>, String> {
		let mut values: Vec<Expression> = Vec::new();
		loop {
			let start = self.current;
			while match self.peek(0).kind {
				COMMA | SEMICOLON => false,
				EOF => {return Err(self.expectedBefore(";", "<eof>"))}
				_ => true
			} {
				self.current += 1;
			}
			println!("a {} {}", start, self.current);
			values.push(self.buildExpression(start, self.current)?);
			println!("{:#?}", self.peek(0));
			match self.peek(0).kind {
				COMMA => {self.current += 1}
				SEMICOLON => {break}
				_ => {}
			}
		}
		Ok(values)
	}

	fn buildTable(&mut self) -> Result<ComplexToken, String> {
		let mut values: Vec<(Expression, Expression)> = Vec::new();
		let mut metas: Vec<(String, Expression)> = Vec::new();
		loop {
			let name: Result<Expression, String>;
			let pn = self.advance();
			match pn.kind {
				IDENTIFIER => {
					name = Ok(vec![VALUE {
						kind: pn.kind,
						line: pn.line,
						value: pn.lexeme.clone()
					}]);
				}
				CURLY_BRACKET_CLOSED => {break}
				SQUARE_BRACKET_OPEN => {
					let mut qscope: u8 = 1;
					let start = self.current;
					while match self.advance().kind {
						SQUARE_BRACKET_OPEN => {qscope += 1; true}
						SQUARE_BRACKET_CLOSED => {
							qscope -= 1;
							match qscope {
								0 => false,
								_ => true
							}
						}
						EOF => {return Err(self.expectedBefore("]", "<eof>"))}
						_ => true
					} {}
					name = Ok(self.buildExpression(start, self.current - 1)?);
					self.current += 1;
				}
				META => {
					name = Err(String::from(match self.advance().lexeme.as_ref() {
						"index" => "__index",
						"newindex" => "__newindex",
						"mode" => "__mode",
						"call" => "__call",
						"metatable" => "__metatable",
						"tostring" => "__tostring",
						"gc" => "__gc",
						"name" => "__name",
						"unm" | "unary" => "__unm",
						"add" | "+" => "__add",
						"sub" | "-" => "__sub",
						"mul" | "*" => "__mul",
						"div" | "/" => "__div",
						"mod" | "%" => "__mod",
						"pow" | "^" => "__pow",
						"concat" | ".." => "__concat",
						"eq" | "equal" | "==" => "__eq",
						"lt" | "less_than" | "<" => "__lt",
						"le" | "less_than_equal" | "<=" => "__le",
						_ => {return Err(self.expected("<meta name>", &self.lookBack(0).lexeme))}
					}))
				}
				_ => {return Err(self.expected("<name>", &pn.lexeme))}
			}
			if !self.advanceIf(DEFINE) {
				return Err(self.expected("=", &self.peek(0).lexeme))
			}
			let start = self.current;
			let mut cscope = 0u8;
			while match self.peek(0).kind {
				COMMA | CURLY_BRACKET_CLOSED => {
					if cscope == 0 {false} else {true}
				}
				ROUND_BRACKET_OPEN => {cscope += 1; true}
				ROUND_BRACKET_CLOSED => {
					if cscope == 0 {return Err(self.expectedBefore("(", ")"))}
					cscope -= 1;
					true
				}
				EOF => {return Err(self.expectedBefore("}", "<eof>"))}
				_ => true
			} {
				self.current += 1;
			}
			match name {
				Ok(n) => {values.push((n, self.buildExpression(start, self.current)?))}
				Err(n) => {metas.push((n, self.buildExpression(start, self.current)?))}
			}
			self.advanceIf(COMMA);
		}
		Ok(TABLE {values, metas})
	}

	fn checkOperator(&self, t: &Token, start: usize, end: usize) -> Result<(), String> {
		if self.current - 1 == start {
			return Err(self.error(format!("Operator '{}' not expected at the start of expression", t.lexeme)))
		}
		if self.current == end {
			return Err(self.error(format!("Operator '{}' not expected at the end of expression", t.lexeme)))
		}
		let pt: TokenType = self.lookBack(1).kind;
		let nt: TokenType = self.peek(0).kind;
		if match pt {
			NUMBER | IDENTIFIER | STRING | DOLLAR | TRUE | FALSE | NIL |
			ROUND_BRACKET_CLOSED | SQUARE_BRACKET_CLOSED => false,
			_ => true
		} {
			return Err(self.error(format!("Operator '{}' has invalid left hand token", t.lexeme)))
		}
		if match nt {
			NUMBER | IDENTIFIER | STRING | DOLLAR | PROTECTED_GET | TRUE | FALSE | NIL |
			ROUND_BRACKET_OPEN | SQUARE_BRACKET_OPEN => false,
			_ => true
		} {
			return Err(self.error(format!("Operator '{}' has invalid right hand token", t.lexeme)))
		}
		Ok(())
	}

	fn checkIndex(&self, t: &Token, expr: &mut Expression) -> Result<(), String> {
		if self.peek(0).kind != IDENTIFIER || match self.lookBack(0).kind {
			IDENTIFIER | SQUARE_BRACKET_CLOSED => true,
			_ => false
		} {
			return Err(self.error(format!("'{}' should be used only when indexing tables.", t.lexeme)))
		}
		expr.push(CHAR {
			kind: t.kind,
			lexeme: t.lexeme.clone(),
			line: self.getLine()
		});
		Ok(())
	}

	fn buildDelimitedExpression(&mut self, square: bool) -> Result<Expression, String> {
		self.current -= 1;
		let mut pscope = 0u8;
		let pstart = self.current + 1;
		if square {
			loop {
				let nt = self.advance().kind;
				match nt {
					SQUARE_BRACKET_OPEN => {pscope += 1}
					SQUARE_BRACKET_CLOSED => {
						pscope -= 1;
						if pscope == 0 {break}
					}
					EOF => {return Err(self.expectedBefore("]", "<eof>"))}
					_ => {}
				}
			}
		} else {
			loop {
				let nt = self.advance().kind;
				match nt {
					ROUND_BRACKET_OPEN => {pscope += 1}
					ROUND_BRACKET_CLOSED => {
						pscope -= 1;
						if pscope == 0 {break}
					}
					EOF => {return Err(self.expectedBefore(")", "<eof>"))}
					_ => {}
				}
			}
		}
		self.buildExpression(pstart, self.current - 1)
	}

	fn buildExpression(&mut self, start: usize, end: usize) -> Result<Expression, String> {
		println!("{} {}", start, end);
		let mut expr = Expression::new();
		self.current = start;
		while self.current < end {
			let t = self.advance();
			match t.kind {
				IDENTIFIER => {
					let mut fname = self.buildIdentifier(true)?;
					self.current -= 1;
					if IsVar(self.current, end, &self.peek(0)) {
						expr.append(&mut fname);
					} else {return Err(self.unexpected(t.lexeme.as_str()))}
				}
				CURLY_BRACKET_OPEN => {expr.push(self.buildTable()?)}
				PLUS | MINUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS |
				BIT_AND | BIT_OR | BIT_XOR | BIT_NOT | LEFT_SHIFT | RIGHT_SHIFT => {
					self.checkOperator(&t, start, end)?;
					expr.push(CHAR {
						kind: t.kind,
						lexeme: t.lexeme,
						line: self.getLine()
					})
				}
				PROTECTED_GET => {
					if self.peek(0).kind == ROUND_BRACKET_OPEN {
						expr.push(PGET (self.buildDelimitedExpression(false)?));
						self.current += 1;
					} else {
						return Err(self.unexpected("?>"))
					}
				}
				AND => {
					self.checkOperator(&t, start, end)?;
					expr.push(CHAR {
						kind: t.kind,
						lexeme: String::from("and"),
						line: self.getLine()
					})
				}
				OR => {
					self.checkOperator(&t, start, end)?;
					expr.push(CHAR {
						kind: t.kind,
						lexeme: String::from("or"),
						line: self.getLine()
					})
				}
				NOT => {
					self.checkOperator(&t, start, end)?; //todo: fix this little thing
					expr.push(CHAR {
						kind: t.kind,
						lexeme: String::from("not"),
						line: self.getLine()
					})
				}
				NUMBER | STRING | TRUE | FALSE | NIL => {
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push(VALUE {
							value: t.lexeme,
							kind: t.kind,
							line: self.getLine()
						})
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				ROUND_BRACKET_OPEN => {
					expr.push(CHAR {
						kind: ROUND_BRACKET_OPEN,
						lexeme: String::from("("),
						line: t.line
					});
					expr.append(&mut self.buildDelimitedExpression(false)?);
					expr.push(CHAR {
						kind: ROUND_BRACKET_CLOSED,
						lexeme: String::from(")"),
						line: self.getLine()
					});
					self.current += 2;
					let mut fname = self.buildIdentifier(true)?;
					if IsVar(self.current, end, &self.peek(0)) {
						expr.append(&mut fname);
					} else {return Err(self.unexpected(t.lexeme.as_str()))}
					self.current -= 1;
				}
				DOLLAR => {
					let nt = self.peek(0);
					let mut num: u8 = 1;
					if self.current != end && nt.kind == NUMBER {
						num = nt.lexeme.parse().unwrap();
						self.current += 1;
					}
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push(PSEUDO {
							num,
							line: self.getLine()
						});
					} else {
						return Err(self.unexpected(&nt.lexeme))
					}
				}
				_ => {return Err(self.unexpected(t.lexeme.as_str()))}
			}
		}
		if expr.len() == 0 {
			return Err(self.unexpected(self.at(end).lexeme.as_str()))
		}
		Ok(expr)
	}

	fn buildIdentifier(&mut self, dofuncs: bool) -> Result<Expression, String> {
		let mut expr = Expression::new();
		self.current -= 1;
		loop {
			let t = self.advance();
			match t.kind {
				IDENTIFIER => {
					expr.push(VALUE {
						kind: IDENTIFIER,
						line: self.getLine(),
						value: t.lexeme
					})
				}
				DOT => {self.checkIndex(&t, &mut expr)?}
				METHOD => {
					self.checkIndex(&t, &mut expr)?;
					if self.peek(1).kind != ROUND_BRACKET_OPEN {
						return Err(self.expected("(", &self.peek(1).lexeme))
					}
				}
				SQUARE_BRACKET_OPEN => {
					let mut qexpr: &mut Expression = &mut self.buildDelimitedExpression(true)?;
					qexpr.push(CHAR {
						kind: SQUARE_BRACKET_CLOSED,
						lexeme: String::from("]"),
						line: t.line
					});
					qexpr.insert(0, CHAR {
						kind: SQUARE_BRACKET_OPEN,
						lexeme: String::from("["),
						line: t.line
					});
					expr.append(&mut qexpr);
					self.current += 1;
				}
				ROUND_BRACKET_OPEN => {
					if !dofuncs {return Err(self.error(String::from("You can't call functions here.")))}
					let mut fname = Expression::new();
					fname.append(&mut expr);
					self.current -= 2;
					expr.push(self.buildCall(fname)?);
					self.current += 1;
				}
				_ => {break}
			}
		}
		Ok(expr)
	}
}

pub fn ParseTokens(tokens: Vec<Token>, filename: String) -> Result<Expression, String> {
	let mut i: ParserInfo = ParserInfo::new(tokens, filename);
	while !i.ended() {
		let t = i.advance();
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
				let values: Vec<Expression> = if !areInit {Vec::new()} else {i.findExpressions()?};
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
				let expr: Expression = Vec::new();

				/*let line: u32 = i.getLine();
				let p = i.peek(0);
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
						if i.compare(SEMICOLON) {i.current += 1}
					}
					_ => {
						let mut names: Vec<Expression> = Vec::new();
						loop {
							let name: Expression = vec![VALUE {
								value: t.lexeme,
								kind: IDENTIFIER,
								line: line
							}];

						}
						/*names.push(vec![VALUE {
							value: t.lexeme,
							kind: IDENTIFIER,
							line: line
						}]);
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
						}*/
						let checkt = i.advance();
						let check = checkt.kind.clone() as u8;
						if check < DEFINE as u8 || check > CONCATENATE as u8 {
							return Err(i.expected("=", &checkt.lexeme))
						}
						drop(check);
						let values: Vec<Expression> = i.findExpressions()?;
						if names.len() != values.len() {
							return Err(i.expectedBefore("<expr>", &i.peek(0).lexeme))
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
				}*/
			}
			ROUND_BRACKET_OPEN => {
				i.current -= 1;
				let start = i.current + 1;
				let mut pscope: u8 = 0;
				loop {
					let nt = i.advance().kind;
					match nt {
						ROUND_BRACKET_OPEN => {pscope += 1}
						ROUND_BRACKET_CLOSED => {
							pscope -= 1;
							if pscope == 0 {break}
						}
						EOF => {return Err(i.expectedBefore(")", "<eof>"))}
						_ => {}
					}
				}
				let fname = i.buildExpression(start, i.current - 1)?;
				let call = i.buildCall(fname)?;
				i.expr.push(call);
				i.current += 1;
				i.advanceIf(SEMICOLON);
			}
			_ => {return Err(i.unexpected(t.lexeme.as_str()))}
		}
	}
	Ok(i.expr)
}