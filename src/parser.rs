#![allow(non_camel_case_types)]

use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use self::ComplexToken::*;
use self::CheckResult::*;
use std::cmp;

type Expression = Vec<ComplexToken>;

fn GetCondition(t: TokenType) -> CheckResult {
	match t {
		CURLY_BRACKET_OPEN => CHECK_FORCESTOP,
		_ => CHECK_CONTINUE
	}
}

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

	PSEUDO {
		num: u8,
		line: u32
	},

	TABLE {
		values: Vec<(Expression, Expression)>,
		metas: Vec<(String, Expression)>
	},

	FUNCTION {
		local: bool,
		name: String,
		args: Vec<String>,
		code: Expression
	},

	LAMBDA {
		args: Vec<String>,
		code: Expression
	},

	IF_STATEMENT {
		condition: Expression,
		code: Expression
	},

	WHILE_LOOP {
		condition: Expression,
		code: Expression
	},

	LOOP_UNTIL {
		condition: Expression,
		code: Expression
	},

	CALL(Vec<Expression>),
	PGET(Expression),
	DO_BLOCK(Expression)
}

enum CheckResult {
	CHECK_CONTINUE,
	CHECK_BUILD,
	CHECK_STOP,
	CHECK_BACKSTOP,
	CHECK_FORCESTOP,
}

fn IsVar(current: usize, end: usize, nt: &Token) -> bool {
	current >= end || nt.isOp() || nt.kind == ROUND_BRACKET_CLOSED
}

struct ParserInfo {
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	expr: Expression,
	testing: bool
}

impl ParserInfo {
	fn new(tokens: Vec<Token>, filename: String) -> ParserInfo {
		ParserInfo {
			current: 0,
			size: tokens.len() - 1,
			tokens: tokens,
			filename: filename,
			expr: Vec::new(),
			testing: false
		}
	}

	fn getLine(&self) -> u32 {
		self.at(self.current - 1).line
	}

	fn error(&self, msg: String) -> String {
		if !self.testing {
			println!("Error in file \"{}\" at line [{}]!", self.filename, self.getLine());
		}
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
		self.tokens[cmp::min(pos, self.size)].to_owned()
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

	fn assertAdvance(&mut self, expected: TokenType, error: &str) -> Result<Token, String> {
		let t = self.advance();
		if t.kind != expected {
			return Err(self.expected(error, &t.lexeme))
		}
		Ok(t)
	}

	fn assert(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.advanceIf(expected) {
			return Err(self.expected(error, &self.peek(0).lexeme))
		}
		Ok(())
	}

	fn buildCall(&mut self) -> Result<ComplexToken, String> {
		self.current += 2;
		let args = self.findExpressions(1, 0, 0, 1, |t| {
			match t {
				COMMA => CHECK_BUILD,
				ROUND_BRACKET_CLOSED => CHECK_STOP,
				_ => CHECK_CONTINUE
			}
		})?;
		Ok(CALL(args))
	}

	/*fn findExpression(&mut self, scope: u8, check: impl Fn(TokenType) -> CheckResult) -> Result<(Expression, bool), String> {
		let expr: Expression;
		let mut pscope = scope;
		let mut qscope = 0u8;
		let mut cscope = 0u8;
		let start = self.current;
		loop {
			let t = self.peek(0).kind;
			match t {
				ROUND_BRACKET_OPEN => {pscope += 1}
				SQUARE_BRACKET_OPEN => {qscope += 1}
				CURLY_BRACKET_OPEN => {cscope += 1}
				ROUND_BRACKET_CLOSED => {
					if pscope == 0 {return Err(self.expectedBefore("(", ")"))}
					pscope -= 1;
				}
				SQUARE_BRACKET_CLOSED => {
					if qscope == 0 {return Err(self.expectedBefore("[", "]"))}
					qscope -= 1;
				}
				CURLY_BRACKET_CLOSED => {
					if cscope == 0 {return Err(self.expectedBefore("{", "}"))}
					cscope -= 1;
				}
				EOF => {return Err(self.expectedBefore(";", "<eof>"))}
				_ => {}
			}
			let allscope = pscope + qscope + cscope;
			if pscope + qscope + cscope <= scope {
				let result = check(t);
				match result {
					CHECK_CONTINUE => {}
					CHECK_BUILD => {
						expr = self.buildExpression(start, self.current)?;
						self.current += 1;
						return Ok((expr, false));
					}
				}
			}
			self.current += 1;
		}
	}*/

	fn findExpressions(&mut self, mut pscope: u8, mut qscope: u8, mut cscope: u8, scope: u8, check: impl Fn(TokenType) -> CheckResult) -> Result<Vec<Expression>, String> {
		let mut values: Vec<Expression> = Vec::new();
		let mut stop = false;
		loop {
			let start = self.current;
			loop {
				let t = self.peek(0).kind;
				match t {
					ROUND_BRACKET_OPEN => {pscope += 1}
					SQUARE_BRACKET_OPEN => {qscope += 1}
					CURLY_BRACKET_OPEN => {cscope += 1}
					ROUND_BRACKET_CLOSED => {
						if pscope == 0 {return Err(self.expectedBefore("(", ")"))}
						pscope -= 1;
					}
					SQUARE_BRACKET_CLOSED => {
						if qscope == 0 {return Err(self.expectedBefore("[", "]"))}
						qscope -= 1;
					}
					CURLY_BRACKET_CLOSED => {
						if cscope == 0 {return Err(self.expectedBefore("{", "}"))}
						cscope -= 1;
					}
					EOF => {return Err(self.expectedBefore(";", "<eof>"))}
					_ => {}
				}
				let allscope = pscope + qscope + cscope;
				if allscope <= scope {
					match check(t) {
						CHECK_CONTINUE => {}
						CHECK_BUILD => {
							values.push(self.buildExpression(start, self.current)?);
							self.current += 1;
							break;
						}
						CHECK_STOP => {
							let isexpr = self.current > start;
							if allscope == 0 && isexpr {
								values.push(self.buildExpression(start, self.current)?);
								stop = true;
								break;
							} else if !isexpr {
								stop = true;
								break;
							}
						}
						CHECK_BACKSTOP => {
							let isexpr = self.current > start;
							if allscope == 0 && isexpr {
								values.push(self.buildExpression(start, self.current - 1)?);
								self.current += 1;
								stop = true;
								break;
							} else if !isexpr {
								stop = true;
								break;
							}
						}
						CHECK_FORCESTOP => {
							if self.current > start {
								values.push(self.buildExpression(start, self.current)?);
								self.current += 1;
								stop = true;
								break;
							} else {
								stop = true;
								break;
							}
						}
					}
				}
				self.current += 1;
			}
			if stop {break}
		}
		Ok(values)
	}

	fn findExpression(&mut self, pscope: u8, qscope: u8, cscope: u8, scope: u8, check: impl Fn(TokenType) -> CheckResult) -> Result<Expression, String> {
		match self.findExpressions(pscope, qscope, cscope, scope, check)?.pop() {
			Some(expr) => Ok(expr),
			None => {
				let t = self.peek(0).lexeme;
				Err(self.expected("<expr>", &t))
			}
		}
	}

	fn buildTable(&mut self) -> Result<ComplexToken, String> {
		let mut values: Vec<(Expression, Expression)> = Vec::new();
		let mut metas: Vec<(String, Expression)> = Vec::new();
		loop {
			if self.advanceIf(CURLY_BRACKET_CLOSED) {break}
			let start = self.current;
			let mut qscope = 1u8;
			let mut iskey = false;
			while match self.peek(0).kind {
				CURLY_BRACKET_OPEN => {
					qscope += 1;
					true
				}
				CURLY_BRACKET_CLOSED => {
					qscope -= 1;
					qscope != 0
				}
				COMMA => qscope != 1,
				DEFINE => {
					iskey = true;
					false
				}
				EOF => {return Err(self.expectedBefore("}", "<eof>"))}
				_ => true
			} {
				self.current += 1;
			}
			if !iskey {
				values.push((Expression::new(), self.buildExpression(start, self.current)?));
				self.advanceIf(COMMA);
				continue
			}
			self.current = start;
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
				SQUARE_BRACKET_OPEN => {
					let mut qscope = 1u8;
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
		let nt: TokenType = self.peek(0).kind;
		if match nt {
			NUMBER | IDENTIFIER | STRING | DOLLAR | PROTECTED_GET | TRUE | FALSE |
			NIL | NOT | ROUND_BRACKET_OPEN | SQUARE_BRACKET_OPEN => false,
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
				PLUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS |
				BIT_AND | BIT_OR | BIT_XOR | BIT_NOT | LEFT_SHIFT | RIGHT_SHIFT => {
					self.checkOperator(&t, start, end)?;
					expr.push(CHAR {
						kind: t.kind,
						lexeme: t.lexeme,
						line: self.getLine()
					})
				}
				MINUS => {
					self.checkOperator(&t, usize::MAX, end)?;
					expr.push(CHAR {
						kind: t.kind,
						lexeme: t.lexeme,
						line: self.getLine()
					})
				}
				PROTECTED_GET => {
					if self.peek(0).kind == ROUND_BRACKET_OPEN {
						expr.push(PGET(self.buildDelimitedExpression(false)?));
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
					self.checkOperator(&t, usize::MAX, end)?;
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
					expr.append(&mut fname);
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
				FN => {
					self.assert(ROUND_BRACKET_OPEN, "(")?;
					let args = self.buildIdentifierList()?;
					self.assert(ROUND_BRACKET_CLOSED, ")")?;
					self.assert(CURLY_BRACKET_OPEN, "{")?;
					let code = self.buildCodeBlock()?;
					expr.push(LAMBDA {args, code});
				}
				_ => {return Err(self.unexpected(t.lexeme.as_str()))}
			}
		}
		if expr.len() == 0 {
			return Err(self.expected("<expr>", self.at(end).lexeme.as_str()))
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
					let nt = self.peek(0);
					if nt.kind == IDENTIFIER {
						return Err(self.unexpected(&nt.lexeme))
					}
					expr.push(VALUE {
						kind: IDENTIFIER,
						line: self.getLine(),
						value: t.lexeme
					})
				}
				DOT => {self.checkIndex(&t, &mut expr)?}
				METHOD => {
					if !dofuncs {return Err(self.error(String::from("You can't call methods here.")))}
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
					self.current -= 2;
					expr.push(self.buildCall()?);
					self.current += 1;
				}
				_ => {break}
			}
		}
		Ok(expr)
	}

	fn buildCodeBlock(&mut self) -> Result<Expression, String> {
		let mut tokens: Vec<Token> = Vec::new();
		let mut cscope = 1u8;
		loop {
			let t = self.advance();
			match t.kind {
				CURLY_BRACKET_OPEN => {cscope += 1}
				CURLY_BRACKET_CLOSED => {
					cscope -= 1;
					if cscope == 0 {break}
				}
				EOF => {return Err(self.expectedBefore("}", "<eof>"))}
				_ => {}
			}
			tokens.push(t);
		}
		if tokens.is_empty() {Ok(Expression::new())} else {
			tokens.push(Token {
				kind: EOF,
				lexeme: String::new(),
				line: self.getLine()
			});
			ParseTokens(tokens, self.filename.clone())
		}
	}

	fn buildIdentifierList(&mut self) -> Result<Vec<String>, String> {
		let mut idents: Vec<String> = Vec::new();
		while {
			let t = self.assertAdvance(IDENTIFIER, "<name>")?;
			idents.push(t.lexeme);
			self.advanceIf(COMMA)
		} {}
		Ok(idents)
	}
}

pub fn ParseTokens(tokens: Vec<Token>, filename: String) -> Result<Expression, String> {
	let mut i: ParserInfo = ParserInfo::new(tokens, filename);
	while !i.ended() {
		let t = i.advance();
		match t.kind {
			LOCAL | GLOBAL => {
				if i.advanceIf(FN) {
					let name = i.assertAdvance(IDENTIFIER, "<name>")?.lexeme;
					i.assert(ROUND_BRACKET_OPEN, "(")?;
					let args = i.buildIdentifierList()?;
					i.assert(ROUND_BRACKET_CLOSED, ")")?;
					i.assert(CURLY_BRACKET_OPEN, "{")?;
					let code = i.buildCodeBlock()?;
					i.expr.push(FUNCTION {
						local: t.kind == LOCAL,
						name, args, code
					});
					continue
				}
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
				let values: Vec<Expression> = if !areInit {Vec::new()} else {
					i.findExpressions(0, 0, 0, 0, |t| {
						match t {
							COMMA => CHECK_BUILD,
							SEMICOLON => CHECK_STOP,
							_ => CHECK_CONTINUE
						}
					})?
				};
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
				let start = i.current;
				i.current -= 1;
				i.testing = true;
				let testexpr = i.findExpression(0, 0, 0, 0, |t| {
					match t {
						SEMICOLON => CHECK_BACKSTOP,
						_ => CHECK_CONTINUE,
					}
				});
				i.testing = false;
				match testexpr {
					Ok(mut expr) => {i.expr.append(&mut expr); continue}
					Err(_) => {i.current = start}
				}
				let mut names: Vec<Expression> = Vec::new();
				while {
					names.push(i.buildIdentifier(false)?);
					i.current += 1;
					i.lookBack(1).kind == COMMA
				} {}
				i.current -= 1;
				let checkt = i.lookBack(0);
				let check = checkt.kind.clone() as u8;
				if check < DEFINE as u8 || check > CONCATENATE as u8 {
					return Err(i.expected("=", &checkt.lexeme))
				}
				let values: Vec<Expression> = i.findExpressions(0, 0, 0, 0, |t| {
					match t {
						COMMA => CHECK_BUILD,
						SEMICOLON => CHECK_STOP,
						_ => CHECK_CONTINUE
					}
				})?;
				if names.len() != values.len() {
					return Err(i.expectedBefore("<expr>", &i.peek(0).lexeme))
				}
				let mut tuples: Vec<(Expression, Expression)> = Vec::new();
				let mut it: usize = 0;
				for name in names.iter() {
					tuples.push((name.clone(), values.get(it).unwrap().clone()));
					it += 1;
				}
				i.expr.push(ALTER {
					kind: checkt.kind,
					values: tuples
				});
				i.current += 1;
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
				let fname = &mut i.buildExpression(start, i.current - 1)?;
				let call = i.buildCall()?;
				i.expr.append(fname);
				i.expr.push(call);
				i.current += 1;
				i.advanceIf(SEMICOLON);
			}
			CURLY_BRACKET_OPEN => {
				let block = i.buildCodeBlock()?;
				i.expr.push(DO_BLOCK(block));
			}
			IF => {
				let condition = i.findExpression(0, 0, 0, 1, GetCondition)?;
				let code = i.buildCodeBlock()?;
				i.expr.push(IF_STATEMENT {condition, code})
			}
			WHILE => {
				let condition = i.findExpression(0, 0, 0, 1, GetCondition)?;
				let code = i.buildCodeBlock()?;
				i.expr.push(WHILE_LOOP {condition, code})
			}
			LOOP => {
				match i.advance().kind {
					CURLY_BRACKET_OPEN => {
						let code = i.buildCodeBlock()?;
						if i.peek(0).kind == UNTIL {
							i.current += 1;
							let condition = i.findExpression(0, 0, 0, 0, |t| {
								match t {
									SEMICOLON => CHECK_FORCESTOP,
									_ => CHECK_CONTINUE,
								}
							})?;
							i.expr.push(LOOP_UNTIL {condition, code})
						} else {
							i.expr.push(WHILE_LOOP {
								condition: vec![VALUE {
									value: String::from("true"),
									kind: TRUE,
									line: t.line
								}],
								code
							})
						}
					}
					UNTIL => {
						let condition = i.findExpression(0, 0, 0, 1, GetCondition)?;
						let code = i.buildCodeBlock()?;
						i.expr.push(LOOP_UNTIL {condition, code})
					}
					_ => {return Err(i.unexpected("loop"))}
				}
			}
			_ => {return Err(i.unexpected(t.lexeme.as_str()))}
		}
	}
	Ok(i.expr)
}