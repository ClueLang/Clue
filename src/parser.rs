#![allow(non_camel_case_types)]

use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::scanner::TokenType::*;
use self::ComplexToken::*;
use self::CheckResult::*;
use std::cmp;

pub type Expression = Vec<ComplexToken>;

fn GetCondition(t: TokenType) -> CheckResult {
	match t {
		CURLY_BRACKET_OPEN => CHECK_FORCESTOP,
		_ => CHECK_CONTINUE
	}
}

#[derive(Debug, Clone)]
pub enum ComplexToken {
	SYMBOL {
		lexeme: String,
		line: usize
	},

	VARIABLE {
		local: bool,
		names: Vec<String>,
		values: Vec<Expression>,
		line: usize
	},

	ALTER {
		kind: TokenType,
		names: Vec<Expression>,
		values: Vec<Expression>,
		line: usize
	},

	PSEUDO {
		num: u8,
		line: usize
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

	FOR_LOOP {
		iterator: String,
		start: Expression,
		end: Expression,
		alter: Expression,
		code: Expression
	},

	FOR_FUNC_LOOP {
		iterators: Vec<String>,
		expr: Expression,
		code: Expression
	},

	CALL(Vec<Expression>),
	PGET(Expression),
	DO_BLOCK(Expression),
	RETURN_EXPR(Expression),
	CONTINUE_LOOP, BREAK_LOOP
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

	fn getLine(&self) -> usize {
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
					EOF => {return Err(self.expectedBefore(";", "<end>"))}
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
				EOF => {return Err(self.expectedBefore("}", "<end>"))}
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
					name = Ok(vec![SYMBOL {
						line: pn.line,
						lexeme: pn.lexeme.clone()
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
						EOF => {return Err(self.expectedBefore("]", "<end>"))}
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
				EOF => {return Err(self.expectedBefore("}", "<end>"))}
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
			NUMBER | IDENTIFIER | STRING | DOLLAR | PROTECTED_GET | TRUE | FALSE | MINUS |
			NIL | NOT | HASHTAG | ROUND_BRACKET_OPEN | SQUARE_BRACKET_OPEN => false,
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
		expr.push(SYMBOL {
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
					EOF => {return Err(self.expectedBefore("]", "<end>"))}
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
					EOF => {return Err(self.expectedBefore(")", "<end>"))}
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
				EQUAL | NOT_EQUAL | BIGGER | BIGGER_EQUAL | SMALLER | SMALLER_EQUAL |
				BIT_AND | BIT_OR | BIT_XOR | BIT_NOT | LEFT_SHIFT | RIGHT_SHIFT => {
					self.checkOperator(&t, start, end)?;
					expr.push(SYMBOL {
						lexeme: t.lexeme,
						line: t.line
					})
				}
				MINUS => {
					self.checkOperator(&t, usize::MAX, end)?;
					expr.push(SYMBOL {
						lexeme: t.lexeme,
						line: t.line
					})
				}
				HASHTAG => {
					if match self.peek(0).kind {
						IDENTIFIER | CURLY_BRACKET_OPEN | ROUND_BRACKET_OPEN => false,
						_ => true
					} {
						return Err(self.expected("<table>", &self.peek(0).lexeme))
					}
					expr.push(SYMBOL {
						lexeme: String::from("#"),
						line: t.line
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
					expr.push(SYMBOL {
						lexeme: String::from("and"),
						line: t.line
					})
				}
				OR => {
					self.checkOperator(&t, start, end)?;
					expr.push(SYMBOL {
						lexeme: String::from("or"),
						line: t.line
					})
				}
				NOT => {
					self.checkOperator(&t, usize::MAX, end)?;
					expr.push(SYMBOL {
						lexeme: String::from("not"),
						line: t.line
					})
				}
				NUMBER | TRUE | FALSE | NIL => {
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push(SYMBOL {
							lexeme: t.lexeme,
							line: t.line
						})
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				STRING => {
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push(SYMBOL {
							lexeme: format!("[[{}]]", t.lexeme),
							line: t.line
						})
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				ROUND_BRACKET_OPEN => {
					expr.push(SYMBOL {
						lexeme: String::from("("),
						line: t.line
					});
					expr.append(&mut self.buildDelimitedExpression(false)?);
					expr.push(SYMBOL {
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
					let args: Vec<String>;
					if !self.advanceIf(ROUND_BRACKET_CLOSED) {
						args = self.buildIdentifierList()?;
						self.assert(ROUND_BRACKET_CLOSED, ")")?;
					} else {
						args = Vec::new();
					}
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
					expr.push(SYMBOL {
						line: self.getLine(),
						lexeme: t.lexeme
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
					qexpr.push(SYMBOL {
						lexeme: String::from("]"),
						line: t.line
					});
					qexpr.insert(0, SYMBOL {
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
				EOF => {return Err(self.expectedBefore("}", "<end>"))}
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
					let args: Vec<String>;
					if !i.advanceIf(ROUND_BRACKET_CLOSED) {
						args = i.buildIdentifierList()?;
						i.assert(ROUND_BRACKET_CLOSED, ")")?;
					} else {
						args = Vec::new();
					}
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
				let areinit = match check.kind {
					DEFINE => true,
					SEMICOLON => false,
					_ => {return Err(i.expected("=", &check.lexeme))}
				};
				let mut values: Vec<Expression> = if !areinit {Vec::new()} else {
					i.findExpressions(0, 0, 0, 0, |t| {
						match t {
							COMMA => CHECK_BUILD,
							SEMICOLON => CHECK_STOP,
							_ => CHECK_CONTINUE
						}
					})?
				};
				let mut it: usize = 0;
				for _ in names.iter() {
					match values.get(it) {
						Some(_) => {},
						None => {
							values.push(vec![SYMBOL {
								line: i.getLine(),
								lexeme: String::from("nil")
							}])
						}
					};
					it += 1;
				}
				i.expr.push(VARIABLE {
					local: t.kind == LOCAL,
					line: t.line,
					names, values
				});
				if areinit {
					i.current += 1;
				}
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
				i.expr.push(ALTER {
					kind: checkt.kind,
					line: t.line,
					names, values
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
						EOF => {return Err(i.expectedBefore(")", "<end>"))}
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
								condition: vec![SYMBOL {
									lexeme: String::from("true"),
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
			FOR => {
				if i.peek(1).kind == DEFINE {
					let iterator = i.assertAdvance(IDENTIFIER, "<name>")?.lexeme;
					i.current += 1;
					let start = i.findExpression(0, 0, 0, 0, |t| {
						match t {
							COMMA => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					let end = i.findExpression(0, 0, 0, 1, |t| {
						match t {
							CURLY_BRACKET_OPEN => CHECK_FORCESTOP,
							COMMA => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					i.current -= 1;
					let alter = if i.advance().kind != CURLY_BRACKET_OPEN {
						i.findExpression(0, 0, 0, 1, GetCondition)?
					} else {
						vec![SYMBOL {
							line: i.getLine(),
							lexeme: String::from("1")
						}]
					};
					let code = i.buildCodeBlock()?;
					i.expr.push(FOR_LOOP {iterator, start, end, alter, code})
				} else {
					let iterators = i.buildIdentifierList()?;
					let expr = match i.advance().kind {
						OF => {
							let line = i.getLine();
							let mut expr = vec![SYMBOL {
								line,
								lexeme: String::from("pairs")
							}, SYMBOL {
								lexeme: String::from("("),
								line
							}];
							expr.append(&mut i.findExpression(0, 0, 0, 1, GetCondition)?);
							expr.push(SYMBOL {
								lexeme: String::from(")"),
								line: i.getLine()
							});
							expr
						}
						IN => {
							let line = i.getLine();
							let mut expr = vec![SYMBOL {
								line,
								lexeme: String::from("ipairs")
							}, SYMBOL {
								lexeme: String::from("("),
								line
							}];
							expr.append(&mut i.findExpression(0, 0, 0, 1, GetCondition)?);
							expr.push(SYMBOL {
								lexeme: String::from(")"),
								line: i.getLine()
							});
							expr
						}
						WITH => {i.findExpression(0, 0, 0, 1, GetCondition)?}
						_ => {return Err(i.expected("of', 'in' or 'with", &i.peek(0).lexeme))}
					};
					let code = i.buildCodeBlock()?;
					i.expr.push(FOR_FUNC_LOOP {iterators, expr, code});
				}
			}
			CONTINUE => {i.expr.push(CONTINUE_LOOP); i.advanceIf(SEMICOLON);}
			BREAK => {i.expr.push(BREAK_LOOP); i.advanceIf(SEMICOLON);}
			RETURN => {
				let expr = i.findExpression(0, 0, 0, 0, |t| {
					match t {
						SEMICOLON => CHECK_FORCESTOP,
						_ => CHECK_CONTINUE,
					}
				})?;
				i.expr.push(RETURN_EXPR (expr));
			}
			_ => {return Err(i.unexpected(t.lexeme.as_str()))}
		}
	}
	Ok(i.expr)
}