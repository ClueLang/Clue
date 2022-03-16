#![allow(non_camel_case_types)]

use crate::{
	bar::{UpdateBar, progressbar},
	scanner::{
		Token,
		TokenType,
		TokenType::*
	},
	compiler::CompileTokens,
	ENV_JITBIT,
	ENV_CONTINUE,
	finaloutput
};
use self::{
	ComplexToken::*,
	CheckResult::*
};
use std::{
	collections::LinkedList,
	cmp
};

macro_rules! expression {
    ($($x: expr),*) => {
        {
            let mut expr = Expression::new();
            $(expr.push_back($x);)*
            expr
        }
    };
}

pub type Expression = LinkedList<ComplexToken>;
pub type FunctionArgs = Vec<(String, Option<Expression>)>;

#[derive(Debug, Clone, PartialEq)]
pub enum ComplexToken {
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

	TABLE {
		values: Vec<(Expression, Expression)>,
		metas: Vec<(String, Expression)>
	},

	FUNCTION {
		local: bool,
		name: Expression,
		args: FunctionArgs,
		code: CodeBlock,
	},

	LAMBDA {
		args: FunctionArgs,
		code: CodeBlock
	},

	IF_STATEMENT {
		condition: Expression,
		code: CodeBlock,
		next: Option<Box<ComplexToken>>,
	},

	WHILE_LOOP {
		condition: Expression,
		code: CodeBlock
	},

	LOOP_UNTIL {
		condition: Expression,
		code: CodeBlock
	},

	FOR_LOOP {
		iterator: String,
		start: Expression,
		end: Expression,
		alter: Expression,
		code: CodeBlock
	},

	FOR_FUNC_LOOP {
		iterators: Vec<String>,
		expr: Expression,
		code: CodeBlock
	},

	TRY_CATCH {
		totry: CodeBlock,
		catch: Option<CodeBlock>,
		error: Option<String>
	},

	IDENT {
		expr: Expression,
		line: usize
	},

	SYMBOL(String),
	PSEUDO(usize),
	CALL(Vec<Expression>),
	EXPR(Expression),
	DO_BLOCK(CodeBlock),
	RETURN_EXPR(Option<Expression>),
	CONTINUE_LOOP, BREAK_LOOP
}

enum CheckResult {
	CHECK_CONTINUE,
	CHECK_BUILD,
	CHECK_STOP,
	CHECK_BACKSTOP,
	CHECK_FORCESTOP,
	CHECK_ADVANCESTOP
}

fn IsVar(current: usize, end: usize, nt: &Token) -> bool {
	current >= end || nt.isOp() || nt.kind == ROUND_BRACKET_CLOSED
}

fn GetCondition(t: TokenType) -> CheckResult {
	match t {
		CURLY_BRACKET_OPEN => CHECK_FORCESTOP,
		_ => CHECK_CONTINUE
	}
}

fn PrependToOutput(string: String) {
	unsafe {finaloutput = string + &finaloutput}
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock {
	pub start: usize,
	pub code: Expression,
	pub end: usize
}

struct ParserInfo {
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	expr: Expression,
	testing: bool,
	ternaryid: u8
}

impl ParserInfo {
	fn new(tokens: Vec<Token>, filename: String) -> ParserInfo {
		ParserInfo {
			current: 0,
			size: tokens.len() - 1,
			tokens: tokens,
			filename: filename,
			expr: Expression::new(),
			testing: false,
			ternaryid: 0
		}
	}

	fn getLine(&self) -> usize {
		self.at(self.current - 1).line
	}

	fn error(&self, msg: String) -> String {
		if !self.testing {
			println!("\nError in file \"{}\" at line {}!", self.filename, self.getLine());
		}
		msg
	}

	fn expected(&self, expected: &str, got: &str) -> String {
		self.error(format!("Expected '{}', got '{}'", expected, got))
	}

	fn expectedBefore(&self, expected: &str, before: &str) -> String {
		self.error(format!("Expected '{}' before '{}'", expected, before))
	}

	fn unexpected(&self, str: &str) -> String {
		self.error(format!("Unexpected token '{}'", str))
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> Token {
		unsafe {progressbar = self.current;}
		UpdateBar(700);
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

	fn assertCompare(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.compare(expected) {
			return Err(self.expected(error, &self.peek(0).lexeme))
		}
		Ok(())
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
					SAFE_SQUARE_BRACKET | SQUARE_BRACKET_OPEN => {qscope += 1}
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
						CHECK_ADVANCESTOP => {
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
						CHECK_FORCESTOP => {
							if self.current > start {
								values.push(self.buildExpression(start, self.current)?);
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

	fn getExpression(&mut self, pscope: u8, qscope: u8, cscope: u8, scope: u8, check: impl Fn(TokenType) -> CheckResult) -> Result<Expression, String> {
		match self.findExpressions(pscope, qscope, cscope, scope, check)?.pop() {
			Some(expr) => Ok(expr),
			None => Ok(Expression::new())
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
					name = Ok(expression![SYMBOL(pn.lexeme.clone())]);
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
					self.current = start;
					name = Ok(self.buildName()?);
					self.current -= 1;
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
			NUMBER | IDENTIFIER | STRING | DOLLAR | PROTECTED_GET | TRUE | FALSE | MINUS | BIT_NOT |
			NIL | NOT | HASHTAG | ROUND_BRACKET_OPEN | SQUARE_BRACKET_OPEN | TREDOTS => false,
			_ => true
		} {
			return Err(self.error(format!("Operator '{}' has invalid right hand token", t.lexeme)))
		}
		Ok(())
	}

	fn buildBitwiseOp(&mut self, t: Token, start: usize, end: usize, expr: &mut Expression, fname: &str) -> Result<(), String> {
		self.checkOperator(&t, start, end)?;
		if let Some(bit) = arg!(&ENV_JITBIT) {
			let mut arg1 = Expression::new();
			arg1.append(expr);
			let arg2 = self.buildExpression(self.current, end)?;
			expr.push_back(SYMBOL(format!("{}.{}", bit, fname)));
			expr.push_back(CALL(vec![arg1, arg2]));
		} else {
			expr.push_back(SYMBOL(t.lexeme))
		}
		Ok(())
	}

	fn checkIndex(&self, t: &Token, expr: &mut Expression, lexeme: &str) -> Result<(), String> {
		if !self.compare(IDENTIFIER) || match self.lookBack(0).kind {
			IDENTIFIER | SQUARE_BRACKET_CLOSED => true,
			_ => false
		} {
			return Err(self.error(format!("'{}' should be used only when indexing", t.lexeme)))
		}
		expr.push_back(SYMBOL(lexeme.to_string()));
		Ok(())
	}

	fn buildExpression(&mut self, start: usize, end: usize) -> Result<Expression, String> {
		let mut expr = Expression::new();
		self.current = start;
		while self.current < end {
			let t = self.advance();
			match t.kind {
				IDENTIFIER => {
					let fname = self.buildIdentifier()?;
					self.current -= 1;
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push_back(fname);
					} else {return Err(self.unexpected(t.lexeme.as_str()))}
				}
				CURLY_BRACKET_OPEN => {expr.push_back(self.buildTable()?)}
				PLUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS |
				EQUAL | BIGGER | BIGGER_EQUAL | SMALLER | SMALLER_EQUAL => {
					self.checkOperator(&t, start, end)?;
					expr.push_back(SYMBOL(t.lexeme))
				}
				MINUS => {
					self.checkOperator(&t, usize::MAX, end)?;
					expr.push_back(SYMBOL(t.lexeme))
				}
				BIT_AND => self.buildBitwiseOp(t, start, end, &mut expr, "band")?,
				BIT_OR => self.buildBitwiseOp(t, start, end, &mut expr, "bor")?,
				BIT_XOR => self.buildBitwiseOp(t, start, end, &mut expr, "bxor")?,
				BIT_NOT => {
					self.checkOperator(&t, usize::MAX, end)?;
					if let Some(bit) = arg!(&ENV_JITBIT) {
						let arg = self.buildExpression(self.current, end)?;
						expr.push_back(SYMBOL(bit.clone() + ".bnot"));
						expr.push_back(CALL(vec![arg]));
					} else {
						expr.push_back(SYMBOL(t.lexeme))
					}
				}
				LEFT_SHIFT => self.buildBitwiseOp(t, start, end, &mut expr, "lshift")?,
				RIGHT_SHIFT => self.buildBitwiseOp(t, start, end, &mut expr, "rshift")?,
				NOT_EQUAL => {
					self.checkOperator(&t, start, end)?;
					expr.push_back(SYMBOL(String::from("~=")))
				}
				HASHTAG => {
					if match self.peek(0).kind {
						IDENTIFIER | CURLY_BRACKET_OPEN | ROUND_BRACKET_OPEN => false,
						_ => true
					} {
						return Err(self.expected("<table>", &self.peek(0).lexeme))
					}
					expr.push_back(SYMBOL(String::from("#")))
				}
				/*PROTECTED_GET => {
					self.assert(ROUND_BRACKET_OPEN, "(")?;
					self.current += 1;
					expr.push_back(PGET(self.buildIdentifier(true)?));
				}*/
				AND => {
					self.checkOperator(&t, start, end)?;
					expr.push_back(SYMBOL(String::from(" and ")))
				}
				OR => {
					self.checkOperator(&t, start, end)?;
					expr.push_back(SYMBOL(String::from(" or ")))
				}
				NOT => {
					self.checkOperator(&t, usize::MAX, end)?;
					expr.push_back(SYMBOL(String::from("not ")))
				}
				TERNARY_THEN => {
					let mut condition = Expression::new();
					condition.append(&mut expr);
					let exprtrue = self.findExpression(0, 0, 0, 0, |t| {
						match t {
							TERNARY_ELSE => CHECK_ADVANCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					let t2 = self.lookBack(0);
					let exprfalse = self.buildExpression(self.current, end)?;
					let name = format!("_t{}", self.ternaryid);
					self.expr.push_back(VARIABLE {
						line: t.line,
						local: true,
						names: vec![name.clone()],
						values: Vec::new()
					});
					let name = SYMBOL(name);
					self.expr.push_back(IF_STATEMENT {
						condition,
						code: CodeBlock {
							start: self.at(start).line,
							code: expression![ALTER {
								kind: DEFINE,
								line: t.line,
								names: vec![expression![name.clone()]],
								values: vec![exprtrue]
							}],
							end: t2.line
						},
						next: Some(Box::new(DO_BLOCK(CodeBlock {
							start: t2.line,
							code: expression![ALTER {
								kind: DEFINE,
								line: t.line,
								names: vec![expression![name.clone()]],
								values: vec![exprfalse]
							}],
							end: self.at(end).line
						})))
					});
					expr.push_back(name);
					self.ternaryid += 1;
				}
				TREDOTS | NUMBER | TRUE | FALSE | NIL => {
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push_back(SYMBOL(t.lexeme))
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				STRING => {
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push_back(SYMBOL(format!("[[{}]]", t.lexeme)))
					} else {
						return Err(self.unexpected(t.lexeme.as_str()))
					}
				}
				ROUND_BRACKET_OPEN => {
					expr.push_back(EXPR(self.getExpression(1, 0, 0, 0, |t| {
						match t {
							ROUND_BRACKET_CLOSED => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?));
					self.current += 2;
					let fname = self.buildIdentifier()?;
					expr.push_back(fname);
					self.current -= 1;
				}
				DOLLAR => {
					let nt = self.peek(0);
					let mut num = 1usize;
					if self.current != end && nt.kind == NUMBER {
						num = match nt.lexeme.parse() {
							Ok(n) => n,
							Err(_) => {return Err(self.error(format!("Pseudo variables cannot point to the {}th variable", nt.lexeme)))}
						};
						self.current += 1;
						if num == 0 {
							return Err(self.error(String::from("Pseudo variables cannot point to the 0th variable")))
						}
					}
					if IsVar(self.current, end, &self.peek(0)) {
						expr.push_back(PSEUDO(num));
					} else {
						return Err(self.unexpected(&nt.lexeme))
					}
				}
				FN => {
					self.assert(ROUND_BRACKET_OPEN, "(")?;
					let args: FunctionArgs = if !self.advanceIf(ROUND_BRACKET_CLOSED) {
						self.buildFunctionArgs()?
					} else {Vec::new()};
					let code = self.buildCodeBlock()?;
					expr.push_back(LAMBDA {args, code});
				}
				_ => {return Err(self.unexpected(t.lexeme.as_str()))}
			}
		}
		if expr.len() == 0 {
			return Err(self.expected("<expr>", self.at(end).lexeme.as_str()))
		}
		Ok(expr)
	}

	fn buildName(&mut self) -> Result<Expression, String> {
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
					expr.push_back(SYMBOL(t.lexeme))
				}
				SAFEDOT => {return Err(self.unexpected("?."))}
				DOT => {self.checkIndex(&t, &mut expr, ".")?}
				SAFE_DOUBLE_COLON | DOUBLE_COLON => {return Err(self.error(String::from("You can't call methods here")))}
				SQUARE_BRACKET_OPEN => {
					let qexpr = self.findExpression(0, 1, 0, 0, |t| {
						match t {
							SQUARE_BRACKET_CLOSED => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					expr.push_back(SYMBOL(String::from("[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
					self.current += 1;
				}
				SAFE_SQUARE_BRACKET => {return Err(self.unexpected("?["))}
				ROUND_BRACKET_OPEN => {return Err(self.error(String::from("You can't call functions here")))}
				_ => {break}
			}
		}
		Ok(expr)
	}

	fn buildIdentifier(&mut self) -> Result<ComplexToken, String> {
		let mut expr = Expression::new();
		let line = self.getLine();
		self.current -= 1;
		loop {
			let t = self.advance();
			match t.kind {
				IDENTIFIER => {
					let nt = self.peek(0);
					if nt.kind == IDENTIFIER {
						return Err(self.unexpected(&nt.lexeme))
					}
					expr.push_back(SYMBOL(t.lexeme))
				}
				SAFEDOT => {self.checkIndex(&t, &mut expr, "?.")?}
				DOT => {self.checkIndex(&t, &mut expr, ".")?}
				SAFE_DOUBLE_COLON => {
					self.checkIndex(&t, &mut expr, "?::")?;
					if self.peek(1).kind != ROUND_BRACKET_OPEN {
						return Err(self.expected("(", &self.peek(1).lexeme))
					}
				}
				DOUBLE_COLON => {
					self.checkIndex(&t, &mut expr, ":")?;
					if self.peek(1).kind != ROUND_BRACKET_OPEN {
						return Err(self.expected("(", &self.peek(1).lexeme))
					}
				}
				SQUARE_BRACKET_OPEN => {
					let qexpr = self.findExpression(0, 1, 0, 0, |t| {
						match t {
							SQUARE_BRACKET_CLOSED => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					expr.push_back(SYMBOL(String::from("[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
					self.current += 1;
				}
				SAFE_SQUARE_BRACKET => {
					let qexpr = self.findExpression(0, 1, 0, 0, |t| {
						match t {
							SQUARE_BRACKET_CLOSED => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					expr.push_back(SYMBOL(String::from("?[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
					self.current += 1;
				}
				ROUND_BRACKET_OPEN => {
					self.current -= 2;
					expr.push_back(self.buildCall()?);
					self.current += 1;
				}
				_ => {break}
			}
		}
		Ok(IDENT {expr, line})
	}

	fn buildCodeBlock(&mut self) -> Result<CodeBlock, String> {
		let start = self.assertAdvance(CURLY_BRACKET_OPEN, "{")?.line;
		let mut tokens: Vec<Token> = Vec::new();
		let mut cscope = 1u8;
		let end: usize;
		loop {
			let t = self.advance();
			match t.kind {
				CURLY_BRACKET_OPEN => {cscope += 1}
				CURLY_BRACKET_CLOSED => {
					cscope -= 1;
					if cscope == 0 {end = t.line; break}
				}
				EOF => {return Err(self.expectedBefore("}", "<end>"))}
				_ => {}
			}
			tokens.push(t);
		}
		let code = if tokens.is_empty() {Expression::new()} else {
			tokens.push(Token {
				kind: EOF,
				lexeme: String::new(),
				line: self.getLine()
			});
			ParseTokens(tokens, self.filename.clone())?
		};
		Ok(CodeBlock {start, code, end})
	}

	fn buildLoopBlock(&mut self) -> Result<CodeBlock, String> {
		let mut code = self.buildCodeBlock()?;
		if !arg!(ENV_CONTINUE) {
			code.code.push_back(SYMBOL(String::from("::continue::")));
		}
		Ok(code)
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

	fn buildFunctionArgs(&mut self) -> Result<FunctionArgs, String> {
		let mut args = FunctionArgs::new();
		while {
			let name = {
				let t = self.advance();
				match t.kind {
					IDENTIFIER => t.lexeme,
					TREDOTS => {
						self.assertCompare(ROUND_BRACKET_CLOSED, ")")?;
						t.lexeme
					}
					_ => {return Err(self.expected("<name>", &t.lexeme))}
				}
			};
			let t = self.advance();
			match t.kind {
				COMMA => {args.push((name, None)); true}
				DEFINE => {
					let default = self.findExpression(1, 0, 0, 1, |t| {
						match t {
							COMMA | ROUND_BRACKET_CLOSED => CHECK_ADVANCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					args.push((name, Some(default)));
					!self.compare(CURLY_BRACKET_OPEN)
				}
				ROUND_BRACKET_CLOSED => {
					args.push((name, None));
					false
				}
				_ => {return Err(self.expected(")", &t.lexeme))}
			}
		} {}
		Ok(args)
	}

	fn buildElseIfChain(&mut self) -> Result<ComplexToken, String> {
		let condition = self.findExpression(0, 0, 0, 1, GetCondition)?;
		let code = self.buildCodeBlock()?;
		Ok(IF_STATEMENT {
			condition, code,
			next: {
				let t = self.advance();
				match t.kind {
					ELSEIF => Some(Box::new(self.buildElseIfChain()?)),
					ELSE => Some(Box::new(DO_BLOCK(self.buildCodeBlock()?))),
					_ => {self.current -= 1; None}
				}
			}
		})
	}

	fn buildEnums(&mut self, local: bool) -> Result<Expression, String> {
		self.current += 1;
		self.assert(CURLY_BRACKET_OPEN, "{")?;
		let mut enums = Expression::new();
		let mut n = 0i16;
		loop {
			if self.advanceIf(CURLY_BRACKET_CLOSED) {break}
			let name = self.assertAdvance(IDENTIFIER, "<name>")?;
			let t = self.advance();
			let value = match t.kind {
				CURLY_BRACKET_CLOSED => {
					self.current -= 1;
					n += 1;
					SYMBOL(n.to_string())
				},
				COMMA => {
					n += 1;
					SYMBOL(n.to_string())
				},
				DEFINE => {
					let t = self.advance();
					if t.kind != NUMBER {
						return Err(self.error(String::from("Enums values should be a non-float number ranging from -32768 to 32767.")))
					}
					n = check!(t.lexeme.parse());
					self.advanceIf(COMMA);
					SYMBOL(n.to_string())
				}
				_ => {return Err(self.expected("}", &t.lexeme))}
			};
			enums.push_back(VARIABLE {
				line: name.line, local,
				names: vec![name.lexeme],
				values: vec![expression![value]]
			});
		}
		Ok(enums)
	}

	fn buildFunction(&mut self, local: bool) -> Result<ComplexToken, String> {
		self.current += 1;
		let name = expression![SYMBOL(self.assertAdvance(IDENTIFIER, "<name>")?.lexeme)];
		self.assert(ROUND_BRACKET_OPEN, "(")?;
		let args = if !self.advanceIf(ROUND_BRACKET_CLOSED) {
			self.buildFunctionArgs()?
		} else {FunctionArgs::new()};
		let code = self.buildCodeBlock()?;
		Ok(FUNCTION {local, name, args, code})
	}
}

pub fn ParseTokens(tokens: Vec<Token>, filename: String) -> Result<Expression, String> {
	let mut i: ParserInfo = ParserInfo::new(tokens, filename);
	while !i.ended() {
		let t = i.advance();
		match t.kind {
			LOCAL | GLOBAL => {
				let local = t.kind == LOCAL;
				match i.peek(0).kind {
					FN => {
						let function = i.buildFunction(local)?;
						i.expr.push_back(function);
					}
					ENUM => {
						let enums = &mut i.buildEnums(local)?;
						i.expr.append(enums);
					}
					_ => {
						let mut names: Vec<String> = Vec::new();
						loop {
							let pname = i.assertAdvance(IDENTIFIER, "<name>")?;
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
						let values: Vec<Expression> = if !areinit {Vec::new()} else {
							i.findExpressions(0, 0, 0, 0, |t| {
								match t {
									COMMA => CHECK_BUILD,
									SEMICOLON => CHECK_STOP,
									_ => CHECK_CONTINUE
								}
							})?
						};
						i.expr.push_back(VARIABLE {
							local, names, values,
							line: t.line,
						});
						if areinit {
							i.current += 1;
						}
					}
				}
			}
			STATIC => {
				match i.peek(0).kind {
					FN => {
						PrependToOutput(CompileTokens(0, expression![i.buildFunction(true)?]) + "\n");
					}
					ENUM => {
						let mut enums = i.buildEnums(true)?.into_iter();
						while let Some(VARIABLE {mut names, mut values, ..}) = enums.next() {
							let name = names.pop().unwrap();
							let value = if let Some(SYMBOL(lexeme)) = values.pop().unwrap().pop_back() {
								lexeme
							} else {return Err(String::from("Something unexpected happened"))};
							PrependToOutput(format!("local {} = {}\n", name, value));
						}
					}
					_ => {
						let mut result = String::from("local ");
						let names = i.buildIdentifierList()?;
						i.advanceIf(SEMICOLON);
						let size = names.len();
						for i in 0..size {
							let name = names.get(i).unwrap();
							result += name;
							if i + 1 != size {result += ", "}
						}
						PrependToOutput(result + "\n")
					}
				}
			}
			METHOD => {
				let name = {
					let mut expr = Expression::new();
					loop {
						let t = i.advance();
						match t.kind {
							IDENTIFIER => {
								let nt = i.peek(0);
								if nt.kind == IDENTIFIER {
									return Err(i.unexpected(&nt.lexeme))
								}
								expr.push_back(SYMBOL(t.lexeme))
							}
							DOT => {i.checkIndex(&t, &mut expr, ".")?}
							DOUBLE_COLON => {
								i.checkIndex(&t, &mut expr, ":")?;
								if i.peek(1).kind != ROUND_BRACKET_OPEN {
									return Err(i.expected("(", &i.peek(1).lexeme))
								}
							}
							ROUND_BRACKET_OPEN => {break}
							_ => {return Err(i.expected("(", &t.lexeme))}
						}
					}
					expr
				};
				let args: FunctionArgs = if !i.advanceIf(ROUND_BRACKET_CLOSED) {
					i.buildFunctionArgs()?
				} else {Vec::new()};
				let code = i.buildCodeBlock()?;
				i.expr.push_back(FUNCTION {
					local: false,
					name, args, code
				});
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
					Err(msg) => {
						if &msg == "Expected ';' before '<end>'" {
							return Err(i.error(msg))
						}
						i.current = start
					}
				}
				let mut names: Vec<Expression> = Vec::new();
				while {
					names.push(i.buildName()?);
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
				i.expr.push_back(ALTER {
					kind: checkt.kind,
					line: t.line,
					names, values
				});
				i.current += 1;
			}
			/*PROTECTED_GET => {
				i.assert(ROUND_BRACKET_OPEN, "(")?;
				i.current += 1;
				let ident = i.buildIdentifier(true)?;
				i.expr.push_back(EXPR(vec![PGET(ident)]));
				i.current -= 1;
				i.assertCompare(ROUND_BRACKET_CLOSED, ")")?;
				let call = i.buildCall()?;
				i.expr.push_back(call);
				i.current += 1;
				i.advanceIf(SEMICOLON);
			}*/
			ROUND_BRACKET_OPEN => {
				let expr = i.findExpression(1, 0, 0, 0, |t| {
					match t {
						ROUND_BRACKET_CLOSED => CHECK_FORCESTOP,
						_ => CHECK_CONTINUE
					}
				})?;
				i.expr.push_back(EXPR(expr));
				i.current += 2;
				let call = i.buildIdentifier()?;
				i.expr.push_back(call);
				i.current += 1;
				i.advanceIf(SEMICOLON);
			}
			CURLY_BRACKET_OPEN => {
				i.current -= 1;
				let block = i.buildCodeBlock()?;
				i.expr.push_back(DO_BLOCK(block));
			}
			IF => {
				let ctoken = i.buildElseIfChain()?;
				i.expr.push_back(ctoken);
			}
			WHILE => {
				let condition = i.findExpression(0, 0, 0, 1, GetCondition)?;
				let code = i.buildLoopBlock()?;
				i.expr.push_back(WHILE_LOOP {condition, code})
			}
			UNTIL => {
				let mut condition = i.findExpression(0, 0, 0, 1, GetCondition)?;
				condition.push_front(SYMBOL(String::from("not (")));
				condition.push_back(SYMBOL(String::from(")")));
				let code = i.buildLoopBlock()?;
				i.expr.push_back(WHILE_LOOP {condition, code})
			}
			LOOP => {
				let code = i.buildLoopBlock()?;
				if i.peek(0).kind == UNTIL {
					i.current += 1;
					let condition = i.findExpression(0, 0, 0, 0, |t| {
						match t {
							SEMICOLON => CHECK_ADVANCESTOP,
							_ => CHECK_CONTINUE,
						}
					})?;
					i.expr.push_back(LOOP_UNTIL {condition, code})
				} else {
					i.expr.push_back(WHILE_LOOP {
						condition: expression![SYMBOL(String::from("true"))],
						code
					})
				}
			}
			FOR => {
				if i.peek(1).kind == DEFINE {
					let iterator = i.assertAdvance(IDENTIFIER, "<name>")?.lexeme;
					i.current += 1;
					let start = i.findExpression(0, 0, 0, 0, |t| {
						match t {
							COMMA => CHECK_ADVANCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					let end = i.findExpression(0, 0, 0, 1, |t| {
						match t {
							COMMA | CURLY_BRACKET_OPEN => CHECK_FORCESTOP,
							_ => CHECK_CONTINUE
						}
					})?;
					let alter = if i.advance().kind != CURLY_BRACKET_OPEN {
						i.findExpression(0, 0, 0, 1, GetCondition)?
					} else {
						i.current -= 1;
						expression![SYMBOL(String::from("1"))]
					};
					let code = i.buildLoopBlock()?;
					i.expr.push_back(FOR_LOOP {iterator, start, end, alter, code})
				} else {
					let iterators = i.buildIdentifierList()?;
					let expr = match i.advance().kind {
						OF => {
							let mut expr = expression![SYMBOL(String::from("pairs("))];
							expr.append(&mut i.findExpression(0, 0, 0, 1, GetCondition)?);
							expr.push_back(SYMBOL(String::from(")")));
							expr
						}
						IN => {
							let mut expr = expression![SYMBOL(String::from("ipairs("))];
							expr.append(&mut i.findExpression(0, 0, 0, 1, GetCondition)?);
							expr.push_back(SYMBOL(String::from(")")));
							expr
						}
						WITH => {i.findExpression(0, 0, 0, 1, GetCondition)?}
						_ => {return Err(i.expected("of', 'in' or 'with", &i.peek(0).lexeme))}
					};
					let code = i.buildLoopBlock()?;
					i.expr.push_back(FOR_FUNC_LOOP {iterators, expr, code});
				}
			}
			CONTINUE => {i.expr.push_back(CONTINUE_LOOP); i.advanceIf(SEMICOLON);}
			BREAK => {i.expr.push_back(BREAK_LOOP); i.advanceIf(SEMICOLON);}
			RETURN => {
				let expr = if i.advanceIf(SEMICOLON) {
					None
				} else {
					Some(i.findExpression(0, 0, 0, 0, |t| {
						match t {
							SEMICOLON => CHECK_ADVANCESTOP,
							_ => CHECK_CONTINUE,
						}
					})?)
				};
				i.expr.push_back(RETURN_EXPR(expr));
			}
			TRY => {
				let totry = i.buildCodeBlock()?;
				let error: Option<String>;
				let catch = if i.advanceIf(CATCH) {
					let t = i.advance();
					if t.kind == IDENTIFIER {
						error = Some(t.lexeme);
					} else {
						error = None;
						i.current -= 1;
					}
					Some(i.buildCodeBlock()?)
				} else {
					error = None;
					None
				};
				i.expr.push_back(TRY_CATCH {totry, error, catch});
			}
			FN | ENUM => {
				return Err(i.error(format!("'{}' must have 'local', 'global' or 'static' beforehand", t.lexeme)))
			}
			_ => {return Err(i.unexpected(t.lexeme.as_str()))}
		}
	}
	Ok(i.expr)
}