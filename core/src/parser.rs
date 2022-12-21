#![allow(non_camel_case_types)]

use self::ComplexToken::*;
use crate::env::ContinueMode;
use crate::scanner::TokenType::*;
use crate::scanner::{Token, TokenType};
use crate::{check, compiler::compile_tokens, flag, format_clue, ENV_DATA};
use ahash::AHashMap;
use std::{cmp, collections::LinkedList};

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
pub type FunctionArgs = Vec<(String, Option<(Expression, usize)>)>;
//pub type LocalsList = Option<AHashMap<String, LuaType>>;
//pub type ArgsAndTypes = (FunctionArgs, Option<Vec<(String, LuaType)>>);
type OptionalEnd = Option<(TokenType, &'static str)>;
type MatchCase = (Vec<Expression>, Option<Expression>, CodeBlock);

#[derive(Debug, Clone, PartialEq)]
pub enum ComplexToken {
	VARIABLE {
		local: bool,
		names: Vec<String>,
		values: Vec<Expression>,
		line: usize,
	},

	ALTER {
		kind: TokenType,
		names: Vec<Expression>,
		values: Vec<Expression>,
		line: usize,
	},

	TABLE {
		values: Vec<(Option<Expression>, Expression, usize)>,
		metas: Vec<(String, Expression, usize)>,
		metatable: Option<String>,
	},

	FUNCTION {
		local: bool,
		name: Expression,
		args: FunctionArgs,
		code: CodeBlock,
	},

	LAMBDA {
		args: FunctionArgs,
		code: CodeBlock,
	},

	IF_STATEMENT {
		condition: Expression,
		code: CodeBlock,
		next: Option<Box<ComplexToken>>,
	},

	MATCH_BLOCK {
		name: String,
		value: Expression,
		branches: Vec<MatchCase>,
		line: usize,
	},

	WHILE_LOOP {
		condition: Expression,
		code: CodeBlock,
	},

	LOOP_UNTIL {
		condition: Expression,
		code: CodeBlock,
	},

	FOR_LOOP {
		iterator: String,
		start: Expression,
		end: Expression,
		alter: Expression,
		code: CodeBlock,
	},

	FOR_FUNC_LOOP {
		iterators: Vec<String>,
		expr: Expression,
		code: CodeBlock,
	},

	TRY_CATCH {
		totry: CodeBlock,
		catch: Option<CodeBlock>,
		error: Option<String>,
	},

	IDENT {
		expr: Expression,
		line: usize,
	},

	MACRO_CALL {
		expr: Expression,
		args: Vec<Expression>,
	},

	SYMBOL(String),
	CALL(Vec<Expression>),
	EXPR(Expression),
	DO_BLOCK(CodeBlock),
	RETURN_EXPR(Option<Vec<Expression>>),
	CONTINUE_LOOP,
	BREAK_LOOP,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CodeBlock {
	pub start: usize,
	pub code: Expression,
	pub end: usize,
}

struct BorrowedToken {
	token: *const Token,
}

impl BorrowedToken {
	fn new(token: *const Token) -> BorrowedToken {
		BorrowedToken { token }
	}

	fn token(&self) -> &Token {
		unsafe { &(*self.token) }
	}

	fn kind(&self) -> TokenType {
		self.token().kind
	}

	fn lexeme(&self) -> String {
		self.token().lexeme.clone()
	}

	fn line(&self) -> usize {
		self.token().line
	}

	fn into_owned(self) -> Token {
		self.token().to_owned()
	}
}
/*
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LuaType {
	ANY,
	NIL,
	NUMBER,
}
*/
struct ParserInfo {
	current: usize,
	size: usize,
	tokens: Vec<Token>,
	filename: String,
	expr: Expression,
	testing: Option<usize>,
	internal_var_id: u8,
	statics: String,
	macros: AHashMap<String, Expression>,
	//locals: LocalsList,
}

impl ParserInfo {
	fn new(tokens: Vec<Token> /*, locals: LocalsList*/, filename: String) -> ParserInfo {
		ParserInfo {
			current: 0,
			size: tokens.len() - 1,
			tokens,
			filename,
			expr: Expression::new(),
			testing: None,
			internal_var_id: 0,
			statics: String::new(),
			macros: AHashMap::default(),
			// locals,
		}
	}

	fn test<T>(&mut self, func: impl FnOnce(&mut ParserInfo) -> T) -> (T, usize) {
		let start = self.current - 1;
		self.testing = Some(0);
		let result = func(self);
		self.testing = match self.testing {
			Some(line) if line > 0 => self.testing,
			_ => None,
		};
		let reached = self.current;
		self.current = start;
		(result, reached)
	}
	/*
		fn warning(&self, msg: impl Into<String>, line: usize) {
			println!(
				"Warning in file \"{}\" at line {}!\nWarning: \"{}\"",
				self.filename,
				line,
				msg.into()
			);
		}
	*/
	fn error(&mut self, msg: impl Into<String>, line: usize) -> String {
		if let Some(0) = self.testing {
			self.testing = Some(line);
		} else {
			println!("Error in file \"{}\" at line {line}!", self.filename);
		}
		msg.into()
	}

	fn expected(&mut self, expected: &str, got: &str, line: usize) -> String {
		self.error(
			format_clue!("Expected '", expected, "', got '", got, "'"),
			line,
		)
	}

	fn expected_before(&mut self, expected: &str, before: &str, line: usize) -> String {
		self.error(
			format_clue!("Expected '", expected, "' before '", before, "'"),
			line,
		)
	}

	fn unexpected(&mut self, str: &str, line: usize) -> String {
		self.error(format_clue!("Unexpected token '", str, "'"), line)
	}

	fn ended(&self) -> bool {
		self.current >= self.size
	}

	fn at(&self, pos: usize) -> BorrowedToken {
		BorrowedToken::new(&self.tokens[cmp::min(pos, self.size)])
	}

	fn advance(&mut self) -> BorrowedToken {
		self.current += 1;
		self.look_back(0)
	}

	fn peek(&self, pos: usize) -> BorrowedToken {
		let pos: usize = self.current + pos;
		self.at(pos)
	}

	fn look_back(&self, pos: usize) -> BorrowedToken {
		let pos: usize = self.current - pos - 1;
		self.at(pos)
	}

	fn compare(&self, expected: TokenType) -> bool {
		if self.ended() {
			return false;
		}
		if self.peek(0).kind() != expected {
			return false;
		}
		true
	}

	fn advance_if(&mut self, expected: TokenType) -> bool {
		if self.ended() {
			return false;
		}
		if self.peek(0).kind() != expected {
			return false;
		}
		self.current += 1;
		true
	}

	fn assert_advance(
		&mut self,
		expected: TokenType,
		error: &str,
	) -> Result<BorrowedToken, String> {
		let t = self.advance();
		if t.kind() != expected {
			return Err(self.expected(error, &t.lexeme(), t.line()));
		}
		Ok(t)
	}

	fn assert_compare(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.compare(expected) {
			let t = self.peek(0);
			return Err(self.expected(error, &t.lexeme(), t.line()));
		}
		Ok(())
	}

	fn assert_end<T>(
		&mut self,
		tocheck: &BorrowedToken,
		end: OptionalEnd,
		iftrue: T,
	) -> Result<T, String> {
		if let Some((kind, lexeme)) = end {
			if tocheck.kind() != kind {
				return Err(self.expected(lexeme, &tocheck.lexeme(), tocheck.line()));
			}
		}
		Ok(iftrue)
	}

	fn assert(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.advance_if(expected) {
			let t = self.peek(0);
			return Err(self.expected(error, &t.lexeme(), t.line()));
		}
		Ok(())
	}
	/*
		fn assert_variable(&mut self, mut variable: Iter<ComplexToken>) -> Result<LuaType, String> {
			let mut scope: &LocalsList = &self.locals;
			let mut luatype = LuaType::NIL;
			while let Some(locals) = scope {
				if let Some(t) = variable.next() {
					// TODO
				} else {
					break;
				}
			}
			Ok(luatype)
		}
	*/

	fn get_next_internal_var(&mut self) -> String {
		self.internal_var_id += 1;
		format_clue!("_internal", self.internal_var_id.to_string())
	}

	fn build_call(&mut self) -> Result<Vec<Expression>, String> {
		let args: Vec<Expression> = if self.advance_if(ROUND_BRACKET_CLOSED) {
			Vec::new()
		} else {
			self.find_expressions(COMMA, Some((ROUND_BRACKET_CLOSED, ")")))?
		};
		Ok(args)
	}

	fn find_expressions(
		&mut self,
		separator: TokenType,
		end: OptionalEnd,
	) -> Result<Vec<Expression>, String> {
		let mut exprs: Vec<Expression> = Vec::new();
		loop {
			let expr = self.build_expression(None)?;
			let t = self.look_back(0);
			exprs.push(expr);
			if t.kind() != separator {
				return self.assert_end(&t, end, exprs);
			}
		}
	}

	fn build_table(&mut self) -> Result<ComplexToken, String> {
		let mut values: Vec<(Option<Expression>, Expression, usize)> = Vec::new();
		let mut metas: Vec<(String, Expression, usize)> = Vec::new();
		let mut metatable: Option<String> = None;
		loop {
			if self.advance_if(CURLY_BRACKET_CLOSED) {
				break;
			}
			let start = self.current;
			let mut qscope = 1u8;
			let mut iskey = false;
			while match self.peek(0).kind() {
				CURLY_BRACKET_OPEN => {
					qscope += 1;
					true
				}
				CURLY_BRACKET_CLOSED => {
					qscope -= 1;
					qscope > 1
				}
				COMMA => qscope != 1,
				DEFINE if qscope == 1 => {
					iskey = true;
					false
				}
				META if self.peek(1).kind() == WITH => {
					iskey = true;
					true
				}
				EOF => return Err(self.expected_before("}", "<end>", self.peek(0).line())),
				_ => true,
			} {
				self.current += 1;
			}
			self.current = start;
			if !iskey {
				values.push((None, self.build_expression(None)?, self.at(start).line()));
				self.current -= 1;
				self.advance_if(COMMA);
				continue;
			}
			let name: Result<Expression, String>;
			let pn = self.advance();
			match pn.kind() {
				IDENTIFIER => {
					name = Ok(expression![SYMBOL(pn.lexeme())]);
				}
				SQUARE_BRACKET_OPEN => {
					let mut qscope = 1u8;
					let start = self.current;
					while match self.advance().kind() {
						SQUARE_BRACKET_OPEN => {
							qscope += 1;
							true
						}
						SQUARE_BRACKET_CLOSED => {
							qscope -= 1;
							!matches!(qscope, 0)
						}
						EOF => return Err(self.expected_before("]", "<end>", self.peek(0).line())),
						_ => true,
					} {}
					self.current = start;
					name = Ok(self.build_name()?);
					self.current -= 1;
				}
				META => {
					if self.advance_if(WITH) {
						if !metas.is_empty() {
							return Err(self.error(
								"An external metatable cannot be used if the table already set its own metamethods",
								pn.line()
							));
						}
						metatable = Some(self.assert_advance(IDENTIFIER, "<name>")?.lexeme());
						self.advance_if(COMMA);
						continue;
					} else {
						if metatable.is_some() {
							return Err(self.error(
								"Metamethods cannot be set if the table already ueses an external metatable",
								pn.line()
							));
						}
						name = Err(String::from(match self.advance().lexeme().as_ref() {
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
							_ => {
								let t = self.peek(0);
								return Err(self.expected("<meta name>", &t.lexeme(), t.line()));
							}
						}))
					}
				}
				_ => return Err(self.expected("<name>", &pn.lexeme(), pn.line())),
			}
			if !self.advance_if(DEFINE) {
				let t = self.peek(0);
				return Err(self.expected("=", &t.lexeme(), t.line()));
			}
			let start = self.current;
			let mut cscope = 0u8;
			while match self.peek(0).kind() {
				COMMA | CURLY_BRACKET_CLOSED => cscope != 0,
				ROUND_BRACKET_OPEN => {
					cscope += 1;
					true
				}
				ROUND_BRACKET_CLOSED => {
					if cscope == 0 {
						return Err(self.expected_before("(", ")", self.peek(0).line()));
					}
					cscope -= 1;
					true
				}
				EOF => return Err(self.expected_before("}", "<end>", self.peek(0).line())),
				_ => true,
			} {
				self.current += 1;
			}
			self.current = start;
			match name {
				Ok(n) => values.push((Some(n), self.build_expression(None)?, pn.line())),
				Err(n) => metas.push((n, self.build_expression(None)?, pn.line())),
			}
			self.current -= 1;
			self.advance_if(COMMA);
		}
		Ok(TABLE {
			values,
			metas,
			metatable,
		})
	}

	fn check_operator(
		&mut self,
		t: &BorrowedToken,
		notable: &mut bool,
		checkback: bool,
	) -> Result<(), String> {
		if match self.peek(0).kind() {
			NUMBER | IDENTIFIER | STRING | SAFE_EXPRESSION | TRUE | FALSE | MINUS
			| BIT_NOT | NIL | NOT | HASHTAG | ROUND_BRACKET_OPEN | AT | THREEDOTS | MATCH => false,
			CURLY_BRACKET_OPEN => {
				*notable = false;
				false
			}
			_ => true,
		} {
			return Err(self.error(
				format!("Operator '{}' has invalid right hand token", t.lexeme()),
				t.line(),
			));
		}
		if checkback
			&& !matches!(
				self.look_back(1).kind(),
				NUMBER
					| IDENTIFIER | STRING
					| TRUE | FALSE | NIL
					| ROUND_BRACKET_CLOSED
					| SQUARE_BRACKET_CLOSED
					| THREEDOTS | CURLY_BRACKET_CLOSED
			) {
			return Err(self.error(
				format!("Operator '{}' has invalid left hand token", t.lexeme()),
				t.line(),
			));
		}
		Ok(())
	}

	fn build_function_op(
		&mut self,
		t: &BorrowedToken,
		expr: &mut Expression,
		fname: impl Into<String>,
		end: OptionalEnd,
		notable: &mut bool,
	) -> Result<(), String> {
		self.check_operator(t, notable, true)?;
		let mut arg1 = Expression::new();
		arg1.append(expr);
		let arg2 = self.build_expression(end)?;
		expr.push_back(SYMBOL(fname.into()));
		expr.push_back(CALL(vec![arg1, arg2]));
		self.current -= 1;
		Ok(())
	}

	fn build_bitwise_op(
		&mut self,
		t: &BorrowedToken,
		expr: &mut Expression,
		fname: &str,
		end: OptionalEnd,
		notable: &mut bool,
	) -> Result<(), String> {
		self.check_operator(t, notable, true)?;
		if let Some(bit) = flag!(env_jitbit) {
			self.build_function_op(t, expr, format!("{bit}.{fname}"), end, notable)?
		} else {
			expr.push_back(SYMBOL(t.lexeme()))
		}
		Ok(())
	}

	fn check_index(
		&mut self,
		t: &BorrowedToken,
		expr: &mut Expression,
		lexeme: &str,
	) -> Result<(), String> {
		if !self.compare(IDENTIFIER)
			|| matches!(self.look_back(0).kind(), IDENTIFIER | SQUARE_BRACKET_CLOSED)
		{
			return Err(self.error(
				format!("'{}' should be used only when indexing", t.lexeme()),
				self.peek(0).line(),
			));
		}
		expr.push_back(SYMBOL(lexeme.to_string()));
		Ok(())
	}

	fn check_val(&mut self) -> bool {
		match self.peek(0).kind() {
			NUMBER | IDENTIFIER | STRING | SAFE_EXPRESSION | TRUE | BIT_NOT | FALSE
			| NIL | NOT | HASHTAG | CURLY_BRACKET_OPEN | THREEDOTS | MATCH => {
				self.current += 1;
				true
			}
			_ => false,
		}
	}

	fn build_expression(&mut self, end: OptionalEnd) -> Result<Expression, String> {
		let mut expr = Expression::new();
		let notable = &mut true;
		let start = self.current;
		let last = loop {
			let t = self.advance();
			match t.kind() {
				IDENTIFIER => {
					let fname = self.build_identifier()?;
					self.current -= 1;
					expr.push_back(fname);
					if self.check_val() {
						break t;
					}
				}
				AT => {
					let name = self.assert_advance(IDENTIFIER, "<name>")?.lexeme();
					let code = self.macros.get_mut(&name);
					let macroexpr = if let Some(macroexpr) = code {
						macroexpr.clone()
					} else {
						return Err(
							self.error(format!("The macro {name} is not defined"), t.line())
						);
					};
					let args = if self.advance_if(ROUND_BRACKET_OPEN) {
						self.build_call()?
					} else {
						Vec::new()
					};
					expr.push_back(MACRO_CALL {
						expr: macroexpr,
						args,
					});
					if self.check_val() {
						break t;
					}
				}
				CURLY_BRACKET_OPEN => {
					if let Some((kind, ..)) = end {
						if kind == CURLY_BRACKET_OPEN && *notable {
							break t;
						}
					}
					expr.push_back(self.build_table()?);
					*notable = true;
					if self.check_val() {
						break t;
					}
				}
				PLUS | STAR | SLASH | PERCENTUAL | CARET | TWODOTS | EQUAL | BIGGER
				| BIGGER_EQUAL | SMALLER | SMALLER_EQUAL => {
					self.check_operator(&t, notable, true)?;
					expr.push_back(SYMBOL(t.lexeme()))
				}
				MINUS => {
					self.check_operator(&t, notable, false)?;
					expr.push_back(SYMBOL(if self.look_back(1).kind() == MINUS {
						format!(" {}", t.lexeme())
					} else {
						t.lexeme()
					}))
				}
				FLOOR_DIVISION => {
					self.build_function_op(&t, &mut expr, "math.floor", end, notable)?
				}
				BIT_AND => self.build_bitwise_op(&t, &mut expr, "band", end, notable)?,
				BIT_OR => self.build_bitwise_op(&t, &mut expr, "bor", end, notable)?,
				BIT_XOR => self.build_bitwise_op(&t, &mut expr, "bxor", end, notable)?,
				BIT_NOT => {
					self.check_operator(&t, notable, false)?;
					if let Some(bit) = flag!(env_jitbit) {
						let arg = self.build_expression(end)?;
						expr.push_back(SYMBOL(bit.clone() + ".bnot"));
						expr.push_back(CALL(vec![arg]));
						self.current -= 1;
					} else {
						expr.push_back(SYMBOL(t.lexeme()))
					}
				}
				LEFT_SHIFT => self.build_bitwise_op(&t, &mut expr, "lshift", end, notable)?,
				RIGHT_SHIFT => self.build_bitwise_op(&t, &mut expr, "rshift", end, notable)?,
				NOT_EQUAL => {
					self.check_operator(&t, notable, true)?;
					expr.push_back(SYMBOL(String::from("~=")))
				}
				HASHTAG => {
					if !matches!(
						self.peek(0).kind(),
						IDENTIFIER | CURLY_BRACKET_OPEN | ROUND_BRACKET_OPEN
					) {
						let t = self.peek(0);
						return Err(self.expected("<table>", &t.lexeme(), t.line()));
					}
					expr.push_back(SYMBOL(String::from("#")))
				}
				/*SAFE_EXPRESSION => {
					self.assert(ROUND_BRACKET_OPEN, "(")?;
					self.current += 1;
					expr.push_back(PGET(self.build_identifier(true)?));
				}*/
				AND => {
					self.check_operator(&t, notable, true)?;
					expr.push_back(SYMBOL(String::from(" and ")))
				}
				OR => {
					self.check_operator(&t, notable, true)?;
					expr.push_back(SYMBOL(String::from(" or ")))
				}
				NOT => {
					self.check_operator(&t, notable, false)?;
					expr.push_back(SYMBOL(String::from("not ")))
				}
				MATCH => {
					let name = self.get_next_internal_var();
					let ident = SYMBOL(name.clone());
					let ctoken = self.build_match_block(name, &|i /*, _*/| {
						let start = i.peek(0).line();
						let expr = i.build_expression(None)?;
						let end = i.look_back(1).line();
						if matches!(i.look_back(0).kind(), CURLY_BRACKET_CLOSED | DEFAULT) {
							i.current -= 1
						}
						Ok(CodeBlock {
							code: expression![ALTER {
								kind: DEFINE,
								names: vec![expression![ident.clone()]],
								values: vec![expr],
								line: end
							}],
							start,
							end,
						})
					})?;
					self.expr.push_back(ctoken);
					expr.push_back(ident);
				}
				QUESTION_MARK => {
					let mut condition = Expression::new();
					condition.append(&mut expr);
					let exprtrue = self.build_expression(Some((COLON, ":")))?;
					let t2 = self.look_back(0);
					let exprfalse = self.build_expression(end)?;
					self.current -= 1;
					let name = self.get_next_internal_var();
					self.expr.push_back(VARIABLE {
						line: t.line(),
						local: true,
						names: vec![name.clone()],
						values: Vec::new(),
					});
					let name = SYMBOL(name);
					self.expr.push_back(IF_STATEMENT {
						condition,
						code: CodeBlock {
							start: self.at(start).line(),
							code: expression![ALTER {
								kind: DEFINE,
								line: t.line(),
								names: vec![expression![name.clone()]],
								values: vec![exprtrue]
							}],
							end: t2.line(),
						},
						next: Some(Box::new(DO_BLOCK(CodeBlock {
							start: t2.line(),
							code: expression![ALTER {
								kind: DEFINE,
								line: t.line(),
								names: vec![expression![name.clone()]],
								values: vec![exprfalse]
							}],
							end: self.at(self.current).line(),
						}))),
					});
					expr.push_back(name);
					if self.check_val() {
						break t;
					}
				}
				THREEDOTS | NUMBER | TRUE | FALSE | NIL | STRING => {
					expr.push_back(SYMBOL(t.lexeme()));
					if self.check_val() {
						break t;
					}
				}
				ROUND_BRACKET_OPEN => {
					expr.push_back(EXPR(
						self.build_expression(Some((ROUND_BRACKET_CLOSED, ")")))?,
					));
					if self.check_val() {
						break t;
					}
					self.current += 1;
					let fname = self.build_identifier()?;
					expr.push_back(fname);
					self.current -= 1;
				}
				FN => {
					let /*(*/args/*, types)*/ = if self.advance_if(ROUND_BRACKET_OPEN)
						&& !self.advance_if(ROUND_BRACKET_CLOSED)
					{
						self.build_function_args()?
					} else {
						/*(*/FunctionArgs::new()//, None)
					};
					let code = self.build_function_block(/*types*/)?;
					expr.push_back(LAMBDA { args, code });
					if self.check_val() {
						break t;
					}
				}
				SEMICOLON => {
					self.current += 1;
					break t;
				}
				_ => break t,
			}
		};
		if expr.is_empty() {
			return Err(self.expected("<expr>", &last.lexeme(), last.line()));
		}
		self.assert_end(&self.look_back(0), end, expr)
	}

	fn build_name(&mut self) -> Result<Expression, String> {
		let mut expr = Expression::new();
		self.current -= 1;
		loop {
			let t = self.advance();
			match t.kind() {
				IDENTIFIER => {
					expr.push_back(SYMBOL(t.lexeme()));
					if self.check_val() {
						break;
					}
				}
				SAFEDOT => return Err(self.unexpected("?.", t.line())),
				DOT => self.check_index(&t, &mut expr, ".")?,
				SAFE_DOUBLE_COLON | DOUBLE_COLON => {
					return Err(self.error("You can't call functions here", t.line()))
				}
				SQUARE_BRACKET_OPEN => {
					let qexpr = self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?;
					expr.push_back(SYMBOL(String::from("[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
				}
				SAFE_SQUARE_BRACKET => return Err(self.unexpected("?[", t.line())),
				ROUND_BRACKET_OPEN => {
					return Err(self.error("You can't call functions here", t.line()))
				}
				_ => break,
			}
		}
		Ok(expr)
	}

	fn build_identifier(&mut self) -> Result<ComplexToken, String> {
		let mut expr = Expression::new();
		let line = self.look_back(0).line();
		self.current -= 1;
		loop {
			let t = self.advance();
			match t.kind() {
				IDENTIFIER => {
					expr.push_back(SYMBOL(t.lexeme()));
					if self.check_val() {
						break;
					}
				}
				SAFEDOT => self.check_index(&t, &mut expr, "?.")?,
				DOT => self.check_index(&t, &mut expr, ".")?,
				SAFE_DOUBLE_COLON => {
					self.check_index(&t, &mut expr, "?::")?;
					if self.peek(1).kind() != ROUND_BRACKET_OPEN {
						let t = self.peek(1);
						return Err(self.expected("(", &t.lexeme(), t.line()));
					}
				}
				DOUBLE_COLON => {
					self.check_index(&t, &mut expr, ":")?;
					if self.peek(1).kind() != ROUND_BRACKET_OPEN {
						let t = self.peek(1);
						return Err(self.expected("(", &t.lexeme(), t.line()));
					}
				}
				SQUARE_BRACKET_OPEN => {
					let qexpr = self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?;
					expr.push_back(SYMBOL(String::from("[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
					if self.check_val() {
						break;
					}
				}
				SAFE_SQUARE_BRACKET => {
					let qexpr = self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?;
					expr.push_back(SYMBOL(String::from("?[")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from("]")));
					if self.check_val() {
						break;
					}
				}
				ROUND_BRACKET_OPEN => {
					expr.push_back(CALL(self.build_call()?));
					if self.check_val() {
						break;
					}
				}
				_ => break,
			}
		}
		Ok(IDENT { expr, line })
	}

	fn get_code_block_start(&mut self) -> Result<usize, String> {
		let t = self.advance();
		if t.kind() != CURLY_BRACKET_OPEN {
			self.current -= 2;
			Ok(self.assert_advance(CURLY_BRACKET_OPEN, "{")?.line())
		} else {
			Ok(t.line())
		}
	}

	fn parse_code_block(
		&self,
		mut tokens: Vec<Token>,
		//locals: LocalsList,
	) -> Result<Expression, String> {
		if tokens.is_empty() {
			Ok(Expression::new())
		} else {
			tokens.push(self.tokens.last().unwrap().clone());
			Ok(parse_tokens(
				tokens, /*, locals*/
				self.filename.clone(),
			)?)
		}
	}

	fn build_code_block(&mut self /*, locals: LocalsList*/) -> Result<CodeBlock, String> {
		let start = self.get_code_block_start()?;
		let mut tokens: Vec<Token> = Vec::new();
		let mut cscope = 1u8;
		let end: usize;
		loop {
			let t = self.advance();
			match t.kind() {
				CURLY_BRACKET_OPEN => cscope += 1,
				CURLY_BRACKET_CLOSED => {
					cscope -= 1;
					if cscope == 0 {
						end = t.line();
						break;
					}
				}
				EOF => return Err(self.expected_before("}", "<end>", t.line())),
				_ => {}
			}
			tokens.push(t.into_owned());
		}
		let code = self.parse_code_block(tokens /*, locals*/)?;
		Ok(CodeBlock { start, code, end })
	}

	fn build_function_block(
		&mut self,
		//args: Option<Vec<(String, LuaType)>>,
	) -> Result<CodeBlock, String> {
		/*
		if let Some(args) = args {
			self.build_code_block({
				let mut locals = self.locals.clone().unwrap();
				for (name, luatype) in args {
					locals.insert(name, luatype);
				}
				Some(locals)
			})
		} else {*/
		self.build_code_block(/*self.locals.clone()*/)
		//}
	}

	fn build_loop_block(&mut self) -> Result<CodeBlock, String> {
		let mut hascontinue = false;
		let mut is_in_other_loop = false;
		let start = self.get_code_block_start()?;
		let mut tokens: Vec<Token> = Vec::new();
		let mut cscope = 1u8;
		let end: usize;
		loop {
			let t = self.advance();
			match t.kind() {
				CURLY_BRACKET_OPEN => cscope += 1,
				CURLY_BRACKET_CLOSED => {
					cscope -= 1;
					is_in_other_loop = false;
					if cscope == 0 {
						end = t.line();
						break;
					}
				}
				FOR | WHILE | LOOP => is_in_other_loop = true,
				CONTINUE if !is_in_other_loop => {
					hascontinue = true;
					let line = t.line();
					if flag!(env_continue) == ContinueMode::MOONSCRIPT {
						tokens.push(Token::new(IDENTIFIER, "_continue", line));
						tokens.push(Token::new(DEFINE, "=", line));
						tokens.push(Token::new(TRUE, "true", line));
						tokens.push(Token::new(BREAK, "break", line));
						continue;
					}
				}
				EOF => return Err(self.expected_before("}", "<end>", t.line())),
				_ => {}
			}
			tokens.push(t.into_owned());
		}
		let mut code = self.parse_code_block(tokens /*, self.locals.clone()*/)?;
		if hascontinue {
			match flag!(env_continue) {
				ContinueMode::SIMPLE => {}
				ContinueMode::LUAJIT => code.push_back(SYMBOL(String::from("::continue::"))),
				ContinueMode::MOONSCRIPT => {
					code.push_back(ALTER {
						kind: DEFINE,
						names: vec![expression![SYMBOL(String::from("_continue"))]],
						values: vec![expression![SYMBOL(String::from("true"))]],
						line: end,
					});
					code = expression![
						VARIABLE {
							local: true,
							names: vec![String::from("_continue")],
							values: vec![expression![SYMBOL(String::from("false"))]],
							line: start
						},
						LOOP_UNTIL {
							condition: expression![SYMBOL(String::from("true"))],
							code: CodeBlock { start, code, end }
						},
						IF_STATEMENT {
							condition: expression![
								SYMBOL(String::from("not ")),
								SYMBOL(String::from("_continue"))
							],
							code: CodeBlock {
								start: end,
								code: expression![BREAK_LOOP],
								end
							},
							next: None
						}
					]
				}
			}
		}
		Ok(CodeBlock { start, code, end })
	}

	fn build_identifier_list(&mut self) -> Result<Vec<String>, String> {
		let mut idents: Vec<String> = Vec::new();
		while {
			let t = self.assert_advance(IDENTIFIER, "<name>")?;
			idents.push(t.lexeme());
			self.advance_if(COMMA)
		} {}
		Ok(idents)
	}

	fn build_function_args(&mut self) -> Result</*ArgsAndTypes*/ FunctionArgs, String> {
		let mut args = FunctionArgs::new();
		/*let mut types: Option<Vec<(String, LuaType)>> = if self.locals.is_some() {
			Some(Vec::new())
		} else {
			None
		};*/
		while {
			let name = {
				let t = self.advance();
				match t.kind() {
					IDENTIFIER => t,
					THREEDOTS => {
						self.assert_compare(ROUND_BRACKET_CLOSED, ")")?;
						t
					}
					_ => return Err(self.expected("<name>", &t.lexeme(), t.line())),
				}
			};
			/*if let Some(types) = &mut types {
				types.push((
					name.lexeme(),
					if name.kind() == THREEDOTS {
						LuaType::ANY
					} else {
						self.build_type()?
					},
				))
			}*/
			let t = self.advance();
			match t.kind() {
				COMMA => {
					args.push((name.lexeme(), None));
					true
				}
				DEFINE => {
					let default = self.build_expression(None)?;
					args.push((name.lexeme(), Some((default, name.line()))));
					let notended = self.peek(0).kind() != CURLY_BRACKET_OPEN;
					if notended {
						match self.look_back(0).kind() {
							COMMA => {}
							ROUND_BRACKET_CLOSED => self.current -= 1,
							_ => {
								let t = self.peek(0);
								return Err(self.expected(")", &t.lexeme(), t.line()));
							}
						}
					}
					notended
				}
				ROUND_BRACKET_CLOSED => {
					args.push((name.lexeme(), None));
					false
				}
				_ => return Err(self.expected(")", &t.lexeme(), t.line())),
			}
		} {}
		Ok(/*(args, types)*/ args)
	}

	fn build_elseif_chain(&mut self) -> Result<ComplexToken, String> {
		let condition = self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?;
		let code = self.build_code_block(/*self.locals.clone()*/)?;
		Ok(IF_STATEMENT {
			condition,
			code,
			next: {
				let t = self.advance();
				match t.kind() {
					ELSEIF => Some(Box::new(self.build_elseif_chain()?)),
					ELSE => Some(Box::new(DO_BLOCK(
						self.build_code_block(/*self.locals.clone()*/)?,
					))),
					_ => {
						self.current -= 1;
						None
					}
				}
			},
		})
	}

	fn build_enums(&mut self, local: bool) -> Result<Expression, String> {
		self.current += 1;
		self.assert(CURLY_BRACKET_OPEN, "{")?;
		let mut enums = Expression::new();
		let mut n = 0i16;
		loop {
			if self.advance_if(CURLY_BRACKET_CLOSED) {
				break;
			}
			let name = self.assert_advance(IDENTIFIER, "<name>")?;
			let t = self.advance();
			let value = match t.kind() {
				CURLY_BRACKET_CLOSED => {
					self.current -= 1;
					n += 1;
					SYMBOL(n.to_string())
				}
				COMMA => {
					n += 1;
					SYMBOL(n.to_string())
				}
				DEFINE => {
					let t = self.advance();
					if t.kind() != NUMBER {
						return Err(self.error("Enums values should be a non-float number ranging from -32768 to 32767.", t.line()));
					}
					n = check!(t.lexeme().parse());
					self.advance_if(COMMA);
					SYMBOL(n.to_string())
				}
				_ => return Err(self.expected("}", &t.lexeme(), t.line())),
			};
			enums.push_back(VARIABLE {
				line: name.line(),
				local,
				names: vec![name.lexeme()],
				values: vec![expression![value]],
			});
		}
		/*if let Some(locals) = &mut self.locals {
			for r#enum in &enums {
				if let VARIABLE { names, .. } = r#enum {
					locals.insert(names[0].clone(), LuaType::NUMBER);
				}
			}
		}*/
		Ok(enums)
	}

	fn build_function(&mut self, local: bool) -> Result<ComplexToken, String> {
		self.current += 1;
		let t = self.assert_advance(IDENTIFIER, "<name>")?;
		let name = expression![SYMBOL(t.lexeme())];
		self.assert(ROUND_BRACKET_OPEN, "(")?;
		let /*(*/args/*, types)*/ = if !self.advance_if(ROUND_BRACKET_CLOSED) {
			self.build_function_args()?
		} else {
			/*(*/FunctionArgs::new()//, None)
		};
		let code = self.build_function_block(/*types*/)?;
		/*if self.locals.is_some() {
			self.add_variable(t.lexeme(), LuaType::NIL);
		}*/
		Ok(FUNCTION {
			local,
			name,
			args,
			code,
		})
	}
	/*
		fn add_variable(&mut self, name: String, luatype: LuaType) {
			if let Some(locals) = &mut self.locals {
				locals.insert(name, luatype);
			}
		}

		fn build_type(&mut self) -> Result<LuaType, String> {
			if self.advance_if(COLON) {
				Ok(LuaType::ANY) //PLACEHOLDER
			} else {
				Ok(LuaType::NIL)
			}
		}
	*/
	fn build_variable(&mut self) -> Result</*(*/ String /*, LuaType)*/, String> {
		let name = self.assert_advance(IDENTIFIER, "<name>")?.lexeme();
		/*if self.locals.is_some() {
			let luatype = self.build_type()?;
			self.add_variable(name.to_string(), luatype.clone());
			Ok((name, luatype))
		} else {*/
		Ok(/*(*/ name /*, LuaType::ANY)*/)
		//}
	}

	fn build_variables(&mut self, local: bool, line: usize) -> Result<ComplexToken, String> {
		let mut names: Vec<String> = Vec::new();
		loop {
			let /*(*/pname/* , _)*/ = self.build_variable()?;
			names.push(pname);
			if !self.compare(COMMA) {
				self.advance_if(SEMICOLON);
				break;
			}
			self.current += 1;
		}
		let check = self.advance();
		let areinit = check.kind() == DEFINE;
		let values: Vec<Expression> = if !areinit {
			if local {
				Vec::new()
			} else {
				/*if let Some(_locals) = &self.locals {
					//println!("{:?}", locals);
				} else {
					self.warning("Defining external globals will not do anything if you don't have type checking enabled!", line)
				}*/
				self.current -= 1;
				return Ok(SYMBOL(String::new()));
			}
		} else {
			self.find_expressions(COMMA, None)?
		};
		self.current -= 1;
		Ok(VARIABLE {
			local,
			names,
			values,
			line,
		})
	}

	fn compile_static(&mut self, expr: Expression) {
		let code = compile_tokens(0, expr);
		self.statics += &(code + "\n");
	}

	fn build_match_case(
		&mut self,
		pexpr: Option<Expression>,
		func: &impl Fn(&mut ParserInfo /*, LocalsList*/) -> Result<CodeBlock, String>,
	) -> Result<MatchCase, String> {
		let mut conditions: Vec<Expression> = Vec::new();
		let mut current = Expression::new();
		let (expr, extraif) = match pexpr {
			Some(expr) => (expr, None),
			None => {
				self.current += 1;
				(
					self.build_expression(Some((IF, "if")))?,
					Some(self.build_expression(Some((ARROW, "=>")))?),
				)
			}
		};
		for ctoken in expr {
			match ctoken {
				SYMBOL(lexeme) if lexeme == " or " => {
					conditions.push(current.clone());
					current.clear();
				}
				_ => current.push_back(ctoken),
			}
		}
		if !current.is_empty() {
			conditions.push(current);
		}
		Ok((conditions, extraif, func(self /*, self.locals.clone()*/)?))
	}

	fn build_match_block(
		&mut self,
		name: String,
		func: &impl Fn(&mut ParserInfo /*, LocalsList*/) -> Result<CodeBlock, String>,
	) -> Result<ComplexToken, String> {
		let line = self.peek(0).line();
		let value = self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?;
		let mut branches: Vec<MatchCase> = Vec::new();
		while {
			if self.advance_if(DEFAULT) {
				let t = self.advance();
				match t.kind() {
					ARROW => {
						if branches.is_empty() {
							return Err(self.error(
								"The default case (with no extra if) of a match block must be the last case, not the first",
								t.line()));
						}
						branches.push((Vec::new(), None, func(self /*, self.locals.clone()*/)?));
						self.assert(CURLY_BRACKET_CLOSED, "}")?;
						false
					}
					IF => {
						let extraif = self.build_expression(Some((ARROW, "=>")))?;
						branches.push((
							Vec::new(),
							Some(extraif),
							func(self /*, self.locals.clone()*/)?,
						));
						!self.advance_if(CURLY_BRACKET_CLOSED)
					}
					_ => return Err(self.expected("=>", &t.lexeme(), t.line())),
				}
			} else {
				let (testexpr, reached) = self.test(|i| i.build_expression(Some((ARROW, "=>"))));
				branches.push(match testexpr {
					Err(msg) if msg == "Expected '=>', got 'if'" => {
						self.build_match_case(None, func)?
					}
					Ok(expr) => {
						self.current = reached;
						self.build_match_case(Some(expr), func)?
					}
					Err(msg) => return Err(self.error(msg, self.testing.unwrap())),
				});
				!self.advance_if(CURLY_BRACKET_CLOSED)
			}
		} {}
		Ok(MATCH_BLOCK {
			name,
			value,
			branches,
			line,
		})
	}

	fn parse_token_local_global(&mut self, t: &BorrowedToken) -> Result<(), String> {
		let local = t.kind() == LOCAL;
		match self.peek(0).kind() {
			FN => {
				let function = self.build_function(local)?;
				self.expr.push_back(function);
			}
			ENUM => {
				let enums = &mut self.build_enums(local)?;
				self.expr.append(enums);
			}
			_ => {
				let vars = self.build_variables(local, t.line())?;
				self.expr.push_back(vars);
			}
		}

		Ok(())
	}

	fn parse_token_static(&mut self, t: &BorrowedToken) -> Result<(), String> {
		match self.peek(0).kind() {
			FN => {
				let function = expression![self.build_function(true)?];
				self.compile_static(function);
			}
			ENUM => {
				let enums = self.build_enums(true)?;
				self.compile_static(enums);
			}
			_ => {
				let vars = expression![self.build_variables(true, t.line())?];
				self.compile_static(vars);
			}
		}

		Ok(())
	}

	fn parse_token_method(&mut self) -> Result<(), String> {
		let name = {
			let mut expr = Expression::new();
			loop {
				let t = self.advance();
				match t.kind() {
					IDENTIFIER => {
						let nt = self.peek(0);
						if nt.kind() == IDENTIFIER {
							return Err(self.unexpected(&nt.lexeme(), nt.line()));
						}
						expr.push_back(SYMBOL(t.lexeme()))
					}
					DOT => self.check_index(&t, &mut expr, ".")?,
					DOUBLE_COLON => {
						self.check_index(&t, &mut expr, ":")?;
						let t = self.peek(1);
						if t.kind() != ROUND_BRACKET_OPEN {
							return Err(self.expected("(", &t.lexeme(), t.line()));
						}
					}
					ROUND_BRACKET_OPEN => break,
					_ => return Err(self.expected("(", &t.lexeme(), t.line())),
				}
			}
			expr
		};
		let /*(*/args/*, types)*/ = if !self.advance_if(ROUND_BRACKET_CLOSED) {
			self.build_function_args()?
		} else {
			/*(*/FunctionArgs::new()//, None)
		};
		let code = self.build_function_block(/*types*/)?;
		//ADD FUNCTION FOR ADDING VALUES INSIDE TABLES MAYBE?
		self.expr.push_back(FUNCTION {
			local: false,
			name,
			args,
			code,
		});

		Ok(())
	}

	fn parse_token_identifier(&mut self, t: &BorrowedToken) -> Result<(), String> {
		let testexpr = self.test(|i| i.build_name()).0;
		if let Err(msg) = testexpr {
			return match msg.as_str() {
				"You can't call functions here"
				| "Unexpected token '?.'"
				| "Unexpected token '?['" => {
					let expr = &mut self.build_expression(None)?;
					self.expr.append(expr);
					self.current -= 1;
					Ok(())
				}
				_ => Err(self.error(msg, self.testing.unwrap())),
			};
		}
		self.current += 1;
		let mut names: Vec<Expression> = Vec::new();
		while {
			names.push(self.build_name()?);
			self.current += 1;
			self.look_back(1).kind() == COMMA
		} {}
		self.current -= 1;
		let checkt = self.look_back(0);
		let check = checkt.kind() as u8;
		if check < DEFINE as u8 || check > MODULATE as u8 {
			return Err(self.expected("=", &checkt.lexeme(), checkt.line()));
		}
		let values: Vec<Expression> = self.find_expressions(COMMA, None)?;
		self.expr.push_back(ALTER {
			kind: checkt.kind(),
			line: t.line(),
			names,
			values,
		});
		self.current -= 1;

		Ok(())
	}

	fn parse_token_round_bracket_open(&mut self) -> Result<(), String> {
		let expr = self.build_expression(Some((ROUND_BRACKET_CLOSED, ")")))?;
		self.expr.push_back(EXPR(expr));
		self.current += 1;
		let call = self.build_identifier()?;
		self.expr.push_back(call);
		self.current += 1;
		self.advance_if(SEMICOLON);

		Ok(())
	}

	fn parse_token_curly_bracket_open(&mut self) -> Result<(), String> {
		self.current -= 1;
		let block = self.build_code_block(/*self.locals.clone()*/)?;
		self.expr.push_back(DO_BLOCK(block));

		Ok(())
	}

	fn parse_token_if(&mut self) -> Result<(), String> {
		let ctoken = self.build_elseif_chain()?;
		self.expr.push_back(ctoken);

		Ok(())
	}

	fn parse_token_match(&mut self) -> Result<(), String> {
		let ctoken =
			self.build_match_block(String::from("_match"), &ParserInfo::build_code_block)?;
		self.expr.push_back(ctoken);

		Ok(())
	}

	fn parse_token_while(&mut self) -> Result<(), String> {
		let condition = self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?;
		let code = self.build_loop_block()?;
		self.expr.push_back(WHILE_LOOP { condition, code });

		Ok(())
	}

	fn parse_token_until(&mut self) -> Result<(), String> {
		let mut condition = self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?;
		condition.push_front(SYMBOL(String::from("not (")));
		condition.push_back(SYMBOL(String::from(")")));
		let code = self.build_loop_block()?;
		self.expr.push_back(WHILE_LOOP { condition, code });

		Ok(())
	}

	fn parse_token_loop(&mut self) -> Result<(), String> {
		let code = self.build_loop_block()?;
		if self.peek(0).kind() == UNTIL {
			self.current += 1;
			let condition = self.build_expression(None)?;
			self.current -= 1;
			self.expr.push_back(LOOP_UNTIL { condition, code })
		} else {
			self.expr.push_back(WHILE_LOOP {
				condition: expression![SYMBOL(String::from("true"))],
				code,
			})
		}

		Ok(())
	}

	fn parse_token_for(&mut self) -> Result<(), String> {
		if self.peek(1).kind() == DEFINE {
			let iterator = self.assert_advance(IDENTIFIER, "<name>")?.lexeme();
			self.current += 1;
			let start = self.build_expression(Some((COMMA, ",")))?;
			let end = self.build_expression(None)?;
			self.current -= 1;
			let t = self.advance();
			let alter = match t.kind() {
				CURLY_BRACKET_OPEN => {
					self.current -= 1;
					expression![SYMBOL(String::from("1"))]
				}
				COMMA => self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?,
				_ => return Err(self.expected(",", &t.lexeme(), t.line())),
			};
			let code = self.build_loop_block()?;
			self.expr.push_back(FOR_LOOP {
				iterator,
				start,
				end,
				alter,
				code,
			})
		} else {
			let iterators = self.build_identifier_list()?;
			let expr = match self.advance().kind() {
				OF => {
					let mut expr = expression![SYMBOL(String::from("pairs("))];
					expr.append(&mut self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?);
					expr.push_back(SYMBOL(String::from(")")));
					expr
				}
				IN => {
					let mut expr = expression![SYMBOL(String::from("ipairs("))];
					expr.append(&mut self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?);
					expr.push_back(SYMBOL(String::from(")")));
					expr
				}
				WITH => self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?,
				_ => {
					let t = self.peek(0);
					return Err(self.expected("of', 'in' or 'with", &t.lexeme(), t.line()));
				}
			};
			let code = self.build_loop_block()?;
			self.expr.push_back(FOR_FUNC_LOOP {
				iterators,
				expr,
				code,
			});
		}

		Ok(())
	}

	fn parse_token_continue(&mut self) -> Result<(), String> {
		self.expr.push_back(CONTINUE_LOOP);
		self.advance_if(SEMICOLON);

		Ok(())
	}

	fn parse_token_break(&mut self) -> Result<(), String> {
		self.expr.push_back(BREAK_LOOP);
		self.advance_if(SEMICOLON);

		Ok(())
	}

	fn parse_token_return(&mut self) -> Result<(), String> {
		let expr = if self.ended() || self.advance_if(SEMICOLON) {
			None
		} else {
			Some(self.find_expressions(COMMA, None)?)
		};
		self.expr.push_back(RETURN_EXPR(expr));

		Ok(())
	}

	fn parse_token_try(&mut self) -> Result<(), String> {
		let totry = self.build_code_block(/*self.locals.clone()*/)?;
		let error: Option<String>;
		let catch = if self.advance_if(CATCH) {
			let t = self.advance();
			if t.kind() == IDENTIFIER {
				error = Some(t.lexeme());
			} else {
				error = None;
				self.current -= 1;
			}
			Some(self.build_code_block(/*self.locals.clone()*/)?)
		} else {
			error = None;
			None
		};
		self.expr.push_back(TRY_CATCH {
			totry,
			error,
			catch,
		});

		Ok(())
	}

	fn parse_token_macro(&mut self) -> Result<(), String> {
		let name = self.assert_advance(IDENTIFIER, "<name>")?.lexeme();
		let code = self.build_expression(None)?;
		self.current -= 1;
		self.macros.insert(name, code);
		Ok(())
	}

	fn parse_token_fn_enum(&mut self, t: &BorrowedToken) -> Result<(), String> {
		Err(self.error(
			format!(
				"'{}' must have 'local', 'global' or 'static' beforehand",
				t.lexeme()
			),
			t.line(),
		))
	}
}

pub fn parse_tokens(
	tokens: Vec<Token>,
	//locals: Option<AHashMap<String, LuaType>>,
	filename: String,
) -> Result<Expression, String> {
	let mut i = ParserInfo::new(tokens /*, locals*/, filename);
	while !i.ended() {
		let t = i.advance();
		match t.kind() {
			LOCAL | GLOBAL => i.parse_token_local_global(&t)?,
			STATIC => i.parse_token_static(&t)?,
			METHOD => i.parse_token_method()?,
			IDENTIFIER => i.parse_token_identifier(&t)?,
			ROUND_BRACKET_OPEN => i.parse_token_round_bracket_open()?,
			CURLY_BRACKET_OPEN => i.parse_token_curly_bracket_open()?,
			IF => i.parse_token_if()?,
			MATCH => i.parse_token_match()?,
			WHILE => i.parse_token_while()?,
			UNTIL => i.parse_token_until()?,
			LOOP => i.parse_token_loop()?,
			FOR => i.parse_token_for()?,
			CONTINUE => i.parse_token_continue()?,
			BREAK => i.parse_token_break()?,
			RETURN => i.parse_token_return()?,
			TRY => i.parse_token_try()?,
			MACRO => i.parse_token_macro()?,
			FN | ENUM => i.parse_token_fn_enum(&t)?,
			EOF => break,
			_ => return Err(i.expected("<end>", &t.lexeme(), t.line())),
		}
	}

	if !i.statics.is_empty() {
		let debug = flag!(env_debug);
		let output = ENV_DATA
			.write()
			.expect("Can't lock env_data")
			.output_code()
			.to_string();
		ENV_DATA
			.write()
			.expect("Can't lock env_data")
			.rewrite_output_code(
				if debug {
					format!("--statics defined in \"{}\":\n{}\n", i.filename, i.statics)
				} else {
					i.statics
				} + &output,
			);
	}

	//println!("LOCALS = {:#?}", i.locals);

	Ok(i.expr)
}
