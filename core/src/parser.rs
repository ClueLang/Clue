//! The parser is the third step of the compilation process. It takes the tokens from the scanner and
//! converts them into an AST (Abstract Syntax Tree).
//!
//! The parser is a recursive descent parser, which means that it uses a recursive function to parse
//! the tokens. This function is called [`parse_tokens`] and is the entry point for the parser.

#![allow(non_camel_case_types)]

use self::ComplexToken::*;
use crate::{
	compiler::Compiler,
	env::{BitwiseMode, ContinueMode, LuaVersion, Options},
	scanner::{BorrowedToken, Token, TokenType, TokenType::*},
	ErrorMessaging,
	format_clue,
	check, impl_errormessaging,
};
use std::{cell::Cell, vec, cmp, collections::VecDeque};

#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

macro_rules! vec_deque {
	($($elem:expr),*) => {
		VecDeque::from([$(($elem)),*])
	};
}

macro_rules! bitwise {
	($self:expr, $t:expr, $expr:expr, $fname:literal, $end:expr, $notable:expr) => {{
		if $self.build_bitwise_op(&$t, &mut $expr, $fname, $end, $notable)? {
			break $t;
		}
	}};
}

/// A list of [`ComplexToken`]s, which is the AST.
pub type Expression = VecDeque<ComplexToken>;

/// Function arguments, a list of identifiers with optional default values.
/// used in function signatures.
pub type FunctionArgs = Vec<(String, Option<(Expression, usize)>)>;

//pub type LocalsList = Option<AHashMap<String, LuaType>>;
//pub type ArgsAndTypes = (FunctionArgs, Option<Vec<(String, LuaType)>>);

/// An optional end token, which is used to check if the end token is present.
/// It is a tuple of the token type and the token lexeme.
type OptionalEnd = Option<(TokenType, &'static str)>;

/// A tuple representing a match case, containing the things you can match, it's internal code, an optional condition and a code block.
/// In the example
/// ```clue
/// match x {
///   1 if z == 0 => {foo()},
/// }
/// ```
/// the first element of the tuple would be `1`, the second element would be `z == 0`
/// and the third element would be `{foo()}`.
type MatchCase = (Vec<Expression>, Expression, Option<Expression>, CodeBlock);

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// An enum representing all the possible complex tokens that can be parsed
/// such as functions, if statements, variables, tables etc.
pub enum ComplexToken {
	/// A variable declaration.
	VARIABLE {
		/// Whether the variable(s) is/are local or not.
		local: bool,

		/// The names of the variable(s).
		names: Vec<String>,

		/// The values of the variable(s).
		values: Vec<Expression>,

		/// The line number of the variable declaration.
		line: usize,
	},

	/// An assignment to a variable or a list of variables.
	ALTER {
		/// The kind of assignment (e.g. `+=`, `-=`, `=`)
		kind: TokenType,

		/// The names of the variable(s).
		names: VecDeque<Expression>,

		/// The values of the variable(s).
		values: Vec<Expression>,

		/// The line number of the assignment.
		line: usize,
	},

	/// A table.
	TABLE {
		/// the table's keys and values values.
		values: Vec<(Option<Expression>, Expression, usize)>,

		/// the table's metamethods.
		metas: Vec<(String, Expression, usize)>,

		/// the table's metatable.
		metatable: Option<String>,
	},

	/// A function declaration.
	FUNCTION {
		/// Whether the function is local or not.
		local: bool,

		/// The name of the function.
		name: Expression,

		/// The arguments of the function.
		args: FunctionArgs,

		/// The code block of the function.
		code: CodeBlock,
	},

	/// A lambda function.
	LAMBDA {
		/// The arguments of the function.
		args: FunctionArgs,

		/// The code block of the function.
		code: CodeBlock,
	},

	/// An if statement.
	IF_STATEMENT {
		/// The condition of the if statement.
		condition: Expression,

		/// The code block of the if statement.
		code: CodeBlock,

		/// The next elseif/else statement.
		next: Option<Box<ComplexToken>>,
	},

	/// A match statement.
	MATCH_BLOCK {
		/// The name of the internal variable used to store the matched expression.
		name: String,

		/// The expression to match on.
		value: Expression,

		/// The list of match cases.
		branches: Vec<MatchCase>,

		/// The line number of the match statement.
		line: usize,
	},

	/// A while loop.
	WHILE_LOOP {
		/// The condition of the while loop.
		condition: Expression,

		/// The code block of the while loop.
		code: CodeBlock,

		/// The line number of the while loop.
		line: usize,
	},

	/// An until loop.
	LOOP_UNTIL {
		/// The condition of the loop.
		condition: Expression,

		/// The code block of the loop.
		code: CodeBlock,

		/// The line number of the loop.
		line: usize,
	},

	/// A for loop over a range of number e.g. `for i = 0, 10, 1 {...}`.
	FOR_LOOP {
		/// The iterator variable of the for loop.
		iterator: String,

		/// The starting value of the for loop.
		start: Expression,

		/// The last value of the for loop.
		end: Expression,

		/// The amount to alter the iterator every iteration.
		alter: Expression,

		/// The code block of the for loop.
		code: CodeBlock,

		/// The line number of the for loop.
		line: usize,
	},

	/// A for loop over a some iterator which can be either a for..in loop, a for..of loop or a for..with loop.
	FOR_FUNC_LOOP {
		/// The iterator of the for loop.
		iterators: Vec<String>,

		/// The expression of the for loop
		/// for..in loops: for k,v in pairs(t) do end
		/// for..of loops: for k,v in ipairs(t) do end
		/// for..with loops: for k,v in custom_iter(t) do end
		expr: Expression,

		/// The code block of the for loop.
		code: CodeBlock,

		/// The line number of the for loop.
		line: usize,
	},

	/// A try catch block.
	TRY_CATCH {
		/// The code block of the try block.
		totry: CodeBlock,
		
		/// An optional code block of the catch block.
		catch: Option<CodeBlock>,
		
		/// The name of the error variable in the catch block.
		error: Option<String>,
	},

	/// An identifier.
	IDENT {
		/// The expression of the identifier.
		expr: Expression,

		/// The line number of the identifier.
		line: usize,
	},

	/// Any symbol.
	SYMBOL(String),

	/// A function call.
	CALL(Vec<Expression>),

	/// An expression.
	EXPR(Expression),

	/// A do block.
	DO_BLOCK(CodeBlock),

	/// A return statement.
	RETURN_EXPR(Option<Vec<Expression>>),

	/// A continue keyword.
	CONTINUE_LOOP,

	/// A break keyword.
	BREAK_LOOP,
}

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
/// A code block.
pub struct CodeBlock {
	/// The start line of the code block.
	pub start: usize,

	/// The code of the code block.
	pub code: Expression,

	/// The end line of the code block.
	pub end: usize,
}

/*
#[derive(Clone, PartialEq, Eq, Debug)]
pub enum LuaType {
	ANY,
	NIL,
	NUMBER,
}
*/
struct ParserInfo<'a> {
	options: &'a Options,
	current: usize,
	size: usize,
	filename: &'a String,
	expr: Expression,
	tokens: Vec<Token>,
	//code: Code,
	internal_var_id: u8,
	internal_stack: Vec<Cell<Expression>>,
	statics: String,
	compiler: Compiler<'a>,
	errors: u8,
	//locals: LocalsList,
}

impl_errormessaging!(ParserInfo<'_>);

impl<'a> ParserInfo<'a> {
	fn new(
		tokens: Vec<Token>, /* , locals: LocalsList */
		//code: Code,
		filename: &'a String,
		options: &'a Options,
	) -> ParserInfo<'a> {
		ParserInfo {
			current: 0,
			size: tokens.len() - 1,
			filename,
			expr: Expression::with_capacity(tokens.len()),
			tokens,
			//code,
			internal_var_id: 0,
			internal_stack: Vec::new(),
			statics: String::new(),
			compiler: Compiler::new(options, filename),
			options,
			errors: 0,
			// locals,
		}
	}

	/*
		fn warning(&self, msg: impl Into<String>, line: usize) {
			println!(
				"Warning in \"{}\" at line {}!\nWarning: \"{}\"",
				self.filename,
				line,
				msg.into()
			);
		}
	*/

	fn error(&mut self, msg: impl Into<String>, line: usize, column: usize) -> String {
		eprintln!("Error in {}:{line}:{column}!", self.filename);
		msg.into()
	}

	fn expected(&mut self, expected: &str, got: &str, line: usize, column: usize) -> String {
		self.error(
			format_clue!("Expected '", expected, "', got '", got, "'"),
			line,
			column,
		)
	}

	fn expected_before(
		&mut self,
		expected: &str,
		before: &str,
		line: usize,
		column: usize,
	) -> String {
		self.error(
			format_clue!("Expected '", expected, "' before '", before, "'"),
			line,
			column,
		)
	}

	fn unexpected(&mut self, str: &str, line: usize, column: usize) -> String {
		self.error(format_clue!("Unexpected token '", str, "'"), line, column)
	}

	const fn ended(&self) -> bool {
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
			return Err(self.expected(error, &t.lexeme(), t.line(), t.column()));
		}
		Ok(t)
	}

	fn assert_compare(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.compare(expected) {
			let t = self.peek(0);
			return Err(self.expected(error, &t.lexeme(), t.line(), t.column()));
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
				return Err(self.expected(
					lexeme,
					&tocheck.lexeme(),
					tocheck.line(),
					tocheck.column(),
				));
			}
		}
		Ok(iftrue)
	}

	fn assert(&mut self, expected: TokenType, error: &str) -> Result<(), String> {
		if !self.advance_if(expected) {
			let t = self.peek(0);
			return Err(self.expected(error, &t.lexeme(), t.line(), t.column()));
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
		let var = format_clue!("_internal", self.internal_var_id.to_string());
		self.internal_var_id += 1;
		var
	}

	fn build_call(&mut self) -> Result<Vec<Expression>, String> {
		let args: Vec<Expression> = if self.advance_if(ROUND_BRACKET_CLOSED) {
			Vec::new()
		} else {
			self.find_expressions(Some((ROUND_BRACKET_CLOSED, ")")))?
		};
		Ok(args)
	}

	fn find_expressions(&mut self, end: OptionalEnd) -> Result<Vec<Expression>, String> {
		let mut exprs: Vec<Expression> = Vec::new();
		loop {
			let expr = self.build_expression(None)?;
			let t = self.look_back(0);
			exprs.push(expr);
			if t.kind() != COMMA {
				return self.assert_end(&t, end, exprs);
			}
		}
	}

	fn build_table(&mut self) -> Result<ComplexToken, String> {
		let mut values: Vec<(Option<Expression>, Expression, usize)> = Vec::new();
		let mut metas: Vec<(String, Expression, usize)> = Vec::new();
		let mut metatable: Option<String> = None;
		while !self.advance_if(CURLY_BRACKET_CLOSED) {
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
				EOF => {
					return Err(self.expected_before(
						"}",
						"<end>",
						self.peek(0).line(),
						self.peek(0).column(),
					))
				}
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
					name = Ok(vec_deque![SYMBOL(pn.lexeme())]);
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
						EOF => {
							return Err(self.expected_before(
								"]",
								"<end>",
								self.peek(0).line(),
								self.peek(0).column(),
							))
						}
						_ => true,
					} {}
					self.current = start;
					name = Ok(vec_deque![
						SYMBOL(String::from("[")),
						EXPR(self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?),
						SYMBOL(String::from("]"))
					]);
				}
				META => {
					if self.advance_if(WITH) {
						if !metas.is_empty() {
							return Err(self.error(
								"An external metatable cannot be used if the table already set its own metamethods",
								pn.line(),
								pn.column()
							));
						}
						metatable = Some(self.assert_advance(IDENTIFIER, "<name>")?.lexeme());
						self.advance_if(COMMA);
						continue;
					}

					if metatable.is_some() {
						return Err(self.error(
								"Metamethods cannot be set if the table already uses an external metatable",
								pn.line(),
								pn.column()
							));
					}
					let name_token = self.advance();
					name = Err(String::from(match name_token.lexeme().as_ref() {
						"index" => "__index",
						"newindex" => "__newindex",
						"usedindex" => {
							if matches!(self.options.env_target, Some(LuaVersion::BLUA)) {
								"__usedindex"
							} else {
								return Err(self.error(
								"The 'usedindex' metamethod can only be used with --target=blua",
								name_token.line(),
								name_token.column()
							));
							}
						}
						"mode" => "__mode",
						"call" => "__call",
						"metatable" => "__metatable",
						"tostring" => "__tostring",
						"len" => "__len",
						"pairs" => "__pairs",
						"ipairs" => "__ipairs",
						"gc" => "__gc",
						"name" => "__name",
						"close" => "__close",
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
							return Err(self.expected(
								"<meta name>",
								&name_token.lexeme(),
								name_token.line(),
								name_token.column(),
							));
						}
					}))
				}
				_ => return Err(self.expected("<name>", &pn.lexeme(), pn.line(), pn.column())),
			}
			if !self.advance_if(DEFINE) {
				let t = self.peek(0);
				return Err(self.expected("=", &t.lexeme(), t.line(), t.column()));
			}
			let start = self.current;
			let mut cscope = 0u8;
			while match self.peek(0).kind() {
				COMMA | CURLY_BRACKET_CLOSED => cscope != 0,
				ROUND_BRACKET_OPEN | SAFE_CALL => {
					cscope += 1;
					true
				}
				ROUND_BRACKET_CLOSED => {
					if cscope == 0 {
						let t = self.peek(0);
						return Err(self.expected_before("(", ")", t.line(), t.column()));
					}
					cscope -= 1;
					true
				}
				EOF => {
					return Err(self.expected_before(
						"}",
						"<end>",
						self.peek(0).line(),
						self.peek(0).column(),
					))
				}
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
		checkback: Option<&Expression>,
	) -> Result<(), String> {
		if match self.peek(0).kind() {
			NUMBER | IDENTIFIER | STRING | TRUE | FALSE | MINUS | BIT_NOT | NIL | NOT | HASHTAG
			| ROUND_BRACKET_OPEN | THREEDOTS | MATCH => false,
			CURLY_BRACKET_OPEN => {
				*notable = false;
				false
			}
			_ => true,
		} {
			return Err(self.error(
				format!("Operator '{}' has invalid right hand token", t.lexeme()),
				t.line(),
				t.column(),
			));
		}
		if let Some(expr) = checkback {
			if expr.is_empty() {
				return Err(self.error(
					format!("Operator '{}' lacks a left hand token", t.lexeme()),
					t.line(),
					t.column(),
				));
			} else if !matches!(
				self.look_back(1).kind(),
				NUMBER
					| IDENTIFIER | STRING
					| TRUE | FALSE | NIL | ROUND_BRACKET_CLOSED
					| SQUARE_BRACKET_CLOSED
					| THREEDOTS | CURLY_BRACKET_CLOSED
			) {
				return Err(self.error(
					format!("Operator '{}' has invalid left hand token", t.lexeme()),
					t.line(),
					t.column(),
				));
			}
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
		self.check_operator(t, notable, Some(expr))?;
		let mut arg1 = Expression::with_capacity(expr.len());
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
	) -> Result<bool, String> {
		self.check_operator(t, notable, Some(expr))?;
		Ok(if let Some(bit) = &self.options.env_jitbit {
			self.build_function_op(t, expr, format!("{bit}.{fname}"), end, notable)?;
			self.check_val()
		} else {
			expr.push_back(SYMBOL(t.lexeme()));
			false
		})
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
				self.peek(0).column(),
			));
		}
		expr.push_back(SYMBOL(lexeme.to_owned()));
		Ok(())
	}

	fn check_val(&mut self) -> bool {
		match self.peek(0).kind() {
			NUMBER | IDENTIFIER | STRING | TRUE | BIT_NOT | FALSE | NIL | NOT | HASHTAG
			| CURLY_BRACKET_OPEN | THREEDOTS | MATCH => {
				self.current += 1;
				true
			}
			_ => false,
		}
	}

	fn get_prev_expr(&mut self) -> &mut Expression {
		return match self.internal_stack.last_mut() {
			Some(last) => last.get_mut(),
			None => &mut self.expr
		};
	}

	fn use_internal_stack<T>(
		&mut self,
		f: impl FnOnce(&mut Self) -> Result<T, String>
	) -> Result<(T, Expression), String> {
		self.internal_stack.push(Cell::new(Expression::new()));
		let result = f(self)?;
		let code = self.internal_stack.pop().unwrap().into_inner();
		Ok((result, code))
	}

	fn build_expression(&mut self, end: OptionalEnd) -> Result<Expression, String> {
		let mut expr = Expression::with_capacity(16);
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
					self.check_operator(&t, notable, Some(&expr))?;
					expr.push_back(SYMBOL(t.lexeme()))
				}
				MINUS => {
					self.check_operator(&t, notable, None)?;
					expr.push_back(SYMBOL(if self.look_back(1).kind() == MINUS {
						format!(" {}", t.lexeme())
					} else {
						t.lexeme()
					}))
				}
				FLOOR_DIVISION => {
					self.check_operator(&t, notable, Some(&expr))?;
					let mut division = Expression::with_capacity(expr.len());
					division.append(&mut expr);
					division.push_back(SYMBOL(String::from('/')));
					division.append(&mut self.build_expression(end)?);
					expr.push_back(SYMBOL(String::from("math.floor")));
					expr.push_back(CALL(vec![division]));
					self.current -= 1;
				}
				BIT_AND => bitwise!(self, t, expr, "band", end, notable),
				BIT_OR => bitwise!(self, t, expr, "bor", end, notable),
				BIT_XOR => {
					//SAFETY: the token goes out of scope after BorrowedToken is used, so it stays valid
					let t2 = if self.options.env_bitwise == BitwiseMode::Vanilla {
						Token::new(t.kind(), '~', t.position())
					} else {
						t.into_owned()
					};
					if self.build_bitwise_op(&BorrowedToken::new(&t2), &mut expr, "bxor", end, notable)? {
						break t;
					}
				}
				BIT_NOT => {
					self.check_operator(&t, notable, None)?;
					if let Some(bit) = self.options.env_jitbit.clone() {
						let arg = self.build_expression(end)?;
						expr.push_back(SYMBOL(bit.clone() + ".bnot"));
						expr.push_back(CALL(vec![arg]));
						self.current -= 1;
						if self.check_val() {
							break t;
						}
					} else {
						expr.push_back(SYMBOL(t.lexeme()))
					}
				}
				LEFT_SHIFT => bitwise!(self, t, expr, "lshift", end, notable),
				RIGHT_SHIFT => bitwise!(self, t, expr, "rshift", end, notable),
				NOT_EQUAL => {
					self.check_operator(&t, notable, Some(&expr))?;
					expr.push_back(SYMBOL(String::from("~=")))
				}
				HASHTAG => {
					if !matches!(
						self.peek(0).kind(),
						IDENTIFIER | CURLY_BRACKET_OPEN | ROUND_BRACKET_OPEN
					) {
						let t = self.peek(0);
						return Err(self.expected("<table>", &t.lexeme(), t.line(), t.column()));
					}
					expr.push_back(SYMBOL(String::from("#")))
				}
				/*SAFE_EXPRESSION => {
					self.assert(ROUND_BRACKET_OPEN, "(")?;
					self.current += 1;
					expr.push_back(PGET(self.build_identifier(true)?));
				}*/
				AND => {
					self.check_operator(&t, notable, Some(&expr))?;
					expr.push_back(SYMBOL(String::from(" and ")))
				}
				OR => {
					self.check_operator(&t, notable, Some(&expr))?;
					expr.push_back(SYMBOL(String::from(" or ")))
				}
				NOT => {
					self.check_operator(&t, notable, None)?;
					expr.push_back(SYMBOL(String::from("not ")))
				}
				MATCH => {
					let name = self.get_next_internal_var();
					let ident = SYMBOL(name.clone());
					let mut ctoken = self.build_match_block(name, &|i /* , _ */| {
						let start = i.peek(0).line();
						let (expr, mut code) = i.use_internal_stack(|i| i.build_expression(None))?;
						let end = i.look_back(1).line();
						if matches!(i.look_back(0).kind(), CURLY_BRACKET_CLOSED | DEFAULT) {
							i.current -= 1
						}
						code.push_back(ALTER {
							kind: DEFINE,
							names: vec_deque![vec_deque![ident.clone()]],
							values: vec![expr],
							line: end
						});
						Ok(CodeBlock { start, code, end })
					})?;
					let MATCH_BLOCK {branches, line, ..} = &mut ctoken else {
						unreachable!()
					};
					let last_branch = branches.last().unwrap();
					if !(last_branch.0.is_empty() && last_branch.2.is_none()) {
						branches.push((Vec::new(), Expression::new(), None, CodeBlock {
							start: *line,
							code: vec_deque![ALTER {
								kind: DEFINE,
								names: vec_deque![vec_deque![ident.clone()]],
								values: vec![vec_deque![SYMBOL(String::from("nil"))]],
								line: *line
							}],
							end: *line
						}))
					}
					self.get_prev_expr().push_back(ctoken);
					expr.push_back(ident);
					if self.check_val() {
						break t;
					}
				}
				COALESCE => {
					let mut leftexpr = Expression::with_capacity(expr.len());
					leftexpr.append(&mut expr);
					let (rightexpr, mut code) = self.use_internal_stack(|i| i.build_expression(end))?;
					self.current -= 1;
					let name = self.get_next_internal_var();
					let start = self.at(start).line();
					let end = self.at(self.current).line();
					let prev_expr = self.get_prev_expr();
					prev_expr.push_back(VARIABLE {
						line: start,
						local: true,
						names: vec![name.clone()],
						values: vec![leftexpr],
					});
					let name = SYMBOL(name);
					code.push_back(ALTER {
						kind: DEFINE,
						line: t.line(),
						names: vec_deque![vec_deque![name.clone()]],
						values: vec![rightexpr]
					});
					prev_expr.push_back(IF_STATEMENT {
						condition: vec_deque![name.clone(), SYMBOL(String::from(" == nil"))],
						code: CodeBlock {
							start: t.line(),
							code,
							end,
						},
						next: None,
					});
					expr.push_back(name);
					if self.check_val() {
						break t;
					}
				}
				QUESTION_MARK => {
					let mut condition = Expression::with_capacity(expr.len());
					condition.append(&mut expr);
					let (exprtrue, mut codetrue) = self.use_internal_stack(
						|i| i.build_expression(Some((COLON, ":")))
					)?;
					let t2 = self.look_back(0);
					let (exprfalse, mut codefalse) = self.use_internal_stack(
						|i| i.build_expression(end)
					)?;
					self.current -= 1;
					let name = self.get_next_internal_var();
					let start = self.at(start).line();
					let end = self.at(self.current).line();
					let prev_expr = self.get_prev_expr();
					prev_expr.push_back(VARIABLE {
						line: t.line(),
						local: true,
						names: vec![name.clone()],
						values: Vec::new(),
					});
					let name = SYMBOL(name);
					codetrue.push_back(ALTER {
						kind: DEFINE,
						line: t.line(),
						names: vec_deque![vec_deque![name.clone()]],
						values: vec![exprtrue]
					});
					codefalse.push_back(ALTER {
						kind: DEFINE,
						line: t.line(),
						names: vec_deque![vec_deque![name.clone()]],
						values: vec![exprfalse]
					});
					prev_expr.push_back(IF_STATEMENT {
						condition,
						code: CodeBlock {
							start,
							code: codetrue,
							end: t2.line(),
						},
						next: Some(Box::new(DO_BLOCK(CodeBlock {
							start: t2.line(),
							code: codefalse,
							end,
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
					if self.check_val() {
						break t;
					}
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
			return Err(self.expected("<expr>", &last.lexeme(), last.line(), last.column()));
		}
		self.assert_end(&self.look_back(0), end, expr)
	}

	fn build_name(&mut self) -> Result<Expression, String> {
		Ok(vec_deque![self.build_identifier()?])
	}

	fn build_identifier(&mut self) -> Result<ComplexToken, String> {
		let line = self.look_back(0).line();
		let (mut expr, safe_indexing) = self.build_identifier_internal()?;
		if safe_indexing {
			expr.push_front(SYMBOL(String::from("(")));
			expr.push_back(SYMBOL(String::from(")")));
		}
		Ok(IDENT { expr, line })
	}

	fn build_safe_index(
		&mut self,
		normal_kind: TokenType,
		kind: TokenType,
		expr: &mut Expression,
	) -> bool {
		if (kind as u8).wrapping_sub(6) != normal_kind as u8 {
			return false;
		}
		let mut safe_expr = Expression::with_capacity(expr.len());
		safe_expr.append(expr);
		let name = self.get_next_internal_var();
		self.expr.push_back(VARIABLE {
			local: true,
			names: vec![name.clone()],
			values: vec![safe_expr],
			line: self.peek(0).line(),
		});
		expr.push_back(SYMBOL(name.clone()));
		expr.push_back(SYMBOL(String::from(" and ")));
		expr.push_back(SYMBOL(name));
		true
	}

	fn build_identifier_internal(&mut self) -> Result<(Expression, bool), String> {
		let mut expr = Expression::with_capacity(8);
		let mut safe_indexing = false;
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
				DOT | SAFE_DOT => {
					safe_indexing |= self.build_safe_index(DOT, t.kind(), &mut expr);
					self.check_index(&t, &mut expr, ".")?;
				}
				DOUBLE_COLON | SAFE_DOUBLE_COLON => {
					safe_indexing |= self.build_safe_index(DOUBLE_COLON, t.kind(), &mut expr);
					self.check_index(&t, &mut expr, ":")?;
					match self.peek(1).kind() {
						ROUND_BRACKET_OPEN => {}
						SAFE_CALL => {
							expr.pop_back();
							let position = self.peek(0).position();
							let name = if t.kind() == DOUBLE_COLON {
								let mut start = {
									let mut start = Expression::with_capacity(2);
									if let Some(SYMBOL(lexeme)) = expr.get(1) {
										if lexeme == " and " {
											start.push_back(expr[0].clone());
											start.push_back(expr[1].clone());
										}
									}
									start
								};
								let mut expr_self = Expression::with_capacity(expr.len());
								expr_self.append(&mut expr);
								let name = self.get_next_internal_var();
								self.expr.push_back(VARIABLE {
									local: true,
									names: vec![name.clone()],
									values: vec![expr_self],
									line: position.start.line,
								});
								expr.append(&mut start);
								expr.push_back(SYMBOL(name.clone()));
								name
							} else {
								let SYMBOL(name) = &expr[0] else {
									unreachable!();
								};
								name.to_owned()
							};
							expr.push_back(SYMBOL(String::from(".")));
							self.tokens
								.insert(self.current + 2, Token::new(IDENTIFIER, name, position.clone()));
							if self.peek(3).kind() != ROUND_BRACKET_CLOSED {
								self.tokens.insert(
									self.current + 3,
									Token::new(COMMA, String::from(","), position),
								);
								self.size += 2;
							} else {
								self.size += 1;
							}
						}
						_ => {
							let t = self.peek(1);
							return Err(self.expected("(", &t.lexeme(), t.line(), t.column()));
						}
					}
				}
				SQUARE_BRACKET_OPEN | SAFE_SQUARE_BRACKET => {
					safe_indexing |=
						self.build_safe_index(SQUARE_BRACKET_OPEN, t.kind(), &mut expr);
					let qexpr = self.build_expression(Some((SQUARE_BRACKET_CLOSED, "]")))?;
					expr.push_back(SYMBOL(String::from("[(")));
					expr.push_back(EXPR(qexpr));
					expr.push_back(SYMBOL(String::from(")]")));
					if self.check_val() {
						break;
					}
				}
				ROUND_BRACKET_OPEN | SAFE_CALL => {
					safe_indexing |= self.build_safe_index(ROUND_BRACKET_OPEN, t.kind(), &mut expr);
					expr.push_back(CALL(self.build_call()?));
					if self.check_val() {
						break;
					}
				}
				_ => break,
			}
		}
		Ok((expr, safe_indexing))
	}

	fn get_code_block_start(&mut self) -> Result<usize, String> {
		let t = self.advance();
		if t.kind() != CURLY_BRACKET_OPEN {
			self.current -= 2;
			let t2 = self.advance();
			if t2.kind() == CURLY_BRACKET_OPEN {
				Ok(t2.line())
			} else {
				Err(self.expected("{", &t.lexeme(), t.line(), t.column()))
			}
		} else {
			Ok(t.line())
		}
	}

	fn parse_code_block(
		&mut self,
		mut tokens: Vec<Token>,
		//locals: LocalsList,
	) -> Result<Expression, String> {
		if tokens.is_empty() {
			Ok(Expression::new())
		} else {
			tokens.push(self.tokens.last().unwrap().clone());
			let (ctokens, statics) = parse_tokens(tokens, self.filename, self.options)?;
			self.statics += &statics;
			Ok(ctokens)
		}
	}

	fn build_code_block(&mut self /* , locals: LocalsList */) -> Result<CodeBlock, String> {
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
				EOF => return Err(self.expected_before("}", "<end>", t.line(), t.column())),
				_ => {}
			}
			tokens.push(t.into_owned());
		}
		let code = self.parse_code_block(tokens /* , locals */)?;
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
		let mut hascontinue: Option<String> = None;
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
					let name = self.get_next_internal_var();
					hascontinue = Some(name.clone());
					let position = t.position();
					if self.options.env_continue == ContinueMode::MoonScript {
						tokens.push(Token::new(IDENTIFIER, name, position.clone()));
						tokens.push(Token::new(DEFINE, "=", position.clone()));
						tokens.push(Token::new(TRUE, "true", position.clone()));
						tokens.push(Token::new(BREAK, "break", position));
						continue;
					}
				}
				EOF => return Err(self.expected_before("}", "<end>", t.line(), t.column())),
				_ => {}
			}
			tokens.push(t.into_owned());
		}
		let mut code = self.parse_code_block(tokens /* , self.locals.clone() */)?;
		if let Some(name) = hascontinue {
			use ContinueMode::*;
			match self.options.env_continue {
				Simple => {}
				Goto | LuaJIT => code.push_back(SYMBOL(String::from("::continue::"))),
				MoonScript => {
					code.push_back(ALTER {
						kind: DEFINE,
						names: vec_deque![vec_deque![SYMBOL(name.clone())]],
						values: vec![vec_deque![SYMBOL(String::from("true"))]],
						line: end,
					});
					code = vec_deque![
						VARIABLE {
							local: true,
							names: vec![name.clone()],
							values: vec![vec_deque![SYMBOL(String::from("false"))]],
							line: start
						},
						LOOP_UNTIL {
							condition: vec_deque![SYMBOL(String::from("true"))],
							code: CodeBlock { start, code, end },
							line: start
						},
						IF_STATEMENT {
							condition: vec_deque![SYMBOL(String::from("not ")), SYMBOL(name)],
							code: CodeBlock {
								start: end,
								code: vec_deque![BREAK_LOOP],
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
		loop {
			let t = self.assert_advance(IDENTIFIER, "<name>")?;
			idents.push(t.lexeme());
			if !self.advance_if(COMMA) {
				break Ok(idents);
			}
		}
	}

	fn build_function_args(&mut self) -> Result</* ArgsAndTypes */ FunctionArgs, String> {
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
					_ => return Err(self.expected("<name>", &t.lexeme(), t.line(), t.column())),
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
								return Err(self.expected(")", &t.lexeme(), t.line(), t.column()));
							}
						}
					}
					notended
				}
				ROUND_BRACKET_CLOSED => {
					args.push((name.lexeme(), None));
					false
				}
				_ => return Err(self.expected(")", &t.lexeme(), t.line(), t.column())),
			}
		} {}
		Ok(/* (args, types) */ args)
	}

	fn build_elseif_chain(&mut self, condition: Option<Expression>) -> Result<ComplexToken, String> {
		let condition = match condition {
			Some(condition) => condition,
			None => {
				if self.advance_if(LOCAL) {
					let start = self.look_back(0).line();
					let destructure = self.advance_if(CURLY_BRACKET_OPEN);
					let (vars, mut code) = self.use_internal_stack(
						|i| i.build_variables(true, start, destructure)
					)?;
					let (condition, end) = {
						let VARIABLE {names, line: end, ..} = &vars else {
							unreachable!()
						};
						let mut condition = Expression::with_capacity(names.len());
						let mut names = names.iter();
						let first = names.next().unwrap();
						condition.push_back(SYMBOL(format_clue!(first, " ~= nil")));
						for name in names {
							condition.push_back(SYMBOL(format_clue!(" and ", name, " ~= nil")))
						}
						(condition, *end)
					};
					code.push_back(vars);
					code.push_back(self.build_elseif_chain(Some(condition))?);
					return Ok(DO_BLOCK(CodeBlock { start, code, end }))
				}
				self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?
			}
		};
		let code = self.build_code_block(/*self.locals.clone()*/)?;
		Ok(IF_STATEMENT {
			condition,
			code,
			next: {
				let t = self.advance();
				match t.kind() {
					ELSEIF => Some(Box::new(self.build_elseif_chain(None)?)),
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
						return Err(self.error(
							"Enums values should be a non-float number ranging from -32768 to 32767.",
							t.line(),
							t.column()
						));
					}
					n = check!(t.lexeme().parse());
					self.advance_if(COMMA);
					SYMBOL(n.to_string())
				}
				_ => return Err(self.expected("}", &t.lexeme(), t.line(), t.column())),
			};
			enums.push_back(VARIABLE {
				line: name.line(),
				local,
				names: vec![name.lexeme()],
				values: vec![vec_deque![value]],
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
		let name = vec_deque![SYMBOL(t.lexeme())];
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
	/*fn build_variable(&mut self) -> Result</*(*/ String /*, LuaType)*/, String> {
		let name = self.assert_advance(IDENTIFIER, "<name>")?.lexeme();
		/*if self.locals.is_some() {
			let luatype = self.build_type()?;
			self.add_variable(name.to_string(), luatype.clone());
			Ok((name, luatype))
		} else {*/
		Ok(/*(*/ name /*, LuaType::ANY)*/)
		//}
	}*/

	#[allow(clippy::type_complexity)]
	fn build_destructure_table(&mut self) -> Result<(Vec<String>, Vec<String>, Vec<String>), String> {
		let mut names = Vec::new();
		let mut key_names = Vec::new();
		let name = self.get_next_internal_var();
		let mut internal_names = vec![name.clone()];
		self.build_destructure_table_internal(
			&mut names,
			&mut key_names,
			&mut internal_names,
			name + ".",
		)?;
		Ok((names, key_names, internal_names))
	}

	fn build_destructure_table_internal(
		&mut self,
		names: &mut Vec<String>,
		key_names: &mut Vec<String>,
		internal_names: &mut Vec<String>,
		key_start: String,
	) -> Result<(), String> {
		loop {
			let t = self.assert_advance(IDENTIFIER, "<name>")?;
			names.push(if self.advance_if(ARROW) {
				if self.advance_if(CURLY_BRACKET_OPEN) {
					let name = self.get_next_internal_var();
					internal_names.push(format_clue!(key_start, t.lexeme()));
					internal_names.push(name.clone());
					self.build_destructure_table_internal(
						names,
						key_names,
						internal_names,
						name + ".",
					)?;
					if self.advance_if(COMMA) {
						continue;
					} else {
						self.assert_advance(CURLY_BRACKET_CLOSED, "}")?;
						break Ok(());
					}
				} else {
					self.assert_advance(IDENTIFIER, "<name>")?.lexeme()
				}
			} else {
				t.lexeme()
			});
			key_names.push(format_clue!(key_start, t.lexeme()));
			if !self.advance_if(COMMA) {
				self.assert_advance(CURLY_BRACKET_CLOSED, "}")?;
				break Ok(());
			}
		}
	}

	fn build_table_destructuring(
		&mut self,
		internal_names: Vec<String>,
		values: Vec<Expression>,
		line: usize,
	) {
		let prev_expr = self.get_prev_expr();
		let mut names = internal_names.into_iter();
		prev_expr.push_back(VARIABLE {
			local: true,
			names: vec![names.next().unwrap()],
			values,
			line,
		});
		while let (Some(prev_name), Some(name)) = (names.next(), names.next()) {
			prev_expr.push_back(VARIABLE {
				local: true,
				names: vec![name.clone()],
				values: vec![vec_deque![SYMBOL(prev_name)]],
				line,
			});
		}
	}

	fn build_variables(
		&mut self,
		local: bool,
		line: usize,
		destructure: bool,
	) -> Result<ComplexToken, String> {
		let (names, destructure) = if destructure {
			let (names, key_names, internal_names) = self.build_destructure_table()?;
			(names, Some((key_names, internal_names)))
		} else {
			(self.build_identifier_list()?, None)
		};
		let check = self.advance().kind();
		let mut values: Vec<Expression> = if check != DEFINE {
			if check == SEMICOLON {
				self.current += 1;
			}
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
			self.find_expressions(None)?
		};
		self.current -= 1;
		if let Some((key_names, internal_names)) = destructure {
			self.build_table_destructuring(internal_names, values, line);
			values = Vec::new();
			for key_name in key_names {
				values.push(vec_deque![SYMBOL(key_name)])
			}
		}
		Ok(VARIABLE {
			local,
			names,
			values,
			line,
		})
	}

	fn compile_static(&mut self, expr: Expression) -> Result<(), String> {
		let code = self.compiler.compile_tokens(0, expr)?;
		self.statics += &(code + "\n");
		Ok(())
	}

	fn build_match_block(
		&mut self,
		name: String,
		func: &impl Fn(&mut ParserInfo<'a> /* , LocalsList */) -> Result<CodeBlock, String>,
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
								t.line(),
								t.column()
							));
						}
						branches.push((Vec::new(), Expression::new(), None, func(self /* , self.locals.clone() */)?));
						self.assert(CURLY_BRACKET_CLOSED, "}")?;
						false
					}
					IF => {
						let (extra_if, internal_expr) = self.use_internal_stack(|i|
							i.build_expression(Some((ARROW, "=>")))
						)?;
						branches.push((
							Vec::new(),
							internal_expr,
							Some(extra_if),
							func(self /* , self.locals.clone() */)?,
						));
						!self.advance_if(CURLY_BRACKET_CLOSED)
					}
					_ => return Err(self.expected("=>", &t.lexeme(), t.line(), t.column())),
				}
			} else {
				let ((expr, extra_if), internal_expr) = self.use_internal_stack(|i| {
					let expr = i.build_expression(None)?;
					let t = i.look_back(0);
					let extra_if = match t.kind() {
						ARROW => None,
						IF => Some(i.build_expression(Some((ARROW, "=>")))?),
						_ => return Err(i.expected("=>", &t.lexeme(), t.line(), t.column()))
					};
					Ok((expr, extra_if))
				})?;
				let mut conditions: Vec<Expression> = Vec::new();
				let mut current = Expression::with_capacity(3);
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
				branches.push((
					conditions,
					internal_expr,
					extra_if,
					func(self /* , self.locals.clone() */)?
				));
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
				let destructure = self.advance_if(CURLY_BRACKET_OPEN);
				let vars = self.build_variables(local, t.line(), destructure)?;
				self.expr.push_back(vars);
			}
		}
		Ok(())
	}

	fn parse_token_static(&mut self, t: &BorrowedToken) -> Result<(), String> {
		match self.peek(0).kind() {
			FN => {
				let function = vec_deque![self.build_function(true)?];
				self.compile_static(function)?;
			}
			ENUM => {
				let enums = self.build_enums(true)?;
				self.compile_static(enums)?;
			}
			_ => {
				let vars = vec_deque![self.build_variables(true, t.line(), false)?];
				self.compile_static(vars)?;
			}
		}
		Ok(())
	}

	fn parse_token_method(&mut self) -> Result<(), String> {
		let name = {
			let mut expr = Expression::with_capacity(4);
			loop {
				let t = self.advance();
				match t.kind() {
					IDENTIFIER => {
						let nt = self.peek(0);
						if nt.kind() == IDENTIFIER {
							return Err(self.unexpected(&nt.lexeme(), nt.line(), nt.column()));
						}
						expr.push_back(SYMBOL(t.lexeme()))
					}
					DOT => self.check_index(&t, &mut expr, ".")?,
					DOUBLE_COLON => {
						self.check_index(&t, &mut expr, ":")?;
						let t = self.peek(1);
						if t.kind() != ROUND_BRACKET_OPEN {
							return Err(self.expected("(", &t.lexeme(), t.line(), t.column()));
						}
					}
					ROUND_BRACKET_OPEN => break,
					_ => return Err(self.expected("(", &t.lexeme(), t.line(), t.column())),
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
		let start = self.current - 1;
		let (mut first_expr, safe_indexing) = self.build_identifier_internal()?;
		if let CALL(_) = first_expr.back().unwrap() {
			let line = self.at(start).line();
			if safe_indexing {
				let name = SYMBOL({
					let SYMBOL(name) = first_expr.pop_front().unwrap() else {
						unreachable!()
					};
					first_expr.pop_front();
					first_expr.pop_front();
					name
				});
				first_expr.push_front(name.clone());
				self.expr.push_back(IF_STATEMENT {
					condition: vec_deque![name],
					code: CodeBlock {
						start: line,
						code: vec_deque![IDENT {
							expr: first_expr,
							line
						}],
						end: line,
					},
					next: None,
				});
				self.current -= 1;
			} else {
				self.expr.push_back(IDENT {
					expr: first_expr,
					line,
				});
				self.current -= 1;
				self.advance_if(SEMICOLON);
			}
			return Ok(());
		} else if safe_indexing {
			return Err(self.error(
				"Safe indexing cannot be used when altering variables",
				t.line(),
				t.column(),
			));
		}
		let mut names = vec_deque![first_expr];
		while {
			self.current += 1;
			self.look_back(1).kind() == COMMA
		} {
			names.push_back(self.build_name()?);
		}
		self.current -= 1;
		let checkt = self.look_back(0);
		let check = checkt.kind();
		if check < DEFINE || check > MODULATE {
			return Err(self.expected("=", &checkt.lexeme(), checkt.line(), checkt.column()));
		}
		let values = self.find_expressions(None)?;
		if check == DEFINE_COALESCE {
			for value in values {
				if let Some(name) = names.pop_front() {
					let mut condition = name.clone();
					condition.push_back(SYMBOL(String::from(" == nil")));
					self.expr.push_back(IF_STATEMENT {
						condition,
						code: CodeBlock {
							start: t.line(),
							code: vec_deque![ALTER {
								kind: DEFINE,
								names: vec_deque![name],
								values: vec![value],
								line: t.line()
							}],
							end: t.line(),
						},
						next: None,
					});
				} else {
					break;
				}
			}
		} else {
			self.expr.push_back(ALTER {
				kind: check,
				line: t.line(),
				names,
				values,
			});
		}
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
		let ctoken = self.build_elseif_chain(None)?;
		self.expr.push_back(ctoken);
		Ok(())
	}

	fn parse_token_match(&mut self) -> Result<(), String> {
		let name = self.get_next_internal_var();
		let ctoken = self.build_match_block(name, &ParserInfo::build_code_block)?;
		self.expr.push_back(ctoken);
		Ok(())
	}

	fn parse_token_while(&mut self, line: usize) -> Result<(), String> {
		let condition = self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?;
		let code = self.build_loop_block()?;
		self.expr.push_back(WHILE_LOOP { condition, code, line });
		Ok(())
	}

	fn parse_token_until(&mut self, line: usize) -> Result<(), String> {
		let mut condition = self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?;
		condition.push_front(SYMBOL(String::from("not (")));
		condition.push_back(SYMBOL(String::from(")")));
		let code = self.build_loop_block()?;
		self.expr.push_back(WHILE_LOOP { condition, code, line });
		Ok(())
	}

	fn parse_token_loop(&mut self, line: usize) -> Result<(), String> {
		let code = self.build_loop_block()?;
		let t = self.advance();
		match t.kind() {
			UNTIL => {
				let condition = self.build_expression(None)?;
				self.expr.push_back(LOOP_UNTIL { condition, code, line: t.line() })
			}
			WHILE => {
				let mut condition = self.build_expression(None)?;
				condition.push_front(SYMBOL(String::from("not (")));
				condition.push_back(SYMBOL(String::from(")")));
				self.expr.push_back(LOOP_UNTIL { condition, code, line: t.line() })
			}
			_ => self.expr.push_back(WHILE_LOOP {
				condition: vec_deque![SYMBOL(String::from("true"))],
				code,
				line,
			}),
		}
		self.current -= 1;
		Ok(())
	}

	fn parse_token_for(&mut self, line: usize) -> Result<(), String> {
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
					vec_deque![SYMBOL(String::from("1"))]
				}
				COMMA => self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?,
				_ => return Err(self.expected(",", &t.lexeme(), t.line(), t.column())),
			};
			let code = self.build_loop_block()?;
			self.expr.push_back(FOR_LOOP {
				iterator,
				start,
				end,
				alter,
				code,
				line,
			})
		} else {
			let iterators = self.build_identifier_list()?;
			let expr = match self.advance().kind() {
				OF => {
					let mut expr = vec_deque![SYMBOL(String::from("pairs("))];
					expr.append(&mut self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?);
					expr.push_back(SYMBOL(String::from(")")));
					expr
				}
				IN => {
					let mut expr = vec_deque![SYMBOL(String::from("ipairs("))];
					expr.append(&mut self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?);
					expr.push_back(SYMBOL(String::from(")")));
					expr
				}
				WITH => self.build_expression(Some((CURLY_BRACKET_OPEN, "{")))?,
				_ => {
					let t = self.peek(0);
					return Err(self.expected(
						"of', 'in' or 'with",
						&t.lexeme(),
						t.line(),
						t.column(),
					));
				}
			};
			let code = self.build_loop_block()?;
			self.expr.push_back(FOR_FUNC_LOOP {
				iterators,
				expr,
				code,
				line,
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
		let exprs = if self.ended() || self.advance_if(SEMICOLON) {
			None
		} else {
			Some(self.find_expressions(None)?)
		};
		self.expr.push_back(RETURN_EXPR(exprs));
		if !self.ended() {
			let t = self.look_back(0);
			return Err(self.expected("<end>", &t.lexeme(), t.line(), t.column()))
		}
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

	fn parse_token_fn_enum(&mut self, t: &BorrowedToken) -> Result<(), String> {
		Err(self.error(
			format!(
				"'{}' must have 'local', 'global' or 'static' beforehand",
				t.lexeme()
			),
			t.line(),
			t.column(),
		))
	}
}

/// Parses a list of tokens into an expression
/// Takes a list of [`Token`]s, a filename, and [`Options`]
/// Returns an expression and statics as a string
///
/// # Errors
/// Returns an [`Err`] containing the error message if an unexpected [`Token`] is found.
///
/// # Examples
/// ```
/// use clue_core::{env::Options, parser::*, preprocessor::*, scanner::*};
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
///     let (expr, statics) = parse_tokens(tokens, &filename, &options)?;
///
///     Ok(())
/// }
/// ```
pub fn parse_tokens(
	tokens: Vec<Token>,
	//code: Code,
	//locals: Option<AHashMap<String, LuaType>>,
	filename: &String,
	options: &Options,
) -> Result<(Expression, String), String> {
	let mut i = ParserInfo::new(tokens/*, code*/ /* , locals */, filename, options);
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
			WHILE => i.parse_token_while(t.line())?,
			UNTIL => i.parse_token_until(t.line())?,
			LOOP => i.parse_token_loop(t.line())?,
			FOR => i.parse_token_for(t.line())?,
			CONTINUE => i.parse_token_continue()?,
			BREAK => i.parse_token_break()?,
			RETURN => i.parse_token_return()?,
			TRY => i.parse_token_try()?,
			FN | ENUM => i.parse_token_fn_enum(&t)?,
			EOF => break,
			_ => return Err(i.expected("<end>", &t.lexeme(), t.line(), t.column())),
		}
	}

	//println!("LOCALS = {:#?}", i.locals);

	Ok((
		i.expr,
		if !i.statics.is_empty() && options.env_debug {
			format!("--statics defined in \"{}\":\n{}\n", i.filename, i.statics)
		} else {
			i.statics
		},
	))
}
