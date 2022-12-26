use std::fmt::Write;
use std::iter::{Iterator, Peekable};

use crate::env::Options;
use crate::{
	env::ContinueMode,
	format_clue,
	parser::{CodeBlock, ComplexToken, ComplexToken::*, Expression, FunctionArgs},
	scanner::TokenType::*,
};

pub struct Compiler<'a> {
	options: &'a Options,
}

impl<'a> Compiler<'a> {
	pub fn new(options: &'a Options) -> Self {
		Self { options }
	}

	fn indentate(&self, scope: usize) -> String {
		let mut result = String::with_capacity(128);
		for _ in 0..scope {
			result += "\t";
		}
		result
	}

	fn indentate_if<T: Iterator>(&self, ctokens: &mut Peekable<T>, scope: usize) -> String {
		match ctokens.peek() {
			Some(_) => {
				format!("\n{}", self.indentate(scope))
			}
			None => String::with_capacity(4),
		}
	}

	fn compile_list<T>(
		&self,
		list: Vec<T>,
		separator: &str,
		tostring: &mut impl FnMut(T) -> String,
	) -> String {
		let mut result = String::new();
		let end = list.len();
		let mut start = 0usize;
		for element in list {
			result += &(tostring(element));
			start += 1;
			if start < end {
				result += separator
			}
		}
		result
	}

	fn compile_identifiers(&self, names: Vec<String>) -> String {
		self.compile_list(names, ", ", &mut |name| name)
	}

	fn compile_expressions(&self, scope: usize, values: Vec<Expression>) -> String {
		self.compile_list(values, ", ", &mut |expr| {
			self.compile_expression(scope, expr)
		})
	}

	fn compile_function(
		&self,
		scope: usize,
		args: FunctionArgs,
		code: CodeBlock,
	) -> (String, String) {
		let mut code = self.compile_code_block(scope, "", code);
		let args = self.compile_list(args, ", ", &mut |(arg, default)| {
			if let Some((default, line)) = default {
				let default = self.compile_expression(scope + 2, default);
				let pre = self.indentate(scope + 1);
				let debug = self.compile_debug_line(line, scope + 2);
				let line = self.compile_debug_comment(line);
				code = format_clue!(
					"\n",
					pre,
					"if ",
					arg,
					" == nil then\n",
					pre,
					"\t",
					debug,
					arg,
					" = ",
					default,
					line,
					"\n",
					pre,
					"end",
					code
				);
			}
			arg
		});
		(code, args)
	}

	fn compile_code_block(&self, scope: usize, start: &str, block: CodeBlock) -> String {
		let pre = self.indentate(scope);
		let code = self.compile_tokens(scope + 1, block.code);
		let debug = self.compile_debug_line(block.start, scope + 1);
		if self.options.env_debug {
			format!(
				"{}\n{}\t{}--{}->{}\n{}\n{}",
				start, pre, debug, block.start, block.end, code, pre
			)
		} else {
			format_clue!(start, "\n", code, "\n", pre)
		}
	}

	fn compile_debug_comment(&self, line: usize) -> String {
		if self.options.env_debug {
			format!(" --{line}")
		} else {
			String::new()
		}
	}

	fn compile_debug_line(&self, line: usize, scope: usize) -> String {
		if self.options.env_debug {
			format!("_clueline = {line};\n{}", self.indentate(scope))
		} else {
			String::new()
		}
	}

	fn compile_identifier(&self, scope: usize, expr: Expression) -> String {
		let mut result = String::with_capacity(32);
		let mut checked = String::with_capacity(32);
		let mut iter = expr.into_iter().peekable();
		while let Some(t) = iter.next() {
			match t.clone() {
				SYMBOL(lexeme) => {
					let lexeme = lexeme.as_str();
					match lexeme {
						"?." => {
							result += &(checked.clone() + " and ");
							checked += ".";
						}
						"?::" => {
							result += &(checked.clone() + " and ");
							checked += ":";
						}
						"?[" => {
							result += &(checked.clone() + " and ");
							let texpr = iter.next();
							let rexpr = if let Some(EXPR(expr)) = texpr {
								self.compile_expression(scope, expr.clone())
							} else {
								panic!("This message should never appear");
							};
							write!(checked, "[({rexpr})]")
								.expect("something really unexpected happened");
						}
						"]" => {}
						_ => checked += lexeme,
					}
				}
				EXPR(expr) => {
					let expr = self.compile_expression(scope, expr);
					checked.push_str(&format_clue!("(", expr, ")]"));
				}
				CALL(args) => write!(checked, "({})", self.compile_expressions(scope, args))
					.expect("something really unexpected happened"),
				_ => {}
			}
		}
		if result.is_empty() {
			result + &checked
		} else {
			format_clue!("(", result, checked, ")")
		}
	}

	fn compile_expression(&self, mut scope: usize, expr: Expression) -> String {
		let mut result = String::with_capacity(64);
		for t in expr {
			result += &match t {
				MACRO_CALL { expr, args } => {
					let _args = {
						let mut strings: Vec<String> = Vec::new();
						for arg in args {
							strings.push(self.compile_expression(scope, arg))
						}
						strings
					};
					let expr = self.compile_expression(scope, expr);
					format_clue!("(", expr, ")")
				}
				SYMBOL(lexeme) => lexeme,
				TABLE {
					values,
					metas,
					metatable,
				} => {
					scope += 1;
					let mut prevline = 0;
					let pre1 = self.indentate(scope);
					let values = if values.is_empty() {
						String::new()
					} else {
						self.compile_list(values, ", ", &mut |(name, value, line)| {
							let value = self.compile_expression(scope, value);
							let l = if prevline != 0 {
								self.compile_debug_comment(prevline)
							} else {
								String::new()
							};
							prevline = line;
							if let Some(name) = name {
								let name = self.compile_expression(scope, name);
								format_clue!(l, "\n", pre1, name, " = ", value)
							} else {
								format_clue!(l, "\n", pre1, value)
							}
						}) + &self.compile_debug_comment(prevline)
							+ "\n"
					};
					prevline = 0;
					let pre2 = self.indentate(scope - 1);
					if metas.is_empty() {
						scope -= 1;
						if let Some(metatable) = metatable {
							format!("setmetatable({{{values}{pre2}}}, {metatable})")
						} else {
							format!("{{{values}{pre2}}}")
						}
					} else {
						let metas = self.compile_list(metas, ", ", &mut |(name, value, line)| {
							let value = self.compile_expression(scope, value);
							let l = if prevline != 0 {
								self.compile_debug_comment(prevline)
							} else {
								String::new()
							};
							prevline = line;
							format_clue!(l, "\n", pre1, name, " = ", value)
						});
						scope -= 1;
						let line = self.compile_debug_comment(prevline);
						format!(
							"setmetatable({{{}{}}}, {{{}{}\n{}}})",
							values, pre2, metas, line, pre2
						)
					}
				}
				LAMBDA { args, code } => {
					let (code, args) = self.compile_function(scope, args, code);
					format_clue!("function(", args, ")", code, "end")
				}
				IDENT { expr, .. } => self.compile_identifier(scope, expr),
				CALL(args) => format!("({})", self.compile_expressions(scope, args)),
				EXPR(expr) => format!("({})", self.compile_expression(scope, expr)),
				_ => {
					unreachable!("Unexpected ComplexToken found")
				}
			}
		}
		result
	}

	fn compile_elseif_chain(
		&self,
		scope: usize,
		condition: Expression,
		code: CodeBlock,
		next: Option<Box<ComplexToken>>,
	) -> String {
		let condition = self.compile_expression(scope, condition);
		let code = self.compile_code_block(scope, "then", code);
		let next = if let Some(next) = next {
			String::from("else")
				+ &match *next {
					IF_STATEMENT {
						condition,
						code,
						next,
					} => self.compile_elseif_chain(scope, condition, code, next),
					DO_BLOCK(code) => self.compile_code_block(scope, "", code),
					_ => panic!("Unexpected ComplexToken found"),
				}
		} else {
			String::new()
		};
		format_clue!("if ", condition, " ", code, next)
	}

	pub fn compile_tokens(&self, scope: usize, ctokens: Expression) -> String {
		let mut result = self.indentate(scope);
		let ctokens = &mut ctokens.into_iter().peekable();
		while let Some(t) = ctokens.next() {
			result += &match t {
				SYMBOL(lexeme) => lexeme,
				VARIABLE {
					local,
					names,
					values,
					line,
				} => {
					let debug = self.compile_debug_line(line, scope);
					let line = self.compile_debug_comment(line);
					if !local && self.options.env_rawsetglobals {
						let mut result = debug;
						let mut valuesit = values.iter();
						let namesit = &mut names.iter().peekable();
						while let Some(name) = namesit.next() {
							let value = if let Some(value) = valuesit.next() {
								self.compile_expression(scope, value.clone())
							} else {
								String::from("nil")
							};
							let end = {
								let pend = self.indentate_if(namesit, scope);
								if !pend.is_empty() {
									pend
								} else {
									self.indentate_if(ctokens, scope)
								}
							};
							write!(
								result,
								"rawset(_G, \"{}\", {});{}{}",
								name, value, line, end
							)
							.expect("something really unexpected happened");
						}
						result
					} else {
						let end = self.indentate_if(ctokens, scope);
						let pre = if local { "local " } else { "" };
						if values.is_empty() {
							let ident = self.compile_identifiers(names);
							format_clue!(debug, pre, ident, ";", line, end)
						} else {
							let values = self.compile_expressions(scope, values);
							let names = self.compile_identifiers(names);
							format_clue!(debug, pre, names, " = ", values, ";", line, end)
						}
					}
				}
				ALTER {
					kind,
					names,
					values,
					line,
				} => {
					let iter = names.into_iter();
					let mut names: Vec<String> = Vec::new();
					for name in iter {
						names.push(self.compile_expression(scope, name))
					}
					let mut i = 0usize;
					let values = self.compile_list(values, ", ", &mut |expr| {
						let name = if let Some(name) = names.get(i) {
							name.clone()
						} else {
							String::from("nil")
						};
						i += 1;
						(if kind == DEFINE {
							String::new()
						} else {
							name + match kind {
								DEFINE_AND => " and ",
								DEFINE_OR => " or ",
								INCREASE => " + ",
								DECREASE => " - ",
								MULTIPLY => " * ",
								DIVIDE => " / ",
								EXPONENTIATE => " ^ ",
								CONCATENATE => " .. ",
								MODULATE => " % ",
								_ => panic!("Unexpected alter type found"),
							}
						}) + &self.compile_expression(scope, expr)
					});
					let names = self.compile_identifiers(names);
					let debug = self.compile_debug_line(line, scope);
					let line = self.compile_debug_comment(line);
					format_clue!(
						debug,
						names,
						" = ",
						values,
						";",
						line,
						self.indentate_if(ctokens, scope)
					)
				}
				FUNCTION {
					local,
					name,
					args,
					code,
				} => {
					let pre = if local { "local " } else { "" };
					let end = self.indentate_if(ctokens, scope);
					let name = self.compile_expression(scope, name);
					let (code, args) = self.compile_function(scope, args, code);
					format_clue!(pre, "function ", name, "(", args, ")", code, "end", end)
				}
				IF_STATEMENT {
					condition,
					code,
					next,
				} => {
					let code = self.compile_elseif_chain(scope, condition, code, next);
					format_clue!(code, "end", self.indentate_if(ctokens, scope))
				}
				MATCH_BLOCK {
					name,
					value,
					branches,
					line,
				} => {
					let value = self.compile_expression(scope, value);
					let debug = self.compile_debug_line(line, scope);
					let line = self.compile_debug_comment(line);
					let branches = {
						let mut result = self.indentate(scope);
						let mut branches = branches.into_iter().peekable();
						while let Some((conditions, extraif, code)) = branches.next() {
							let empty = conditions.is_empty();
							let default = empty && extraif.is_none();
							let pre = if default { "else" } else { "if" };
							let condition = {
								let mut condition =
									self.compile_list(conditions, "or ", &mut |expr| {
										let expr = self.compile_expression(scope, expr);
										format_clue!("(", name, " == ", expr, ") ")
									});
								if let Some(extraif) = extraif {
									condition.pop();
									let extraif = self.compile_expression(scope, extraif);
									if empty {
										extraif + " "
									} else {
										format_clue!("(", condition, ") and ", extraif, " ")
									}
								} else {
									condition
								}
							};
							let code = self.compile_code_block(
								scope,
								if default { "" } else { "then" },
								code,
							);
							let end = match branches.peek() {
								Some((conditions, extraif, _))
									if conditions.is_empty() && matches!(extraif, None) =>
								{
									""
								}
								Some(_) => "else",
								_ => "end",
							};
							write!(result, "{pre} {condition}{code}{end}")
								.expect("something really unexpected happened");
						}
						result
					};
					let end = self.indentate_if(ctokens, scope);
					format_clue!(
						debug, "local ", name, " = ", value, ";", line, "\n", branches, end
					)
				}
				WHILE_LOOP { condition, code } => {
					let condition = self.compile_expression(scope, condition);
					let code = self.compile_code_block(scope, "do", code);
					format_clue!(
						"while ",
						condition,
						" ",
						code,
						"end",
						self.indentate_if(ctokens, scope)
					)
				}
				LOOP_UNTIL { condition, code } => {
					let condition = self.compile_expression(scope, condition);
					let code = self.compile_code_block(scope, "", code);
					format_clue!(
						"repeat ",
						code,
						"until ",
						condition,
						self.indentate_if(ctokens, scope)
					)
				}
				FOR_LOOP {
					iterator,
					start,
					end,
					alter,
					code,
				} => {
					let start = self.compile_expression(scope, start);
					let endexpr = self.compile_expression(scope, end);
					let alter = self.compile_expression(scope, alter);
					let code = self.compile_code_block(scope, "do", code);
					let end = self.indentate_if(ctokens, scope);
					format_clue!(
						"for ", iterator, " = ", start, ", ", endexpr, ", ", alter, " ", code,
						"end", end
					)
				}
				FOR_FUNC_LOOP {
					iterators,
					expr,
					code,
				} => {
					let expr = self.compile_expression(scope, expr);
					let iterators = self.compile_identifiers(iterators);
					let code = self.compile_code_block(scope, "do", code);
					format_clue!(
						"for ",
						iterators,
						" in ",
						expr,
						" ",
						code,
						"end",
						self.indentate_if(ctokens, scope)
					)
				}
				TRY_CATCH {
					totry,
					error,
					catch,
				} => {
					let i = self.indentate_if(ctokens, scope);
					let totry = self.compile_code_block(scope, "function()", totry);
					if let Some(catch) = catch {
						let catch = self.compile_code_block(scope, "if not _check then", catch);
						let i2 = self.indentate(scope);
						if let Some(error) = error {
							format_clue!(
								"local _check, ",
								error,
								" = pcall(",
								totry,
								"end)\n",
								i2,
								catch,
								"end",
								i
							)
						} else {
							format_clue!(
								"local _check = pcall(",
								totry,
								"end)\n",
								i2,
								catch,
								"end",
								i
							)
						}
					} else {
						format_clue!("pcall(", totry, "end)", i)
					}
				}
				IDENT { expr, line } => {
					let expr = self.compile_identifier(scope, expr);
					let debug = self.compile_debug_line(line, scope);
					let line = self.compile_debug_comment(line);
					format_clue!(debug, expr, ";", line, self.indentate_if(ctokens, scope))
				}
				EXPR(expr) => {
					format!("({})", self.compile_expression(scope, expr))
				}
				DO_BLOCK(code) => {
					format!(
						"{}end{}",
						self.compile_code_block(scope, "do", code),
						self.indentate_if(ctokens, scope)
					)
				}
				RETURN_EXPR(exprs) => {
					if let Some(exprs) = exprs {
						format!("return {};", self.compile_expressions(scope, exprs))
					} else {
						String::from("return;")
					}
				}
				CONTINUE_LOOP => {
					let end = self.indentate_if(ctokens, scope);
					format!(
						"{};{}",
						if self.options.env_continue == ContinueMode::LUAJIT {
							"goto continue"
						} else {
							"continue"
						},
						end
					)
				}
				BREAK_LOOP => String::from("break;") + &self.indentate_if(ctokens, scope),
				_ => {
					panic!("Unexpected ComplexToken found")
				}
			}
		}
		result
	}
}
