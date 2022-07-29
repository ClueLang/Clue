use crate::{
	parser::{CodeBlock, ComplexToken, ComplexToken::*, Expression, FunctionArgs},
	scanner::TokenType::*,
	ContinueMode, ENV_CONTINUE, ENV_DEBUGCOMMENTS, ENV_RAWSETGLOBALS,
};
use std::iter::{Iterator, Peekable};

fn Indentate(scope: usize) -> String {
	let mut result = String::new();
	for _ in 0..scope {
		result += "\t";
	}
	result
}

fn IndentateIf<T: Iterator>(ctokens: &mut Peekable<T>, scope: usize) -> String {
	match ctokens.peek() {
		Some(_) => format!("\n{}", Indentate(scope)),
		None => String::new(),
	}
}

fn CompileList<T>(list: Vec<T>, separator: &str, tostring: &mut impl FnMut(T) -> String) -> String {
	let mut result = String::new();
	let end = list.iter().count();
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

fn CompileIdentifiers(names: Vec<String>) -> String {
	CompileList(names, ", ", &mut |name| name)
}

fn CompileExpressions(
	scope: usize,
	names: Option<&Vec<String>>,
	values: Vec<Expression>,
) -> String {
	CompileList(values, ", ", &mut |expr| {
		CompileExpression(scope, names, expr)
	})
}

fn CompileFunction(
	scope: usize,
	names: Option<&Vec<String>>,
	args: FunctionArgs,
	code: CodeBlock,
) -> (String, String) {
	let mut code = CompileCodeBlock(scope, "", code);
	let args = CompileList(args, ", ", &mut |(arg, default)| {
		if let Some((default, line)) = default {
			let default = CompileExpression(scope + 2, names, default);
			let pre = Indentate(scope + 1);
			let line = CompileDebugLine(line);
			code = format!(
				"\n{}if {} == nil then\n{}\t{} = {}{}\n{}end{}",
				pre, arg, pre, arg, default, line, pre, code
			)
		}
		arg
	});
	(code, args)
}

fn CompileCodeBlock(scope: usize, start: &str, block: CodeBlock) -> String {
	let code = CompileTokens(scope + 1, block.code);
	let pre = Indentate(scope);
	if arg!(ENV_DEBUGCOMMENTS) {
		format!(
			"{}\n{}\t--{}->{}\n{}\n{}",
			start, pre, block.start, block.end, code, pre
		)
	} else {
		format!("{}\n{}\n{}", start, code, pre)
	}
}

fn CompileDebugLine(line: usize) -> String {
	if arg!(ENV_DEBUGCOMMENTS) {
		format!(" --{}", line)
	} else {
		String::new()
	}
}

fn CompileIdentifier(scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
	let mut result = String::new();
	let mut checked = String::new();
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
							CompileExpression(scope, names, expr.clone())
						} else {
							panic!("This message should never appear");
						};
						checked += &format!("[({})]", rexpr);
					}
					"]" => {}
					_ => checked += lexeme,
				}
			}
			EXPR(expr) => {
				let expr = CompileExpression(scope, names, expr);
				checked += &format!("({})]", expr);
			}
			CALL(args) => checked += &format!("({})", CompileExpressions(scope, names, args)),
			_ => {}
		}
	}
	if result.is_empty() {
		result + &checked
	} else {
		format!("({})", result + &checked)
	}
}

fn CompileExpression(mut scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
	let mut result = String::new();
	for t in expr {
		result += &match t {
			MACRO_CALL { expr, args } => {
				let args = {
					let mut strings: Vec<String> = Vec::new();
					for arg in args {
						strings.push(CompileExpression(scope, names, arg))
					}
					strings
				};
				let expr = CompileExpression(scope, Some(&args), expr);
				format!("({})", expr)
			}
			SYMBOL(lexeme) => {
				let chars: Vec<_> = lexeme.chars().collect();
				if chars[0] == '`' && chars[lexeme.len() - 1] == '`' {
					let text = &lexeme[1..lexeme.len() - 1];
					return format!("[[{}]]", text);
				}
				lexeme
			}

			PSEUDO(num) => match names {
				Some(names) => names
					.get(num - 1)
					.unwrap_or(&String::from("nil"))
					.to_string(),
				None => String::from("nil"),
			},
			TABLE {
				values,
				metas,
				metatable,
			} => {
				scope += 1;
				let mut prevline = 0usize;
				let pre1 = Indentate(scope);
				let values = if values.is_empty() {
					String::new()
				} else {
					CompileList(values, ", ", &mut |(name, value, line)| {
						let value = CompileExpression(scope, names, value);
						let l = if prevline != 0 {
							CompileDebugLine(prevline)
						} else {
							String::new()
						};
						prevline = line;
						if let Some(name) = name {
							let name = CompileExpression(scope, names, name);
							format!("{}\n{}{} = {}", l, pre1, name, value)
						} else {
							format!("{}\n{}{}", l, pre1, value)
						}
					}) + &CompileDebugLine(prevline)
						+ "\n"
				};
				prevline = 0;
				let pre2 = Indentate(scope - 1);
				if metas.is_empty() {
					scope -= 1;
					if let Some(metatable) = metatable {
						format!("setmetatable({{{}{}}}, {})", values, pre2, metatable)
					} else {
						format!("{{{}{}}}", values, pre2)
					}
				} else {
					let metas = CompileList(metas, ", ", &mut |(name, value, line)| {
						let value = CompileExpression(scope, names, value);
						let l = if prevline != 0 {
							CompileDebugLine(prevline)
						} else {
							String::new()
						};
						prevline = line;
						format!("{}\n{}{} = {}", l, pre1, name, value)
					});
					scope -= 1;
					let line = CompileDebugLine(prevline);
					format!(
						"setmetatable({{{}{}}}, {{{}{}\n{}}})",
						values, pre2, metas, line, pre2
					)
				}
			}
			LAMBDA { args, code } => {
				let (code, args) = CompileFunction(scope, names, args, code);
				format!("function({}){}end", args, code)
			}
			IDENT { expr, .. } => CompileIdentifier(scope, names, expr),
			CALL(args) => format!("({})", CompileExpressions(scope, names, args)),
			EXPR(expr) => format!("({})", CompileExpression(scope, names, expr)),
			_ => {
				panic!("Unexpected ComplexToken found")
			}
		}
	}
	result
}

fn CompileElseIfChain(
	scope: usize,
	condition: Expression,
	code: CodeBlock,
	next: Option<Box<ComplexToken>>,
) -> String {
	let condition = CompileExpression(scope, None, condition);
	let code = CompileCodeBlock(scope, "then", code);
	let next = if let Some(next) = next {
		String::from("else")
			+ &match *next {
				IF_STATEMENT {
					condition,
					code,
					next,
				} => CompileElseIfChain(scope, condition, code, next),
				DO_BLOCK(code) => CompileCodeBlock(scope, "", code),
				_ => {
					panic!("Unexpected ComplexToken found")
				}
			}
	} else {
		String::new()
	};
	format!("if {} {}{}", condition, code, next)
}

pub fn CompileTokens(scope: usize, ctokens: Expression) -> String {
	let mut result = Indentate(scope);
	let ctokens = &mut ctokens.into_iter().peekable();
	while let Some(t) = ctokens.next() {
		result += &match t {
			SYMBOL(lexeme) => {
				let chars: Vec<_> = lexeme.chars().collect();
				if chars[0] == '`' && chars[lexeme.len() - 1] == '`' {
					let text = &lexeme[1..lexeme.len() - 1];
					return format!("[[{}]]", text);
				}
				lexeme
			}
			VARIABLE {
				local,
				names,
				values,
				line,
			} => {
				let line = CompileDebugLine(line);
				if !local && arg!(ENV_RAWSETGLOBALS) {
					let mut result = String::new();
					let mut valuesit = values.iter();
					let namesit = &mut names.iter().peekable();
					while let Some(name) = namesit.next() {
						let value = if let Some(value) = valuesit.next() {
							CompileExpression(scope, Some(&names), value.clone())
						} else {
							String::from("nil")
						};
						let end = {
							let pend = IndentateIf(namesit, scope);
							if pend != "" {
								pend
							} else {
								IndentateIf(ctokens, scope)
							}
						};
						result += &format!("rawset(_G, \"{}\", {});{}{}", name, value, line, end);
					}
					result
				} else {
					let end = IndentateIf(ctokens, scope);
					let pre = if local { "local " } else { "" };
					if values.is_empty() {
						format!("{}{};{}{}", pre, CompileIdentifiers(names), line, end)
					} else {
						let values = CompileExpressions(scope, Some(&names), values);
						let names = CompileIdentifiers(names);
						format!("{}{} = {};{}{}", pre, names, values, line, end)
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
					names.push(CompileExpression(scope, None, name))
				}
				let mut i = 0usize;
				let values = CompileList(values, ", ", &mut |expr| {
					let name = if let Some(name) = names.get(i) {
						name.clone()
					} else {
						String::from("nil")
					};
					i += 1;
					(if kind == DEFINE {
						String::new()
					} else {
						name + &match kind {
							DEFINE_AND => " and ",
							DEFINE_OR => " or ",
							INCREASE => " + ",
							DECREASE => " - ",
							MULTIPLY => " * ",
							DIVIDE => " / ",
							EXPONENTIATE => " ^ ",
							CONCATENATE => " .. ",
							MODULATE => " % ",
							_ => {
								panic!("Unexpected alter type found")
							}
						}
					}) + &CompileExpression(scope, Some(&names), expr)
				});
				let names = CompileIdentifiers(names);
				let line = CompileDebugLine(line);
				format!(
					"{} = {};{}{}",
					names,
					values,
					line,
					IndentateIf(ctokens, scope)
				)
			}
			FUNCTION {
				local,
				name,
				args,
				code,
			} => {
				let pre = if local { "local " } else { "" };
				let end = IndentateIf(ctokens, scope);
				let name = CompileExpression(scope, None, name);
				let (code, args) = CompileFunction(scope, None, args, code);
				format!("{}function {}({}){}end{}", pre, name, args, code, end)
			}
			IF_STATEMENT {
				condition,
				code,
				next,
			} => {
				let code = CompileElseIfChain(scope, condition, code, next);
				format!("{}end{}", code, IndentateIf(ctokens, scope))
			}
			MATCH_BLOCK {
				name,
				value,
				branches,
				line,
			} => {
				let value = CompileExpression(scope, None, value);
				let line = CompileDebugLine(line);
				let branches = {
					let mut result = Indentate(scope);
					let mut branches = branches.into_iter().peekable();
					while let Some((conditions, extraif, code)) = branches.next() {
						let empty = conditions.is_empty();
						let default = empty && extraif == None;
						let pre = if default { "else" } else { "if" };
						let condition = {
							let mut condition = CompileList(conditions, "or ", &mut |expr| {
								let expr = CompileExpression(scope, None, expr);
								format!("({} == {}) ", name, expr)
							});
							if let Some(extraif) = extraif {
								condition.pop();
								let extraif = CompileExpression(scope, None, extraif);
								if empty {
									extraif + " "
								} else {
									format!("({}) and {} ", condition, extraif)
								}
							} else {
								condition
							}
						};
						let code = CompileCodeBlock(scope, if default { "" } else { "then" }, code);
						let end = match branches.peek() {
							Some((conditions, extraif, _))
								if conditions.is_empty() && matches!(extraif, None) =>
							{
								""
							}
							Some(_) => "else",
							_ => "end",
						};
						result += &format!("{} {}{}{}", pre, condition, code, end)
					}
					result
				};
				let end = IndentateIf(ctokens, scope);
				format!("local {} = {};{}\n{}{}", name, value, line, branches, end)
			}
			WHILE_LOOP { condition, code } => {
				let condition = CompileExpression(scope, None, condition);
				let code = CompileCodeBlock(scope, "do", code);
				format!(
					"while {} {}end{}",
					condition,
					code,
					IndentateIf(ctokens, scope)
				)
			}
			LOOP_UNTIL { condition, code } => {
				let condition = CompileExpression(scope, None, condition);
				let code = CompileCodeBlock(scope, "", code);
				format!(
					"repeat {}until {}{}",
					code,
					condition,
					IndentateIf(ctokens, scope)
				)
			}
			FOR_LOOP {
				iterator,
				start,
				end,
				alter,
				code,
			} => {
				let start = CompileExpression(scope, None, start);
				let endexpr = CompileExpression(scope, None, end);
				let alter = CompileExpression(scope, None, alter);
				let code = CompileCodeBlock(scope, "do", code);
				let end = IndentateIf(ctokens, scope);
				format!(
					"for {} = {}, {}, {} {}end{}",
					iterator, start, endexpr, alter, code, end
				)
			}
			FOR_FUNC_LOOP {
				iterators,
				expr,
				code,
			} => {
				let expr = CompileExpression(scope, Some(&iterators), expr);
				let iterators = CompileIdentifiers(iterators);
				let code = CompileCodeBlock(scope, "do", code);
				format!(
					"for {} in {} {}end{}",
					iterators,
					expr,
					code,
					IndentateIf(ctokens, scope)
				)
			}
			TRY_CATCH {
				totry,
				error,
				catch,
			} => {
				let i = IndentateIf(ctokens, scope);
				let totry = CompileCodeBlock(scope, "function()", totry);
				if let Some(catch) = catch {
					let catch = CompileCodeBlock(scope, "if not _check then", catch);
					let i2 = Indentate(scope);
					if let Some(error) = error {
						format!(
							"local _check, {} = pcall({}end)\n{}{}end{}",
							error, totry, i2, catch, i
						)
					} else {
						format!(
							"local _check = pcall({}end)\n{}{}end{}",
							totry, i2, catch, i
						)
					}
				} else {
					format!("pcall({}end){}", totry, i)
				}
			}
			IDENT { expr, line } => {
				let expr = CompileIdentifier(scope, None, expr);
				let line = CompileDebugLine(line);
				format!("{};{}{}", expr, line, IndentateIf(ctokens, scope))
			}
			/*CALL(args) => {
				format!("({}){}", CompileExpressions(scope, None, args), IndentateIf(ctokens, scope))
			}*/
			EXPR(expr) => {
				format!("({})", CompileExpression(scope, None, expr))
			}
			DO_BLOCK(code) => {
				format!(
					"{}end{}",
					CompileCodeBlock(scope, "do", code),
					IndentateIf(ctokens, scope)
				)
			}
			RETURN_EXPR(exprs) => {
				if let Some(exprs) = exprs {
					format!("return {};", CompileExpressions(scope, None, exprs))
				} else {
					String::from("return;")
				}
			}
			CONTINUE_LOOP => {
				let end = IndentateIf(ctokens, scope);
				format!(
					"{};{}",
					if arg!(ENV_CONTINUE) == ContinueMode::LUAJIT {
						"goto continue"
					} else {
						"continue"
					},
					end
				)
			}
			BREAK_LOOP => String::from("break;") + &IndentateIf(ctokens, scope),
			_ => {
				panic!("Unexpected ComplexToken found")
			}
		}
	}
	result
}
