use std::fmt::Write;
use std::iter::{Iterator, Peekable};

use crate::{
	env::ContinueMode,
	flag,
	parser::{CodeBlock, ComplexToken, ComplexToken::*, Expression, FunctionArgs},
	scanner::TokenType::*,
	ENV_DATA,
};

fn indentate(scope: usize) -> String {
	let mut result = String::with_capacity(512);
	for _ in 0..scope {
		result += "\t";
	}
	result
}

fn indentate_if<T: Iterator>(ctokens: &mut Peekable<T>, scope: usize) -> String {
	match ctokens.peek() {
		Some(_) => format!("\n{}", indentate(scope)),
		None => String::new(),
	}
}

fn compile_list<T>(
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

fn compile_identifiers(names: Vec<String>) -> String {
	compile_list(names, ", ", &mut |name| name)
}

fn compile_expressions(
	scope: usize,
	names: Option<&Vec<String>>,
	values: Vec<Expression>,
) -> String {
	compile_list(values, ", ", &mut |expr| {
		compile_expression(scope, names, expr)
	})
}

fn compile_function(
	scope: usize,
	names: Option<&Vec<String>>,
	args: FunctionArgs,
	code: CodeBlock,
) -> (String, String) {
	let mut code = compile_code_block(scope, "", code);
	let args = compile_list(args, ", ", &mut |(arg, default)| {
		if let Some((default, line)) = default {
			let default = compile_expression(scope + 2, names, default);
			let pre = indentate(scope + 1);
			let debug = compile_debug_line(line, scope + 2);
			let line = compile_debug_comment(line);
			code = format!(
				"\n{}if {} == nil then\n{}\t{}{} = {}{}\n{}end{}",
				pre, arg, pre, debug, arg, default, line, pre, code
			)
		}
		arg
	});
	(code, args)
}

fn compile_code_block(scope: usize, start: &str, block: CodeBlock) -> String {
	let pre = indentate(scope);
	let code = compile_tokens(scope + 1, block.code);
	let debug = compile_debug_line(block.start, scope + 1);
	if flag!(env_debug) {
		format!(
			"{}\n{}\t{}--{}->{}\n{}\n{}",
			start, pre, debug, block.start, block.end, code, pre
		)
	} else {
		format!("{start}\n{code}\n{pre}")
	}
}

fn compile_debug_comment(line: usize) -> String {
	if flag!(env_debug) {
		format!(" --{line}")
	} else {
		String::new()
	}
}

fn compile_debug_line(line: usize, scope: usize) -> String {
	if flag!(env_debug) {
		format!("_clueline = {line};\n{}", indentate(scope))
	} else {
		String::new()
	}
}

fn compile_identifier(scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
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
							compile_expression(scope, names, expr.clone())
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
				let expr = compile_expression(scope, names, expr);
				write!(checked, "({expr})]").expect("something really unexpected happened");
			}
			CALL(args) => write!(checked, "({})", compile_expressions(scope, names, args))
				.expect("something really unexpected happened"),
			_ => {}
		}
	}
	if result.is_empty() {
		result + &checked
	} else {
		format!("({})", result + &checked)
	}
}

fn compile_expression(mut scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
	let mut result = String::new();
	for t in expr {
		result += &match t {
			MACRO_CALL { expr, args } => {
				let args = {
					let mut strings: Vec<String> = Vec::new();
					for arg in args {
						strings.push(compile_expression(scope, names, arg))
					}
					strings
				};
				let expr = compile_expression(scope, Some(&args), expr);
				format!("({expr})")
			}
			SYMBOL(lexeme) => lexeme,
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
				let pre1 = indentate(scope);
				let values = if values.is_empty() {
					String::new()
				} else {
					compile_list(values, ", ", &mut |(name, value, line)| {
						let value = compile_expression(scope, names, value);
						let l = if prevline != 0 {
							compile_debug_comment(prevline)
						} else {
							String::new()
						};
						prevline = line;
						if let Some(name) = name {
							let name = compile_expression(scope, names, name);
							format!("{l}\n{pre1}{name} = {value}")
						} else {
							format!("{l}\n{pre1}{value}")
						}
					}) + &compile_debug_comment(prevline)
						+ "\n"
				};
				prevline = 0;
				let pre2 = indentate(scope - 1);
				if metas.is_empty() {
					scope -= 1;
					if let Some(metatable) = metatable {
						format!("setmetatable({{{values}{pre2}}}, {metatable})")
					} else {
						format!("{{{values}{pre2}}}")
					}
				} else {
					let metas = compile_list(metas, ", ", &mut |(name, value, line)| {
						let value = compile_expression(scope, names, value);
						let l = if prevline != 0 {
							compile_debug_comment(prevline)
						} else {
							String::new()
						};
						prevline = line;
						format!("{l}\n{pre1}{name} = {value}")
					});
					scope -= 1;
					let line = compile_debug_comment(prevline);
					format!(
						"setmetatable({{{}{}}}, {{{}{}\n{}}})",
						values, pre2, metas, line, pre2
					)
				}
			}
			LAMBDA { args, code } => {
				let (code, args) = compile_function(scope, names, args, code);
				format!("function({args}){code}end")
			}
			IDENT { expr, .. } => compile_identifier(scope, names, expr),
			CALL(args) => format!("({})", compile_expressions(scope, names, args)),
			EXPR(expr) => format!("({})", compile_expression(scope, names, expr)),
			_ => {
				panic!("Unexpected ComplexToken found")
			}
		}
	}
	result
}

fn compile_elseif_chain(
	scope: usize,
	condition: Expression,
	code: CodeBlock,
	next: Option<Box<ComplexToken>>,
) -> String {
	let condition = compile_expression(scope, None, condition);
	let code = compile_code_block(scope, "then", code);
	let next = if let Some(next) = next {
		String::from("else")
			+ &match *next {
				IF_STATEMENT {
					condition,
					code,
					next,
				} => compile_elseif_chain(scope, condition, code, next),
				DO_BLOCK(code) => compile_code_block(scope, "", code),
				_ => panic!("Unexpected ComplexToken found"),
			}
	} else {
		String::new()
	};
	format!("if {condition} {code}{next}")
}

pub fn compile_tokens(scope: usize, ctokens: Expression) -> String {
	let mut result = indentate(scope);
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
				let debug = compile_debug_line(line, scope);
				let line = compile_debug_comment(line);
				if !local && flag!(env_rawsetglobals) {
					let mut result = debug;
					let mut valuesit = values.iter();
					let namesit = &mut names.iter().peekable();
					while let Some(name) = namesit.next() {
						let value = if let Some(value) = valuesit.next() {
							compile_expression(scope, Some(&names), value.clone())
						} else {
							String::from("nil")
						};
						let end = {
							let pend = indentate_if(namesit, scope);
							if !pend.is_empty() {
								pend
							} else {
								indentate_if(ctokens, scope)
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
					let end = indentate_if(ctokens, scope);
					let pre = if local { "local " } else { "" };
					if values.is_empty() {
						format!("{debug}{pre}{};{line}{end}", compile_identifiers(names))
					} else {
						let values = compile_expressions(scope, Some(&names), values);
						let names = compile_identifiers(names);
						format!("{debug}{pre}{names} = {values};{line}{end}")
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
					names.push(compile_expression(scope, None, name))
				}
				let mut i = 0usize;
				let values = compile_list(values, ", ", &mut |expr| {
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
					}) + &compile_expression(scope, Some(&names), expr)
				});
				let names = compile_identifiers(names);
				let debug = compile_debug_line(line, scope);
				let line = compile_debug_comment(line);
				format!(
					"{}{} = {};{}{}",
					debug,
					names,
					values,
					line,
					indentate_if(ctokens, scope)
				)
			}
			FUNCTION {
				local,
				name,
				args,
				code,
			} => {
				let pre = if local { "local " } else { "" };
				let end = indentate_if(ctokens, scope);
				let name = compile_expression(scope, None, name);
				let (code, args) = compile_function(scope, None, args, code);
				format!("{pre}function {name}({args}){code}end{end}")
			}
			IF_STATEMENT {
				condition,
				code,
				next,
			} => {
				let code = compile_elseif_chain(scope, condition, code, next);
				format!("{code}end{}", indentate_if(ctokens, scope))
			}
			MATCH_BLOCK {
				name,
				value,
				branches,
				line,
			} => {
				let value = compile_expression(scope, None, value);
				let debug = compile_debug_line(line, scope);
				let line = compile_debug_comment(line);
				let branches = {
					let mut result = indentate(scope);
					let mut branches = branches.into_iter().peekable();
					while let Some((conditions, extraif, code)) = branches.next() {
						let empty = conditions.is_empty();
						let default = empty && extraif.is_none();
						let pre = if default { "else" } else { "if" };
						let condition = {
							let mut condition = compile_list(conditions, "or ", &mut |expr| {
								let expr = compile_expression(scope, None, expr);
								format!("({name} == {expr}) ")
							});
							if let Some(extraif) = extraif {
								condition.pop();
								let extraif = compile_expression(scope, None, extraif);
								if empty {
									extraif + " "
								} else {
									format!("({condition}) and {extraif} ")
								}
							} else {
								condition
							}
						};
						let code =
							compile_code_block(scope, if default { "" } else { "then" }, code);
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
				let end = indentate_if(ctokens, scope);
				format!("{debug}local {name} = {value};{line}\n{branches}{end}")
			}
			WHILE_LOOP { condition, code } => {
				let condition = compile_expression(scope, None, condition);
				let code = compile_code_block(scope, "do", code);
				format!(
					"while {} {}end{}",
					condition,
					code,
					indentate_if(ctokens, scope)
				)
			}
			LOOP_UNTIL { condition, code } => {
				let condition = compile_expression(scope, None, condition);
				let code = compile_code_block(scope, "", code);
				format!(
					"repeat {}until {}{}",
					code,
					condition,
					indentate_if(ctokens, scope)
				)
			}
			FOR_LOOP {
				iterator,
				start,
				end,
				alter,
				code,
			} => {
				let start = compile_expression(scope, None, start);
				let endexpr = compile_expression(scope, None, end);
				let alter = compile_expression(scope, None, alter);
				let code = compile_code_block(scope, "do", code);
				let end = indentate_if(ctokens, scope);
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
				let expr = compile_expression(scope, Some(&iterators), expr);
				let iterators = compile_identifiers(iterators);
				let code = compile_code_block(scope, "do", code);
				format!(
					"for {} in {} {}end{}",
					iterators,
					expr,
					code,
					indentate_if(ctokens, scope)
				)
			}
			TRY_CATCH {
				totry,
				error,
				catch,
			} => {
				let i = indentate_if(ctokens, scope);
				let totry = compile_code_block(scope, "function()", totry);
				if let Some(catch) = catch {
					let catch = compile_code_block(scope, "if not _check then", catch);
					let i2 = indentate(scope);
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
					format!("pcall({totry}end){i}")
				}
			}
			IDENT { expr, line } => {
				let expr = compile_identifier(scope, None, expr);
				let debug = compile_debug_line(line, scope);
				let line = compile_debug_comment(line);
				format!("{debug}{expr};{line}{}", indentate_if(ctokens, scope))
			}
			/*CALL(args) => {
				format!("({}){}", compile_expressions(scope, None, args), indentate_if(ctokens, scope))
			}*/
			EXPR(expr) => {
				format!("({})", compile_expression(scope, None, expr))
			}
			DO_BLOCK(code) => {
				format!(
					"{}end{}",
					compile_code_block(scope, "do", code),
					indentate_if(ctokens, scope)
				)
			}
			RETURN_EXPR(exprs) => {
				if let Some(exprs) = exprs {
					format!("return {};", compile_expressions(scope, None, exprs))
				} else {
					String::from("return;")
				}
			}
			CONTINUE_LOOP => {
				let end = indentate_if(ctokens, scope);
				format!(
					"{};{}",
					if flag!(env_continue) == ContinueMode::LUAJIT {
						"goto continue"
					} else {
						"continue"
					},
					end
				)
			}
			BREAK_LOOP => String::from("break;") + &indentate_if(ctokens, scope),
			_ => {
				panic!("Unexpected ComplexToken found")
			}
		}
	}
	result
}
