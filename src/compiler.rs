use crate::{
	scanner::TokenType::*,
	parser::{
		ComplexToken,
		ComplexToken::*,
		FunctionArgs,
		CodeBlock,
		Expression
	},
	ENV_CONTINUE,
	ENV_RAWSETGLOBALS
};
use std::{
	iter::Peekable,
	collections::linked_list::IntoIter
};

fn Indentate(scope: usize) -> String {
	let mut result = String::new();
	for _ in 0..scope {
		result += "\t";
	}
	result
}

fn IndentateIf(ctokens: &mut Peekable<IntoIter<ComplexToken>>, scope: usize) -> String {
	match ctokens.peek() {
		Some(_) => format!("\n{}", Indentate(scope)),
		None => String::new()
	}
}

fn CompileList<T>(list: Vec<T>, tostring: &mut impl FnMut(T) -> String) -> String {
	let mut result = String::new();
	let end = list.iter().count();
	let mut start = 0usize;
	for element in list {
		result += &(tostring(element));
		start += 1;
		if start < end {
			result += ", "
		}
	}
	result
}

fn CompileIdentifiers(names: Vec<String>) -> String {
	CompileList(names, &mut |name| {name})
}

fn CompileExpressions(scope: usize, names: Option<&Vec<String>>, values: Vec<Expression>) -> String {
	CompileList(values, &mut |expr| {CompileExpression(scope, names, expr)})
}

fn CompileFunction(scope: usize, names: Option<&Vec<String>>, args: FunctionArgs, code: CodeBlock) -> (String, String) {
	let mut code = CompileCodeBlock(scope, "", code);
	let args = CompileList(args, &mut |(arg, default)| {
		if let Some(default) = default {
			let default = CompileExpression(scope, names, default);
			let pre = Indentate(scope + 1);
			code = format!("\n{}if {} == nil then {} = {} end{}", pre, arg, arg, default, code)
		}
		arg
	});
	(code, args)
}

fn CompileCodeBlock(scope: usize, start: &str, block: CodeBlock) -> String {
	let code = CompileTokens(scope + 1, block.code);
	format!("{}\n{}\n{}", start, code, Indentate(scope))
}

fn CompileIdentifier(scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
	let mut result = String::new();
	let mut checked = String::new();
	let mut iter = expr.iter().peekable();
	for t in iter.clone() {
		iter.next();
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
					"[" => {
						result += &(checked.clone() + " and ");
						let texpr = *(iter.peek().unwrap());
						let rexpr: String;
						if let EXPR(expr) = texpr {
							rexpr = CompileExpression(scope, names, expr.clone());
						} else {
							panic!("This message should never appear");
						}
							checked += &format!("[({})]", rexpr);
						}
					"]" => {}
					_ => {checked += lexeme}
				}
			}
			CALL(args) => {
				checked += &format!("({})", CompileExpressions(scope, names, args))
			}
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
			SYMBOL (lexeme) => lexeme,
			PSEUDO {num, line: _} => {
				match names {
					Some(names) => names.get(num - 1).unwrap_or(&String::from("nil")).to_string(),
					None => String::from("nil")
				}
			}
			TABLE {values, metas} => {
				scope += 1;
				let pre1 = Indentate(scope);
				let values = if values.is_empty() {
					String::new()
				} else {
					CompileList(values, &mut |(name, value)| {
					let name = CompileExpression(scope, names, name);
					let value = CompileExpression(scope, names, value);
					if name.is_empty() {
						format!("\n{}{}", pre1, value)
					} else {
						format!("\n{}{} = {}", pre1, name, value)
					}
				}) + "\n"};
				let pre2 = Indentate(scope - 1);
				if metas.is_empty() {
					scope -= 1;
					format!("{{{}{}}}", values, pre2)
				} else {
					let metas = CompileList(metas, &mut |(name, value)| {
						let value = CompileExpression(scope, names, value);
						format!("\n{}{} = {}", pre1, name, value)
					});
					scope -= 1;
					format!("setmetatable({{{}{}}}, {{{}\n{}}})", values, pre2, metas, pre2)
				}
			}
			LAMBDA {args, code, line: _} => {
				let (code, args) = CompileFunction(scope, names, args, code);
				format!("function({}){}end", args, code)
			}
			CALL(args) => {
				format!("({})", CompileExpressions(scope, names, args))
			}
			EXPR(expr) => {
				format!("({})", CompileExpression(scope, names, expr))
			}
			IDENT(expr) => CompileIdentifier(scope, names, expr),
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}

fn CompileElseIfChain(scope: usize, condition: Expression, code: CodeBlock, next: Option<Box<ComplexToken>>) -> String {
	let condition = CompileExpression(scope, None, condition);
	let code = CompileCodeBlock(scope, "then", code);
	let next = if let Some(next) = next {
		String::from("else") + &match *next {
			IF_STATEMENT {condition, code, next} => CompileElseIfChain(scope, condition, code, next),
			DO_BLOCK(code) => CompileCodeBlock(scope, "", code),
			_ => {panic!("Unexpected ComplexToken found")}
		}
	} else {String::new()};
	format!("if {} {}{}", condition, code, next)
}

pub fn CompileTokens(scope: usize, ctokens: Expression) -> String {
	let mut result = Indentate(scope);
	let ctokens = &mut ctokens.into_iter().peekable();
	for t in ctokens.clone() {
		ctokens.next();
		result += &match t {
			SYMBOL (lexeme) => lexeme,
			VARIABLE {local, names, values, line: _} => {
				if !local && arg!(ENV_RAWSETGLOBALS) {
					String::new()
				} else {
					let pre = if local {"local "} else {""};
					let end = IndentateIf(ctokens, scope);
					if values.is_empty() {
						format!("{}{};{}", pre, CompileIdentifiers(names), end)
					} else {
						let values = CompileExpressions(scope, Some(&names), values);
						let names = CompileIdentifiers(names);
						format!("{}{} = {};{}", pre, names, values, end)
					}
				}
			}
			ALTER {kind, names, values, line: _} => {
				let iter = names.iter();
				let mut names: Vec<String> = Vec::new();
				for name in iter {
					names.push(CompileExpression(scope, None, name.clone()))
				}
				let mut i = 0usize;
				let values = CompileList(values, &mut |expr| {
					let name = names.get(i).unwrap();
					i += 1;
					(match kind {
						DEFINE => String::new(),
						DEFINEIF => format!("{} and ", name),
						INCREASE => format!("{} + ", name),
						DECREASE => format!("{} - ", name),
						MULTIPLY => format!("{} * ", name),
						DIVIDE => format!("{} / ", name),
						EXPONENTIATE => format!("{} ^ ", name),
						CONCATENATE => format!("{} .. ", name),
						_ => {panic!("Unexpected alter type found")}
					}) + &CompileExpression(scope, Some(&names), expr)
				});
				let names = CompileIdentifiers(names);
				format!("{} = {};{}", names, values, IndentateIf(ctokens, scope))
			}
			FUNCTION {local, name, args, code, line: _} => {
				let pre = if local {"local "} else {""};
				let end = IndentateIf(ctokens, scope);
				let name = CompileExpression(scope, None, name);
				let (code, args) = CompileFunction(scope, None, args, code);
				format!("{}function {}({}){}end{}", pre, name, args, code, end)
			}
			IF_STATEMENT {condition, code, next} => {
				let code = CompileElseIfChain(scope, condition, code, next);
				format!("{}end{}", code, IndentateIf(ctokens, scope))
			}
			WHILE_LOOP {condition, code} => {
				let condition = CompileExpression(scope, None, condition);
				let code = CompileCodeBlock(scope, "do", code);
				format!("while {} {}end{}", condition, code, IndentateIf(ctokens, scope))
			}
			LOOP_UNTIL {condition, code} => {
				let condition = CompileExpression(scope, None, condition);
				let code = CompileCodeBlock(scope, "", code);
				format!("repeat {}until {}{}", code, condition, IndentateIf(ctokens, scope))
			}
			FOR_LOOP {iterator, start, end, alter, code} => {
				let start = CompileExpression(scope, None, start);
				let endexpr = CompileExpression(scope, None, end);
				let alter = CompileExpression(scope, None, alter);
				let code = CompileCodeBlock(scope, "do", code);
				let end = IndentateIf(ctokens, scope);
				format!("for {} = {}, {}, {} {}end{}", iterator, start, endexpr, alter, code, end)
			}
			FOR_FUNC_LOOP {iterators, expr, code} => {
				let expr = CompileExpression(scope, Some(&iterators), expr);
				let iterators = CompileIdentifiers(iterators);
				let code = CompileCodeBlock(scope, "do", code);
				format!("for {} in {} {}end{}", iterators, expr, code, IndentateIf(ctokens, scope))
			}
			CALL(args) => {
				format!("({}){}", CompileExpressions(scope, None, args), IndentateIf(ctokens, scope))
			}
			EXPR(expr) => CompileExpression(scope, None, expr),
			IDENT(expr) => CompileIdentifier(scope, None, expr),
			DO_BLOCK(code) => {
				format!("{}end{}", CompileCodeBlock(scope, "do", code), IndentateIf(ctokens, scope))
			}
			RETURN_EXPR(expr) => format!("return {};", CompileExpression(scope, None, expr)),
			CONTINUE_LOOP => {
				let end = IndentateIf(ctokens, scope);
				format!("{};{}", if arg!(ENV_CONTINUE) {"continue"} else {"goto continue"}, end)
			}
			BREAK_LOOP => {
				String::from("break;") + &IndentateIf(ctokens, scope)
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}