#![allow(non_upper_case_globals)]

use crate::scanner::TokenType::*;
use std::iter::Peekable;
use std::vec::IntoIter;
use crate::parser::{
	ComplexToken,
	ComplexToken::*,
	Expression,
	FunctionArgs,
	CodeBlock
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
	format!("{}\n{}\n{}end", start, code, Indentate(scope))
}

fn CompileExpression(mut scope: usize, names: Option<&Vec<String>>, expr: Expression) -> String {
	let mut result = String::new();
	for t in expr {
		result += &match t {
			SYMBOL {lexeme, line: _} => lexeme,
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
				format!("function({}){}", args, code)
			}
			CALL(args) => {
				format!("({})", CompileExpressions(scope, names, args))
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}

pub fn CompileTokens(scope: usize, ctokens: Vec<ComplexToken>) -> String {
	let mut result = Indentate(scope);
	let ctokens = &mut ctokens.into_iter().peekable();
	for t in ctokens.clone() {
		ctokens.next();
		result += &match t {
			SYMBOL {lexeme, line: _} => lexeme,
			VARIABLE {local, names, values, line: _} => {
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
			ALTER {kind, names, values, line: _} => {
				let iter = names.iter();
				let mut names: Vec<String> = Vec::new();
				for name in iter {
					names.push(CompileExpression(scope, None, name.to_vec()))
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
				let mut pre1 = Indentate(scope);
				let pre2 = IndentateIf(ctokens, scope);
				if local {pre1 += "local "}
				let name = CompileExpression(scope, None, name);
				let (code, args) = CompileFunction(scope, None, args, code);
				format!("{}function {}({}){}{}", pre1, name, args, code, pre2)
			}
			IF_STATEMENT {condition, code, line: _} => {
				let condition = CompileExpression(scope, None, condition);
				let code = CompileCodeBlock(scope, "then", code);
				format!("if {} {}{}", condition, code, IndentateIf(ctokens, scope))
			}
			WHILE_LOOP {condition, code} => {
				let condition = CompileExpression(scope, None, condition);
				let code = CompileCodeBlock(scope, "do", code);
				format!("while {} {}{}", condition, code, IndentateIf(ctokens, scope))
			}
			CALL(args) => {
				format!("({}){}", CompileExpressions(scope, None, args), IndentateIf(ctokens, scope))
			}
			CONTINUE_LOOP => {
				String::from("goto continue;") + &IndentateIf(ctokens, scope)
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}