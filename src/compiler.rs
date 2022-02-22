#![allow(non_upper_case_globals)]

use crate::scanner::TokenType::*;
use crate::parser::ComplexToken;
use crate::parser::ComplexToken::*;
use crate::parser::Expression;
use crate::parser::FunctionArgs;

const noPseudos: &Vec<String> = &Vec::new();

fn ReachLine(cline: &mut usize, line: usize) -> String {
	let mut result = String::new();
	for _ in *cline..line {
		result += "\n";
		*cline += 1;
	}
	result
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

fn CompileExpressions(cline: &mut usize, names: &Vec<String>, values: Vec<Expression>) -> String {
	CompileList(values, &mut |expr| {CompileExpression(cline, names, expr)})
}

fn CompileFunction(cline: &mut usize, names: &Vec<String>, args: FunctionArgs, code: Expression) -> (String, String) {
	let mut code = CompileTokens(cline, code);
	let args = CompileList(args, &mut |(arg, default)| {
		if let Some(default) = default {
		let default = CompileExpression(cline, names, default);
		code = format!("if {} == nil then {} = {} end {}", arg, arg, default, code)
		}
		arg
	});
	(code, args)
}

fn CompileExpression(cline: &mut usize, names: &Vec<String>, expr: Expression) -> String {
	let mut result = String::new();
	for t in expr {
		result += &match t {
			SYMBOL {lexeme, line} => {
				*cline += lexeme.matches("\n").count();
				format!("{}{}", ReachLine(cline, line), lexeme)
			}
			PSEUDO {num, line} => {
				format!("{}{}", ReachLine(cline, line), names.get(num - 1).unwrap_or(&String::from("nil")))
			}
			TABLE {values, metas} => {
				let values = CompileList(values, &mut |(name, value)| {
					let name = CompileExpression(cline, names, name);
					let value = CompileExpression(cline, names, value);
					if name.is_empty() {
						format!("{}", value)
					} else {
						format!("{} = {}", name, value)
					}
				});
				if metas.is_empty() {
					format!("{{{}}}", values)
				} else {
					let metas = CompileList(metas, &mut |(name, value)| {
						let value = CompileExpression(cline, names, value);
						format!("{} = {}", name, value)
					});
					format!("setmetatable({{{}}}, {{{}}})", values, metas)
				}
			}
			LAMBDA {args, code} => {
				let (code, args) = CompileFunction(cline, names, args, code);
				format!("function({}){} end", args, code)
			}
			CALL(args) => {
				format!("({})", CompileExpressions(cline, names, args))
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}

pub fn CompileTokens(cline: &mut usize, ctokens: Vec<ComplexToken>) -> String {
	let mut result = String::new();
	for t in ctokens.into_iter() {
		result += &match t {
			SYMBOL {lexeme, line} => {
				*cline += lexeme.matches("\n").count();
				format!("{}{}", ReachLine(cline, line), lexeme)
			}
			VARIABLE {local, names, values, line} => {
				let mut pre = ReachLine(cline, line);
				if local {pre += "local "}
				if values.is_empty() {
					format!("{}{};", pre, CompileIdentifiers(names))
				} else {
					let values = CompileExpressions(cline, &names, values);
					let names = CompileIdentifiers(names);
					format!("{}{} = {};", pre, names, values)
				}
			}
			ALTER {kind, names, values, line} => {
				let pre = ReachLine(cline, line);
				let iter = names.iter();
				let mut names: Vec<String> = Vec::new();
				for name in iter {
					names.push(CompileExpression(cline, noPseudos, name.to_vec()))
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
					}) + &CompileExpression(cline, &names, expr)
				});
				let names = CompileIdentifiers(names);
				format!("{}{} = {};", pre, names, values)
			}
			FUNCTION {local, name, args, code, line} => {
				let mut pre = ReachLine(cline, line);
				if local {pre += "local "}
				let name = CompileExpression(cline, noPseudos, name);
				let (code, args) = CompileFunction(cline, noPseudos, args, code);
				format!("{}function {}({}){} end", pre, name, args, code)
			}
			CALL(args) => {
				format!("({})", CompileExpressions(cline, noPseudos, args))
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}