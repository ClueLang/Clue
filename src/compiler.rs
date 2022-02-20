#![allow(non_upper_case_globals)]

use crate::scanner::TokenType::*;
use crate::parser::ComplexToken;
use crate::parser::ComplexToken::*;
use crate::parser::Expression;

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

fn CompileExpression(cline: &mut usize, names: &Vec<String>, expr: Expression) -> String {
	let mut result = String::new();
	for t in expr {
		match t {
			SYMBOL {lexeme, line} => {
				*cline += lexeme.matches("\n").count();
				result += &format!("{}{} ", ReachLine(cline, line), lexeme);
			}
			PSEUDO {num, line} => {
				result += &format!("{}{} ", ReachLine(cline, line), names.get(num - 1).unwrap_or(&String::from("nil")));
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	result
}

pub fn CompileTokens(ctokens: Vec<ComplexToken>) -> Result<String, String> {
	let mut result = String::new();
	let cline = &mut 1usize;
	for t in ctokens.into_iter() {
		match t {
			VARIABLE {local, names, values, line} => {
				let mut pre = ReachLine(cline, line);
				if local {pre += "local "}
				let values = CompileExpressions(cline, &names, values);
				let names = CompileIdentifiers(names);
				result += &format!("{}{} = {};", pre, names, values);
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
						DEFINEIF => format!("{}and ", name),
						INCREASE => format!("{}+ ", name),
						DECREASE => format!("{}- ", name),
						MULTIPLY => format!("{}* ", name),
						DIVIDE => format!("{}/ ", name),
						EXPONENTIATE => format!("{}^ ", name),
						CONCATENATE => format!("{}.. ", name),
						_ => {panic!("Unexpected alter type found")}
					}) + &CompileExpression(cline, &names, expr)
				});
				let names = CompileIdentifiers(names);
				result += &format!("{}{}= {};", pre, names, values);
			}
			_ => {panic!("Unexpected ComplexToken found")}
		}
	}
	Ok(result)
}