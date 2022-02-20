use crate::parser::ComplexToken;
use crate::parser::ComplexToken::*;
use crate::parser::Expression;

fn ReachLine(cline: &mut u32, line: u32) -> String {
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

fn CompileExpressions(line: &mut u32, values: Vec<Expression>) -> String {
    CompileList(values, &mut |expr| {CompileExpression(line, expr)})
}

fn CompileExpression(cline: &mut u32, expr: Expression) -> String {
    let mut result = String::new();
    for t in expr {
        match t {
            SYMBOL {lexeme, line} => {
                result += &format!("{}{}", ReachLine(cline, line), lexeme);
            }
            _ => {panic!("Unexpected ComplexToken found")}
        }
    }
    result
}

pub fn CompileTokens(ctokens: Vec<ComplexToken>, filename: String) -> Result<String, String> {
    let mut result = String::new();
    let line = &mut 1u32;
    for t in ctokens.into_iter() {
        match t {
            VARIABLE {local, names, values} => {
                if local {result += "local "}
                /*CompileIdentifiers(names);
                result += " = ";
                CompileExpressions(values);*/
                result += &format!("{} = {}", CompileIdentifiers(names), CompileExpressions(line, values));
            }
            _ => {panic!("Unexpected ComplexToken found")}
        }
    }
    Ok(result)
}