use crate::scanner::Token;
use self::ComplexToken::*;

pub enum ComplexToken {
    EXPRESSION  {
        tokens: Vec<Token>,
        line: u32
    },
    EXPRESSIONS {
        exps: Vec<Vec<Token>>,
        line: u32
    }
}

struct TokensInfo {
	line: u32,
	start: u32,
	current: u32,
	size: usize,
	code: Vec<char>,
	filename: String,
	tokens: Vec<Token>,
	errored: bool,
}

pub fn ParseTokens(tokens: Vec<Token>, filename: String) -> Result<Vec<ComplexToken>, String> {
    let ctokens: Vec<ComplexToken> = Vec::new();
    for token in tokens {

    }
    Ok(ctokens)
}