use crate::parser::ComplexToken;
use crate::parser::ComplexToken::*;

struct CompilerInfo {
    ctokens: Vec<ComplexToken>,
    filename: String,
    line: u32,
    result: String
}

impl CompilerInfo {
    fn new(ctokens: Vec<ComplexToken>, filename: String) -> CompilerInfo {
		CompilerInfo {
			line: 0,
            result: String::new(),
            ctokens, filename
		}
	}
}

pub fn CompileTokens(ctokens: Vec<ComplexToken>, filename: String) -> Result<String, String> {
    let i = CompilerInfo::new(ctokens, filename);
    for t in i.ctokens {
        match t {
            VARIABLE {local, values} => {

            }
        }
    }
    Ok(String::from("--Hello, world!"))
}