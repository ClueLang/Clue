#![allow(non_snake_case)]

macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string())
		}
	};
}

mod scanner;
mod parser;

use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use scanner::Token;
use parser::ComplexToken;

fn CompileFile(path: &Path, name: String, args: &[String]) -> Result<(), String> {
	let mut code: String = String::new();
	check!(check!(File::open(path)).read_to_string(&mut code));
	let tokens: Vec<Token> = scanner::ScanCode(code, name.clone())?;
	if args.contains(&"-tokens".to_string()) {
		println!("{:#?}", tokens);
	}
	let ctokens: Vec<ComplexToken> = parser::ParseTokens(tokens, name.clone())?;
	/*let compiledname: String = String::from(path.display()
		.to_string()
		.strip_suffix(".clue")
		.unwrap()) + ".lua";
	let output: File = check!(File::create(compiledname));*/
	/*for token in tokens.iter() {
		println!("{} \"{}\" {}", token.kind, token.lexeme, token.line);
	}*/
	println!("{:#?}", ctokens);
	Ok(())
}

fn CompileFolder(path: &Path, args: &[String]) -> Result<(), String> {
	for entry in check!(fs::read_dir(path)) {
		let entry = check!(entry);
		let name: String = entry.path().file_name().unwrap().to_string_lossy().into_owned();
		let filePathName: String = path.display().to_string() + "\\" + &name;
		let filepath: &Path = &Path::new(&filePathName);
		if filepath.is_dir() {
			CompileFolder(filepath, args)?;
		} else if filePathName.ends_with(".clue") {
			CompileFile(filepath, name, args)?;
		}
	}
	Ok(())
}

fn main() -> Result<(), String> {
	let args: Vec<String> = env::args().collect();
	let codepath;
	if args.len() == 1 || args.contains(&"-help".to_string()) {
		println!(
"Clue transpiler

USAGE:
	clue -help		Send this very message
	clue -version 		Send this transpiler's version
	clue [Path] [OPTIONS]	Compile any *.clue file found in the given directory

OPTIONS:
	-blua		Compile into Blua instead of Lua (slightly different rules)
	-tokens		Print list of detected tokens in compiled files
	-struct 	Print syntax structure of the tokens of the compiled files
	-dontsave	Don't save compiled code");
		return Ok(());
	}
	codepath = &args[1];
	if codepath == "-version" {
		println!("Version a1.0.38");
		return Ok(());
	}
	let path: &Path = Path::new(&codepath);
	if !path.is_dir() {
		return Err(String::from("The given path doesn't exist"));
	}
	CompileFolder(path, &args)?;
	Ok(())
}