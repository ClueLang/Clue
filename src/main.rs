#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string())
		}
	};
}

mod options;
mod scanner;
mod parser;
mod compiler;

use std::env;
use std::fs;
use std::fs::File;
use std::io::prelude::*;
use std::path::Path;
use options::*;
use scanner::*;
use parser::*;
use compiler::*;

pub static mut output: String = String::new();

fn AddToOutput(string: &str) {
	unsafe {output += string}
}

fn CompileCode(code: String, name: String, scope: usize) -> Result<String, String> {
	let tokens: Vec<Token> = ScanCode(code, name.clone())?;
	if *ENV_TOKENS {
		println!("Scanned tokens of file \"{}\":\n{:#?}", name, tokens);
	}
	let ctokens: Vec<ComplexToken> = ParseTokens(tokens, name.clone())?;
	if *ENV_STRUCT {
		println!("Parsed structure of file \"{}\":\n{:#?}", name, ctokens);
	}
	let code = CompileTokens(scope, ctokens);
	if *ENV_PRINTOUT {
		println!("Compiled Lua code of file \"{}\":\n{}", name, code);
	}
	Ok(code)
}

fn CompileFile(path: &Path, name: String, scope: usize) -> Result<String, String> {
	let mut code: String = String::new();
	check!(check!(File::open(path)).read_to_string(&mut code));
	Ok(CompileCode(code, name, scope)?)
}

fn CompileFolder(path: &Path, rpath: String) -> Result<(), String> {
	for entry in check!(fs::read_dir(path)) {
		let entry = check!(entry);
		let name: String = entry.path().file_name().unwrap().to_string_lossy().into_owned();
		let filePathName: String = path.display().to_string() + "\\" + &name;
		let filepath: &Path = &Path::new(&filePathName);
		let rname = rpath.clone() + &name;
		if filepath.is_dir() {
			CompileFolder(filepath, rname + ".")?;
		} else if filePathName.ends_with(".clue") {
			let code = CompileFile(filepath, name.clone(), 2)?;
			let rname = rname.strip_suffix(".clue").unwrap();
			AddToOutput(&format!("[\"{}\"] = function()\n{}\n\tend,\n\t", rname, code));
		}
	}
	Ok(())
}

fn main() -> Result<(), String> {
	let args: Vec<String> = env::args().collect();
	SetupEnv();
	let codepath;
	if args.len() == 1 || args.contains(&String::from("-help")) {
		println!(
"Clue transpiler (BETA)
Made by Felix44
https://github.com/Felix-44/Clue

USAGE:
	clue -help		Send this very message
	clue -version 		Send this transpiler's version
	clue [PATH] [OPTIONS]	Compile any *.clue file found in the given directory

PATH:
	The path to the directory where the *.clue files are located.
	Every directory inside the given directory will be checked too.
	If the path points to a single *.clue file, only that file will be compiled.

OPTIONS:
	-tokens		Print list of detected tokens in compiled files
	-struct 	Print syntax structure of the tokens of the compiled files
	-printout	Print output Lua code in the console
	-nojitbit	Don't use LuaJIT's bit library for bitwise operations
	-continue	Don't use tags for continue
	-dontsave	Don't save compiled code
	-pathiscode 	Treat PATH not as a path but as Clue code");
		return Ok(());
	}
	codepath = &args[1];
	if codepath == "-version" {
		println!("Version b2.0.85");
		return Ok(());
	}
	if *ENV_PATHISCODE {
		let code = CompileCode(codepath.clone(), String::from("(command line)"), 0)?;
		println!("{}", code);
		return Ok(());
	}
	let path: &Path = Path::new(&codepath);
	if path.is_dir() {
		AddToOutput(include_str!("base.lua"));
		AddToOutput("modules = {\n\t");
		CompileFolder(path, String::new())?;
		AddToOutput("\r}\nmodules.main()");
		if !*ENV_DONTSAVE {
			let compiledname = String::from(path.display().to_string()) + "\\main.lua";
			check!(fs::write(compiledname, unsafe {output.clone()}))
		}
	} else if path.is_file() {
		let code = CompileFile(path, path.file_name().unwrap().to_string_lossy().into_owned(), 0)?;
		if !*ENV_DONTSAVE {
			let compiledname = String::from(path.display().to_string().strip_suffix(".clue").unwrap()) + ".lua";
			check!(fs::write(compiledname, code))
		}
	} else {
		return Err(String::from("The given path doesn't exist"));
	}
	Ok(())
}