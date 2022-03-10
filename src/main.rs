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

macro_rules! arg {
	($name: expr) => {
		unsafe {$name}
	};
}

mod scanner;
mod parser;
mod compiler;

use clap::Parser;
use scanner::*;
use parser::*;
use compiler::*;
use std::{
	io::prelude::*,
	time::Instant,
	path::Path,
	fs::File,
	fs
};

pub static mut finaloutput: String = String::new();

pub static mut ENV_TOKENS: bool = false;
pub static mut ENV_STRUCT: bool = false;
pub static mut ENV_OUTPUT: bool = false;
pub static mut ENV_NOJITBIT: bool = false;
pub static mut ENV_CONTINUE: bool = false;
pub static mut ENV_DONTSAVE: bool = false;
pub static mut ENV_PATHISCODE: bool = false;
pub static mut ENV_RAWSETGLOBALS: bool = false;

#[derive(Parser)]
#[clap(about, version = "b3.0.91", long_about = None)]
struct Cli {
	/// The path to the directory where the *.clue files are located.
	/// Every directory inside the given directory will be checked too.
	/// If the path points to a single *.clue file, only that file will be compiled.
	path: String,

	/// The name the output file will have
	#[clap(default_value = "main")]
	outputname: String,

	/// Print list of detected tokens in compiled files
	#[clap(long)]
	tokens: bool,

	/// Print syntax structure of the tokens of the compiled files
	#[clap(long)]
	r#struct: bool,

	/// Print output Lua code in the console
	#[clap(long)]
	output: bool,

	/// Don't use LuaJIT's bit library for bitwise operations
	#[clap(short, long)]
	nojitbit: bool,

	/// Don't use tags for continue
	#[clap(short, long)]
	r#continue: bool,

	/// Don't save compiled code
	#[clap(short, long)]
	dontsave: bool,

	/// Treat PATH not as a path but as Clue code
	#[clap(short, long)]
	pathiscode: bool,

	/// Use rawset to create globals
	#[clap(short, long)]
	rawsetglobals: bool,
}

fn AddToOutput(string: &str) {
	unsafe {finaloutput += string}
}

fn CompileCode(code: String, name: String, scope: usize) -> Result<String, String> {
	let time = Instant::now();
	let tokens: Vec<Token> = ScanCode(code, name.clone())?;
	if arg!(ENV_TOKENS) {
		println!("Scanned tokens of file \"{}\":\n{:#?}", name, tokens);
	}
	let ctokens = ParseTokens(tokens, name.clone())?;
	if arg!(ENV_STRUCT) {
		println!("Parsed structure of file \"{}\":\n{:#?}", name, ctokens);
	}
	let code = CompileTokens(scope, ctokens);
	if arg!(ENV_OUTPUT) {
		println!("Compiled Lua code of file \"{}\":\n{}", name, code);
	}
	println!("Compiled file \"{}\" in {} seconds!", name, time.elapsed().as_secs_f32());
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
	let cli = Cli::parse();
    unsafe {
		ENV_TOKENS = cli.tokens;
		ENV_STRUCT = cli.r#struct;
		ENV_OUTPUT = cli.output;
		ENV_NOJITBIT = cli.nojitbit;
		ENV_CONTINUE = cli.r#continue;
		ENV_DONTSAVE = cli.dontsave;
		ENV_PATHISCODE = cli.pathiscode;
		ENV_RAWSETGLOBALS = cli.rawsetglobals;
	}
	let codepath = &cli.path;
	if arg!(ENV_PATHISCODE) {
		let code = CompileCode(codepath.clone(), String::from("(command line)"), 0)?;
		println!("{}", code);
		return Ok(());
	}
	let path: &Path = Path::new(&codepath);
	if path.is_dir() {
		AddToOutput(include_str!("base.lua"));
		CompileFolder(path, String::new())?;
		AddToOutput("\r}\nrequire(\"main\")");
		if !arg!(ENV_DONTSAVE) {
			let outputname = &format!("\\{}.lua", cli.outputname);
			let compiledname = String::from(path.display().to_string()) + outputname;
			check!(fs::write(compiledname, unsafe {&finaloutput}))
		}
	} else if path.is_file() {
		let code = CompileFile(path, path.file_name().unwrap().to_string_lossy().into_owned(), 0)?;
		AddToOutput(&code);
		if !arg!(ENV_DONTSAVE) {
			let compiledname = String::from(path.display().to_string().strip_suffix(".clue").unwrap()) + ".lua";
			check!(fs::write(compiledname, unsafe {&finaloutput}))
		}
	} else {
		return Err(String::from("The given path doesn't exist"));
	}
	Ok(())
}