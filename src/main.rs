#![allow(non_snake_case)]
#![allow(non_upper_case_globals)]

macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string()),
		}
	};
}

macro_rules! arg {
	($name: expr) => {
		unsafe { $name }
	};
}

mod compiler;
mod parser;
mod scanner;

use clap::{ArgEnum, Parser};
use compiler::*;
use mlua::prelude::*;
use parser::*;
use scanner::*;
use std::{fs, fs::File, io::prelude::*, path::Path, time::Instant};

pub static mut finaloutput: String = String::new();

pub static mut ENV_TOKENS: bool = false;
pub static mut ENV_STRUCT: bool = false;
pub static mut ENV_OUTPUT: bool = false;
pub static mut ENV_JITBIT: Option<String> = None;
pub static mut ENV_CONTINUE: ContinueMode = ContinueMode::SIMPLE;
pub static mut ENV_RAWSETGLOBALS: bool = false;
pub static mut ENV_DEBUG: bool = false;

#[derive(Copy, Clone, PartialEq, ArgEnum)]
pub enum ContinueMode {
	SIMPLE,
	LUAJIT,
	MOONSCRIPT,
}

#[derive(Parser)]
#[clap(about = "C/Rust like programming language that compiles into Lua code\nMade by Maiori\nhttps://github.com/ClueLang/Clue", version, long_about = None)]
struct Cli {
	/// The path to the directory where the *.clue files are located.
	/// Every directory inside the given directory will be checked too.
	/// If the path points to a single *.clue file, only that file will be compiled.
	#[clap(required_unless_present = "license")]
	path: Option<String>,

	/// The name the output file will have
	#[clap(default_value = "main", value_name = "OUTPUT FILE NAME")]
	outputname: String,

	/// Print license information
	#[clap(short = 'L', long, display_order = 1000)]
	license: bool,

	/// Print list of detected tokens in compiled files
	#[clap(long)]
	tokens: bool,

	/// Print syntax structure of the tokens of the compiled files
	#[clap(long)]
	r#struct: bool,

	/// Print output Lua code in the console
	#[clap(long)]
	output: bool,

	/// Use LuaJIT's bit library for bitwise operations
	#[clap(short, long, value_name = "VAR NAME")]
	jitbit: Option<String>,

	/// Change the way continue identifiers are compiled
	#[clap(short, long, value_enum, default_value = "simple", value_name = "MODE")]
	r#continue: ContinueMode,

	/// Don't save compiled code
	#[clap(short = 'D', long)]
	dontsave: bool,

	/// Treat PATH not as a path but as Clue code
	#[clap(short, long)]
	pathiscode: bool,

	/// Use rawset to create globals
	#[clap(short, long)]
	rawsetglobals: bool,

	/// Add debug information in output (might slow down runtime)
	#[clap(short, long)]
	debug: bool,

	/// Use a custom Lua file as base for compiling the directory
	#[clap(short, long, value_name = "FILE NAME")]
	base: Option<String>,

	/// Execute the output Lua code once it's compiled
	#[clap(short, long)]
	execute: bool,
}

fn AddToOutput(string: &str) {
	unsafe { finaloutput += string }
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
	println!(
		"Compiled file \"{}\" in {} seconds!",
		name,
		time.elapsed().as_secs_f32()
	);
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
		let name: String = entry
			.path()
			.file_name()
			.unwrap()
			.to_string_lossy()
			.into_owned();
		let filePathName: String = format!("{}/{}", path.display(), name);
		let filepath: &Path = &Path::new(&filePathName);
		let rname = rpath.clone() + &name;
		if filepath.is_dir() {
			CompileFolder(filepath, rname + ".")?;
		} else if filePathName.ends_with(".clue") {
			let code = CompileFile(filepath, name, 2)?;
			let rname = rname.strip_suffix(".clue").unwrap();
			AddToOutput(&format!(
				"\t[\"{}\"] = function()\n{}\n\tend,\n",
				rname, code
			));
		}
	}
	Ok(())
}

fn ExecuteLuaCode(code: &String) -> Result<(), String> {
	println!("Running compiled code...");
	let lua = Lua::new();
	let time = Instant::now();
	if let Err(error) = lua.load(code).exec() {
		println!("{}", error);
	}
	println!("Code ran in {} seconds!", time.elapsed().as_secs_f32());
	Ok(())
}

fn main() -> Result<(), String> {
	let cli = Cli::parse();
	if cli.license {
		println!("{}", include_str!("../LICENSE"));
		return Ok(());
	}
	unsafe {
		ENV_TOKENS = cli.tokens;
		ENV_STRUCT = cli.r#struct;
		ENV_OUTPUT = cli.output;
		ENV_JITBIT = cli.jitbit;
		ENV_CONTINUE = cli.r#continue;
		ENV_RAWSETGLOBALS = cli.rawsetglobals;
		ENV_DEBUG = cli.debug;
	}
	if let Some(bit) = arg!(&ENV_JITBIT) {
		AddToOutput(&format!("local {} = require(\"bit\");\n", bit));
	}
	let codepath = cli.path.unwrap();
	if cli.pathiscode {
		let code = CompileCode(codepath.clone(), String::from("(command line)"), 0)?;
		if cli.execute {
			ExecuteLuaCode(&code)?
		} else {
			println!("{}", code);
		}
		return Ok(());
	}
	let path: &Path = Path::new(&codepath);
	let mut compiledname = String::new();
	if path.is_dir() {
		AddToOutput("--STATICS\n");
		CompileFolder(path, String::new())?;
		unsafe {
			let (statics, output) = finaloutput.rsplit_once("--STATICS").unwrap();
			finaloutput = match cli.base {
				Some(filename) => {
					let base = match fs::read(filename) {
						Ok(base) => base,
						Err(_) => return Err(String::from("The given custom base was not found!")),
					};
					check!(std::str::from_utf8(&base))
						.to_string()
						.replace("--STATICS\n", &statics)
						.replace("§", &output)
				}
				None => include_str!("base.lua")
					.replace("--STATICS\n", &statics)
					.replace("§", &output),
			}
		}
		if !cli.dontsave {
			let outputname = &format!(
				"{}.lua",
				match cli.outputname.strip_suffix(".lua") {
					Some(outputname) => outputname,
					None => &cli.outputname,
				}
			);
			compiledname = if path.display().to_string().ends_with('/')
				|| path.display().to_string().ends_with('\\')
			{
				format!("{}{}", path.display(), outputname)
			} else {
				format!("{}/{}", path.display(), outputname)
			};
			check!(fs::write(&compiledname, unsafe { &finaloutput }))
		}
	} else if path.is_file() {
		let code = CompileFile(
			path,
			path.file_name().unwrap().to_string_lossy().into_owned(),
			0,
		)?;
		AddToOutput(&code);
		if !cli.dontsave {
			compiledname =
				String::from(path.display().to_string().strip_suffix(".clue").unwrap()) + ".lua";
			check!(fs::write(&compiledname, unsafe { &finaloutput }))
		}
	} else {
		return Err(String::from("The given path doesn't exist"));
	}
	if arg!(ENV_DEBUG) {
		let newoutput = format!(include_str!("debug.lua"), unsafe { &finaloutput });
		check!(fs::write(compiledname, &newoutput));
		if cli.execute {
			ExecuteLuaCode(&newoutput)?;
		}
	} else if cli.execute {
		ExecuteLuaCode(unsafe { &finaloutput })?;
	}
	Ok(())
}

#[cfg(test)]
mod test {
	use crate::CompileFolder;
	use std::path::Path;

	#[test]
	fn CompilationSuccess() {
		CompileFolder(Path::new("examples/"), String::new()).unwrap();
	}
}
