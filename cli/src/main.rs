#![allow(clippy::blocks_in_if_conditions)]

use clap::{crate_version, Parser, ValueEnum, builder::ArgPredicate};
use clue_core::{
	check,
	compiler::*,
	env::{BitwiseMode, ContinueMode, LuaVersion, Options},
	errors::{print_errors, add_source_file},
	format_clue,
	parser::*,
	preprocessor::*,
	scanner::*,
};
use std::{env, fs, path::PathBuf, time::Instant, process::exit, io::{self, Read}};
use threads::compile_folder;
use colored::*;

mod threads;

#[derive(Debug, Copy, Clone, PartialEq, Eq, ValueEnum)]
enum ColorMode {
	Auto,
	Always,
	Never
}

#[derive(Parser)]
#[clap(
	version,
	about = "C/Rust like programming language that compiles into Lua code\nMade by Felicia.iso\nhttps://github.com/ClueLang/Clue",
)]
struct Cli {
	/// The path to the directory where the *.clue files are located,
	/// every directory inside the given directory will be checked too;
	/// if the path points to a single *.clue file, only that file will be compiled
	#[clap(default_value_if("--license", ArgPredicate::IsPresent, Some("test")))] //FIXME
	path: PathBuf,

	/// The name the output file will have
	/// [default for compiling a directory: main]
	/// [default for compiling a single file: that file's name]
	#[clap(value_name = "OUTPUT FILE NAME")]
	outputname: Option<PathBuf>,

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
	#[clap(short, long)]
	output: bool,

	/// Print preprocessed file
	#[clap(short = 'E', long)]
	expand: bool,

	/// Use LuaJIT's bit library for bitwise operations
	#[clap(
		short,
		long,
		hide(true),
		default_missing_value = "bit",
		value_name = "VAR NAME"
	)]
	jitbit: Option<String>,

	/// Change the way bitwise operators are compiled
	#[clap(
		short,
		long,
		value_enum,
		ignore_case(true),
		default_value = "Clue",
		value_name = "MODE"
	)]
	bitwise: BitwiseMode,

	/// Change the way continue identifiers are compiled
	#[clap(
		short,
		long,
		value_enum,
		ignore_case(true),
		default_value = "simple",
		value_name = "MODE"
	)]
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

	/// Use a custom Lua file as base for compiling the directory,
	/// does nothing when not compiling a directory
	#[clap(short = 'B', long, value_name = "FILE NAME")]
	base: Option<String>,

	/// Uses preset configuration based on the targeted Lua version
	#[clap(
		short,
		long,
		value_enum,
		ignore_case(true),
		conflicts_with("bitwise"),
		conflicts_with("jitbit"),
		conflicts_with("continue"),
		value_name = "LUA VERSION"
	)]
	target: Option<LuaVersion>,

	/// Change OS checked by @ifos
	#[clap(long, default_value = std::env::consts::OS, value_name = "TARGET OS")]
	targetos: String,
	/*/// This is not yet supported (Coming out in 4.0)
	#[clap(short, long, value_name = "MODE")]
	types: Option<String>,*/

	/*	/// Enable type checking (might slow down compilation)
		#[clap(
			short,
			long,
			value_enum,
			default_value = "none",
			value_name = "MODE"
		)]
		types: TypesMode,

		/// Use the given Lua version's standard library (--types required)
		#[clap(
			long,
			value_enum,
			default_value = "luajit",
			value_name = "LUA VERSION",
			requires = "types"
		)]
		std: LuaSTD,
	*/
	#[cfg(feature = "mlua")]
	/// Execute the output Lua code once it's compiled
	#[clap(short, long)]
	execute: bool,

	#[cfg(feature = "lsp")]
	/// Print the symbol table of the compiled files
	#[clap(long, hide(true))]
	symbols: bool,

	#[clap(
		short = 'C',
		long,
		value_enum,
		ignore_case(true),
		default_value = "auto",
		value_name = "WHEN"
	)]
	/// Colorize the output
	color: ColorMode,
}

pub fn compile_code(
	codes: PPCode,
	variables: &PPVars,
	filename: &String,
	scope: usize,
	options: &Options,
) -> Result<(String, String), String> {
	let time = Instant::now();
	let code = preprocess_codes(0, codes, variables, filename)?;
	add_source_file(filename, &code);
	if options.env_expand {
		println!("Preprocessed file \"{filename}\":\n{code}");
	}
	let tokens = scan_code(code, filename, options)?;
	if options.env_tokens {
		println!("Scanned tokens of file \"{filename}\":\n{tokens:#?}");
	}
	let (ctokens, statics) = parse_tokens(
		tokens,
		//code,
		/*if flag!(env_types) != TypesMode::NONE {
			Some(AHashMap::default())
		} else {
			None
		},*/
		filename,
		options,
	)?;
	if options.env_struct {
		println!("Parsed structure of file \"{filename}\":\n{ctokens:#?}");
	}
	let code = Compiler::new(options, filename).compile_tokens(scope, ctokens)?;

	if options.env_output {
		println!("Compiled Lua code of file \"{filename}\":\n{code}");
	}
	println!(
		"{} \"{}\" in {} seconds!",
		"Compiled".green().bold(),
		filename,
		time.elapsed().as_secs_f32()
	);
	Ok((code, statics))
}

fn compile_from_string(
	name: &str,
	source_code: &mut [u8],
	options: &Options,
	#[cfg(feature = "mlua")] execute: bool,
	debug: bool,
	dont_save: bool,
) -> Result<(), String> {
	let filename = name.to_string();
	let preprocessed_code = preprocess_code(
		source_code,
		1,
		1,
		false,
		&filename,
		options
	)?;
	let (output, statics) = compile_code(
		preprocessed_code.0,
		&preprocessed_code.1,
		&filename,
		0,
		options,
	)?;
	let code = statics + &output;
	if options.env_outputname.is_some() {
		let outputname = options.env_outputname.clone();
		let (output_path, code) = save_result(dont_save, outputname, code)?;
		finish(
			debug,
			#[cfg(feature = "mlua")]
			execute,
			output_path,
			code
		)
	} else {
		#[cfg(feature = "mlua")]
		if execute {
			execute_lua_code(&code)
		}
		Ok(())
	}
}

#[cfg(feature = "mlua")]
fn execute_lua_code(code: &str) {
	println!("{} the compiled code...", "Running".blue().bold());
	let lua = mlua::Lua::new();
	if let Err(error) = lua.load(code).exec() {
		println!("{error}");
	}
}

fn finish(
	debug: bool,
	#[cfg(feature = "mlua")] execute: bool,
	output_path: Option<PathBuf>,
	code: String,
) -> Result<(), String> {
	if debug {
		let new_output = format!(
			include_str!("debug.lua"),
			format_clue!("\t", code.replace('\n', "\n\t"))
		);
		if let Some(output_path) = output_path {
			check!(fs::write(output_path, &new_output));
		}
		#[cfg(feature = "mlua")]
		if execute {
			execute_lua_code(&new_output)
		}
		return Ok(());
	}
	#[cfg(feature = "mlua")]
	if execute {
		execute_lua_code(&code)
	}
	Ok(())
}

fn save_result(
	dont_save: bool,
	output_name: Option<PathBuf>,
	code: String,
) -> Result<(Option<PathBuf>, String), String> {
	Ok((
		if !dont_save {
			let output_path = match output_name {
				Some(mut output_path) => {
					match output_path.extension() {
						Some(extension) if extension != "lua" => {
							output_path.set_extension(format_clue!(extension.to_string_lossy(), ".lua"));
						}
						None => {
							output_path.set_extension("lua");
						}
						_ => {}
					}
					output_path
				}
				None => PathBuf::from("main.lua"),
			};
			check!(fs::write(&output_path, &code));
			Some(output_path)
		} else {
			None
		},
		code,
	))
}

fn main() {
	#[cfg(windows)]
	colored::control::set_virtual_terminal(true);
	env::set_var("CLUE_VERSION", crate_version!());
	let cli = Cli::parse();
	if cli.license {
		print!(include_str!("../LICENSE"));
		return;
	}
	if cli.color != ColorMode::Auto {
		colored::control::set_override(cli.color == ColorMode::Always);
	}
	if cli.r#continue == ContinueMode::LuaJIT { //TODO: REMOVE LUAJIT mode
		println!("Warning: \"LuaJIT continue mode was deprecated and replaced by goto mode\"")
	}
	if let Err(e) = start_compilation(cli) {
		print_errors();
		eprintln!("{}: {e}", "Error".red().bold());
		exit(-1);
	}
}

fn start_compilation(cli: Cli) -> Result<(), String> {
	let mut path = cli.path;
	let read_from_stdin = path.as_os_str() == "-";
	let mut options = Options {
		env_tokens: cli.tokens,
		env_struct: cli.r#struct,
		env_expand: cli.expand,
		env_jitbit: {
			if cli.jitbit.is_some() {
				println!("Warning: \"--jitbit was deprecated and replaced by --bitwise\"");
				cli.jitbit
			} else if cli.bitwise == BitwiseMode::Library {
				Some(String::from("bit"))
			} else {
				None
			}
		},
		env_bitwise: cli.bitwise,
		env_continue: cli.r#continue,
		env_rawsetglobals: cli.rawsetglobals,
		env_debug: cli.debug,
		env_output: if cli.pathiscode || read_from_stdin {
			cli.outputname.is_none()
		} else {
			cli.output
		},
		env_outputname: cli.outputname,
		env_target: cli.target,
		env_targetos: cli.targetos,
		#[cfg(feature = "lsp")]
		env_symbols: cli.symbols,
        #[cfg(not(feature = "lsp"))]
        env_symbols: false,
	};
	options.preset();
	//let mut code = String::with_capacity(512);

	/*if let Some(bit) = &options.env_jitbit {
		check!(writeln!(&mut code, "local {bit} = require(\"bit\");"));
	}*/
	/*if flag!(env_types) != TypesMode::NONE {
		*check!(LUA_G.write()) = match flag!(env_std) {
			LuaSTD::LUA54 => Some(AHashMap::from_iter([(String::from("print"), LuaType::NIL)])), //PLACEHOLDER
			_ => Some(AHashMap::default()),
		};
	}*/
	if cli.pathiscode {
		return compile_from_string(
			"(command line)",
			unsafe { path.to_string_lossy().into_owned().as_bytes_mut() },
			&options,
			#[cfg(feature = "mlua")]
			cli.execute,
			cli.debug,
			cli.dontsave,
		);
	} else if read_from_stdin {
		let mut buf = Vec::new();
		check!(io::stdin().read_to_end(&mut buf));
		return compile_from_string(
			"(stdin)",
			&mut buf,
			&options,
			#[cfg(feature = "mlua")]
			cli.execute,
			cli.debug,
			cli.dontsave,
		);
	}
	let (output_path, code) = if path.is_dir() {
		let time = Instant::now();
		let output_name = options.env_outputname.clone();
		let (output, statics) = compile_folder(&path, String::new(), options)?;
		println!(
			"{} \"{}\" in {} seconds!",
			"Finished".green().bold(),
			path.display(),
			time.elapsed().as_secs_f32()
		);

		let code = match cli.base {
			Some(filename) => {
				let base = match fs::read(filename) {
					Ok(base) => base,
					Err(_) => return Err(String::from("The given custom base was not found!")),
				};
				check!(std::str::from_utf8(&base))
					.to_owned()
					.replace("--STATICS\n", &statics)
					.replace('§', &output)
			}
			None => include_str!("base.lua")
				.replace("--STATICS\n", &statics)
				.replace('§', &output),
		};
		save_result(cli.dontsave, output_name, code)?
	} else if {
		match path.extension() {
			Some(extension) if extension != "clue" => {
				path.set_extension(format_clue!(extension.to_string_lossy(), ".clue"));
			}
			None => {
				path.set_extension("clue");
			}
			_ => {}
		}
		path.is_file()
	} {
		let filename = path.file_name().unwrap().to_string_lossy().into_owned();
		let (rawcode, variables) = read_file(&path, &filename, &options)?;
		let (output, statics) = compile_code(rawcode, &variables, &filename, 0, &options)?;
		let code = statics + &output;
		save_result(cli.dontsave, options.env_outputname.or_else(|| Some(path.with_extension("lua"))), code)?
	} else {
		return Err(format!(
			"{} was not found!",
			path.to_string_lossy().into_owned()
		));
	};

	#[cfg(feature = "mlua")]
	return finish(cli.debug, cli.execute, output_path, code);
	#[cfg(not(feature = "mlua"))]
	finish(cli.debug, output_path, code)
}

#[cfg(test)]
mod tests {
	use crate::compile_folder;
	use clue_core::env::Options;

	#[test]
	fn compilation_success() {
		compile_folder("../examples/", String::new(), Options::default()).unwrap();
	}
}
