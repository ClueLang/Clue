use std::{fs, path::Path, time::Instant};
use clap::{Args, crate_version};
use clue_core::{
	env::{
		BitwiseMode,
		ContinueMode,
		LuaVersion,
		Options
	},
	preprocessor::{
		preprocess_code,
		read_file,
		PPCode,
		PPVars, preprocess_codes
	},
	check,
	format_clue, scanner::{Token, scan_code}, parser::parse_tokens, compiler::Compiler
};
use crate::threads::compile_folder;

#[derive(Args)]
pub struct BuildArgs {
	/// The path to the directory where the *.clue files are located.
	/// Every directory inside the given directory will be checked too.
	/// If the path points to a single *.clue file, only that file will be compiled.
	path: String,

	/// The name the output file will have
	/// [default for compiling a directory: main]
	/// [default for compiling a single file: that file's name]
	#[clap(value_name = "OUTPUT FILE NAME")]
	outputname: Option<String>,

	/// Print list of detected tokens in compiled files
	#[clap(long)]
	tokens: bool,

	/// Print syntax structure of the tokens of the compiled files
	#[clap(long)]
	r#struct: bool,

	/// Print output Lua code in the console
	#[clap(short, long)]
	output: bool,

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

	/// Use a custom Lua file as base for compiling the directory
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
	#[cfg(feature = "interpreter")]
	/// Execute the output Lua code once it's compiled
	#[clap(short, long)]
	execute: bool,

	#[cfg(not(feature = "interpreter"))]
	#[clap(short, long, hide(true))]
	execute: bool,
}

pub fn build(args: BuildArgs) -> Result<(), String> {
	#[cfg(not(feature = "interpreter"))]
	if args.execute {
		return Err(String::from("Clue was installed without the 'interpreter' feature!"));
	}

	std::env::set_var("CLUE_VERSION", crate_version!());
	if args.r#continue == ContinueMode::LuaJIT {
		println!("Warning: \"LuaJIT continue mode was deprecated and replaced by goto mode\"")
	}
	let mut options = Options {
		env_tokens: args.tokens,
		env_struct: args.r#struct,
		env_jitbit: {
			if args.jitbit.is_some() { //DONT KEEP THIS IN 4.0
				println!("Warning: \"--jitbit was deprecated and replaced by --bitwise\"");
				args.jitbit
			} else if args.bitwise == BitwiseMode::Library {
				Some(String::from("bit"))
			} else {
				None
			}
		},
		env_bitwise: args.bitwise,
		env_continue: args.r#continue,
		env_rawsetglobals: args.rawsetglobals,
		env_debug: args.debug,
		env_output: if args.pathiscode {
			args.outputname.is_none()
		} else {
			args.output
		},
		env_target: args.target,
		env_targetos: args.targetos,
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
	if args.pathiscode {
		let mut codepath = args.path;
		let filename = String::from("(command line)");
		let code = unsafe { codepath.as_bytes_mut() };
		let preprocessed_code = preprocess_code(code, 1, false, &filename, &options)?;
		let (code, statics) = compile_code(
			preprocessed_code.0,
			&preprocessed_code.1,
			&filename,
			0,
			&options,
		)?;
		let code = code + &statics;
		#[cfg(feature = "interpreter")]
		if args.execute {
			execute_lua_code(&code)
		}
		return if let Some(outputname) = args.outputname.clone() {
			check!(fs::write(&outputname, &code));
			#[cfg(feature = "interpreter")]
			return finish(args.debug, args.execute, Some(outputname), code);
			#[cfg(not(feature = "interpreter"))]
			finish(args.debug, Some(outputname), code)
		} else {
			Ok(())
		};
	}
	let path: &Path = Path::new(&args.path);
	let (output_path, code) = if path.is_dir() {
		let (output, statics) = compile_folder(&args.path, String::new(), options)?;
		let code = match args.base {
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
		(
			if !args.dontsave {
				let output_name = match args.outputname {
					Some(output_name) if output_name.ends_with(".lua") => output_name,
					Some(output_name) => output_name + ".lua",
					None => String::from("main.lua"),
				};
				check!(fs::write(&output_name, &code));
				Some(output_name)
			} else {
				None
			},
			code,
		)
	} else if path.is_file() {
		let name = path.file_name().unwrap().to_string_lossy().into_owned();
		let (rawcode, variables) = read_file(&args.path, &name, &options)?;
		let (output, statics) = compile_code(rawcode, &variables, &name, 0, &options)?;
		let code = statics + &output;
		(
			if !args.dontsave {
				let output_name = match args.outputname {
					Some(output_name) if output_name.ends_with(".lua") => output_name,
					Some(output_name) => output_name + ".lua",
					None => format_clue!(name.strip_suffix(".clue").unwrap(), ".lua"),
				};
				check!(fs::write(&output_name, &code));
				Some(output_name)
			} else {
				None
			},
			code,
		)
	} else {
		return Err(String::from("The given path doesn't exist"));
	};

	#[cfg(feature = "interpreter")]
	return finish(args.debug, args.execute, output_path, code);
	#[cfg(not(feature = "interpreter"))]
	finish(args.debug, output_path, code)
}

pub fn compile_code(
	codes: PPCode,
	variables: &PPVars,
	name: &String,
	scope: usize,
	options: &Options,
) -> Result<(String, String), String> {
	let time = Instant::now();
	let code = preprocess_codes(0, codes, variables, name)?;
	let tokens: Vec<Token> = scan_code(code, name)?;
	if options.env_tokens {
		println!("Scanned tokens of file \"{name}\":\n{tokens:#?}");
	}
	let (ctokens, statics) = parse_tokens(
		tokens,
		/*if flag!(env_types) != TypesMode::NONE {
			Some(AHashMap::default())
		} else {
			None
		},*/
		name, options,
	)?;

	if options.env_struct {
		println!("Parsed structure of file \"{name}\":\n{ctokens:#?}");
	}

	let code = Compiler::new(options).compile_tokens(scope, ctokens);

	if options.env_output {
		println!("Compiled Lua code of file \"{name}\":\n{code}");
	}
	println!(
		"Compiled file \"{}\" in {} seconds!",
		name,
		time.elapsed().as_secs_f32()
	);
	Ok((code, statics))
}

#[cfg(feature = "interpreter")]
fn execute_lua_code(code: &str) {
	println!("Running compiled code...");
	let lua = mlua::Lua::new();
	let time = Instant::now();
	if let Err(error) = lua.load(code).exec() {
		println!("{error}");
	}
	println!("Code ran in {} seconds!", time.elapsed().as_secs_f32());
}

#[cfg(feature = "interpreter")]
fn finish(
	debug: bool,
	execute: bool,
	output_path: Option<String>,
	code: String,
) -> Result<(), String> {
	if debug {
		let new_output = format!(include_str!("debug.lua"), &code);
		if let Some(output_path) = output_path {
			check!(fs::write(output_path, &new_output));
		}
		if execute {
			execute_lua_code(&new_output)
		}
	} else if execute {
		execute_lua_code(&code)
	}
	Ok(())
}

#[cfg(not(feature = "interpreter"))]
fn finish(
	debug: bool,
	output_path: Option<String>,
	code: String,
) -> Result<(), String> {
	if debug {
		let new_output = format!(include_str!("debug.lua"), &code);
		if let Some(output_path) = output_path {
			check!(fs::write(output_path, &new_output));
		}
	}
	Ok(())
}
