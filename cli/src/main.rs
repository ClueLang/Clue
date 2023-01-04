use ahash::AHashMap;
use clap::{crate_version, Parser};
use clue_core as clue;
use clue::{
	check,
	compiler::*,
	format_clue,
	parser::*,
	preprocessor::*,
	scanner::*,
	env::{ContinueMode, Options}/*,
	LUA_G*/
};
use std::{
	cmp::min,
	sync::{Arc, Mutex},
	thread,
	ffi::OsStr,
	fmt::Display,
	fs,
	path::Path,
	time::Instant,
};

macro_rules! println {
    ($($rest:tt)*) => {
        std::println!($($rest)*)
    }
}

#[derive(Parser)]
#[clap(
	version,
	about = "C/Rust like programming language that compiles into Lua code\nMade by Maiori\nhttps://github.com/ClueLang/Clue",
	long_about = None
)]
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

	/// This is not yet supported (Coming out in 4.0)
	#[clap(short, long, value_name = "MODE")]
	types: Option<String>,

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
}

fn compile_code(
	mut codes: Vec<(Code, bool)>,
	variables: &PPVars,
	name: &String,
	scope: usize,
	options: &Options,
) -> Result<(String, String), String> {
	let time = Instant::now();
	let code = if codes.len() == 1 {
		codes.pop().unwrap().0
	} else {
		let mut code = Code::new();
		for (codepart, uses_vars) in codes {
			code.append(if uses_vars {
				preprocess_variables(0, (&codepart).into_iter().peekable(), variables, name)?
			} else {
				codepart
			})
		}
		code
	};
	let tokens: Vec<Token> = scan_code(code, name)?;
	if options.env_tokens {
		println!("Scanned tokens of file \"{}\":\n{:#?}", name, tokens);
	}
	let (ctokens, statics) = parse_tokens(
		tokens,
		/*if flag!(env_types) != TypesMode::NONE {
			Some(AHashMap::default())
		} else {
			None
		},*/
		name,
		options,
	)?;

	if options.env_struct {
		println!("Parsed structure of file \"{}\":\n{:#?}", name, ctokens);
	}

	let code = Compiler::new(options).compile_tokens(scope, ctokens);

	if options.env_output {
		println!("Compiled Lua code of file \"{}\":\n{}", name, code);
	}
	println!(
		"Compiled file \"{}\" in {} seconds!",
		name,
		time.elapsed().as_secs_f32()
	);
	Ok((code, statics))
}

fn check_for_files<P: AsRef<Path>>(
	path: P,
	rpath: String,
) -> Result<Vec<(String, String)>, std::io::Error>
where
	P: AsRef<OsStr> + Display,
{
	let mut files = vec![];
	for entry in fs::read_dir(&path)? {
		let entry = entry?;
		let name = entry
			.path()
			.file_name()
			.unwrap()
			.to_string_lossy()
			.into_owned();
		let filepath_name = format!("{path}/{name}");
		let filepath = Path::new(&filepath_name);
		let realname = rpath.clone() + &name;
		if filepath.is_dir() {
			let mut inside_files = check_for_files(filepath_name, realname + ".")?;
			files.append(&mut inside_files);
		} else if filepath_name.ends_with(".clue") {
			files.push((filepath_name, realname));
		}
	}
	Ok(files)
}

fn compile_folder<P: AsRef<Path>>(
	path: P,
	rpath: String,
	options: &Options,
) -> Result<(String, String), String>
where
	P: AsRef<OsStr> + Display,
{
	let files = check!(check_for_files(path, rpath));
	let threads_count = min(files.len(), num_cpus::get() * 2);
	let codes = Arc::new(Mutex::new(Vec::with_capacity(files.len())));
	let files = Arc::new(Mutex::new(files));
	let errored = Arc::new(Mutex::new(0u8));
	let variables = Arc::new(Mutex::new(Vec::new()));
	let output = Arc::new(Mutex::new(
		String::with_capacity(files.lock().unwrap().len() * 512) + "\n",
	));
	let statics = Arc::new(Mutex::new(String::with_capacity(512)));
	let mut threads = Vec::with_capacity(threads_count);
	for _ in 0..threads_count {
		// this `.clone()` is used to create new pointers
		// that can be used from inside the newly created thread
		let files = files.clone();
		let errored = errored.clone();
		let codes = codes.clone();
		let variables = variables.clone();
		let thread = thread::spawn(move || loop {
			// Acquire the lock, check the files to compile, get the file to compile and then drop the lock
			let (filename, realname) = {
				let mut files = files.lock().unwrap();
				if files.is_empty() {
					break;
				}
				files.pop().unwrap()
			};
			let (file_codes, file_variables) = match read_file(&filename, &filename) {
				Ok(t) => t,
				Err(e) => {
					*errored.lock().unwrap() += 1;
					println!("Error: {}", e);
					continue;
				}
			};
			codes.lock().unwrap().push((file_codes, realname));
			variables.lock().unwrap().push(file_variables);
		});
		threads.push(thread);
	}
	for thread in threads {
		thread.join().unwrap();
	}
	match *errored.lock().unwrap() {
		0 => {},
		1 => return Err(String::from("1 file failed to compile!")),
		n => return Err(format!("{n} files failed to compile!")),
	}
	let variables = Arc::new({
		let mut total_vars = AHashMap::new();
		let mut variables = variables.lock().unwrap();
		while let Some(file_variables) = variables.pop() {
			for (k, v) in file_variables {
				total_vars.insert(k, v);
			}
		}
		total_vars
	});
	let mut threads = Vec::with_capacity(threads_count);
	for _ in 0..threads_count {
		let options = options.clone();
		let codes = codes.clone();
		let variables = variables.clone();
		let errored = errored.clone();
		let output = output.clone();
		let statics = statics.clone();
		let thread = thread::spawn(move || loop {
			let (codes, realname) = {
				let mut codes = codes.lock().unwrap();
				if codes.is_empty() {
					break;
				}
				codes.pop().unwrap()
			};
			let (code, static_vars) = match compile_code(codes, &variables, &realname, 2, &options) {
				Ok(t) => t,
				Err(e) => {
					*errored.lock().unwrap() += 1;
					println!("Error: {}", e);
					continue;
				}
			};
			let string = format_clue!(
				"\t[\"",
				realname.strip_suffix(".clue").unwrap(),
				"\"] = function()\n",
				code,
				"\n\tend,\n"
			);
			let mut output = output.lock().unwrap();
			*statics.lock().unwrap() = static_vars;
			*output = output.clone() + &string;
		});
		threads.push(thread);
	}
	for thread in threads {
		thread.join().unwrap();
	}
	let errored = *errored.lock().unwrap();
	match errored {
		0 => Ok((
			output.lock().unwrap().drain(..).collect(),
			statics.lock().unwrap().drain(..).collect(),
		)),
		1 => Err(String::from("1 file failed to compile!")),
		n => Err(format!("{n} files failed to compile!")),
	}
}

#[cfg(feature = "mlua")]
fn execute_lua_code(code: &str) {
	println!("Running compiled code...");
	let lua = mlua::Lua::new();
	let time = Instant::now();
	if let Err(error) = lua.load(code).exec() {
		println!("{}", error);
	}
	println!("Code ran in {} seconds!", time.elapsed().as_secs_f32());
}

fn main() -> Result<(), String> {
	std::env::set_var("CLUE_VERSION", crate_version!());
	let cli = Cli::parse();
	if cli.license {
		println!(include_str!("../../LICENSE"));
		return Ok(());
	} else if cli.types.is_some() {
		//TEMPORARY PLACEHOLDER UNTIL 4.0
		return Err(String::from("Type checking is not supported yet!"));
	}

	let options = Options {
		env_tokens: cli.tokens,
		env_struct: cli.r#struct,
		env_jitbit: cli.jitbit.clone(),
		env_continue: cli.r#continue,
		env_rawsetglobals: cli.rawsetglobals,
		env_debug: cli.debug,
		env_output: cli.output,
	};

	let mut code = String::with_capacity(512);

	if let Some(bit) = &options.env_jitbit {
		code += &format!("local {bit} = require(\"bit\");\n");
	}
	/*if flag!(env_types) != TypesMode::NONE {
		*check!(LUA_G.write()) = match flag!(env_std) {
			LuaSTD::LUA54 => Some(AHashMap::from_iter([(String::from("print"), LuaType::NIL)])), //PLACEHOLDER
			_ => Some(AHashMap::default()),
		};
	}*/
	let codepath = cli.path.unwrap();
	if cli.pathiscode {
		let filename = String::from("(command line)");
		let (rawcode, variables) = read_file(codepath, &filename)?;
		let (code, statics) = compile_code(rawcode, &variables, &filename, 0, &options)?;
		let code = code + &statics;
		println!("{}", code);
		#[cfg(feature = "mlua")]
		if cli.execute {
			execute_lua_code(&code)
		}
		return Ok(());
	}
	let path: &Path = Path::new(&codepath);
	let mut compiledname = String::new();

	if path.is_dir() {
		let (output, statics) = compile_folder(&codepath, String::new(), &options)?;

		code = match cli.base {
			Some(filename) => {
				let base = match fs::read(filename) {
					Ok(base) => base,
					Err(_) => return Err(String::from("The given custom base was not found!")),
				};
				check!(std::str::from_utf8(&base))
					.to_string()
					.replace("--STATICS\n", &statics)
					.replace('§', &output)
			}
			None => include_str!("base.lua")
				.replace("--STATICS\n", &statics)
				.replace('§', &output),
		};
		if !cli.dontsave {
			let output_name = &format!(
				"{}.lua",
				match cli.outputname.strip_suffix(".lua") {
					Some(output_name) => output_name,
					None => &cli.outputname,
				}
			);
			let display = path.display().to_string();
			compiledname = if display.ends_with('/') || display.ends_with('\\') {
				format!("{display}{output_name}")
			} else {
				format!("{display}/{output_name}")
			};
			check!(fs::write(&compiledname, &code))
		}
	} else if path.is_file() {
		let name = path.file_name().unwrap().to_string_lossy().into_owned();
		let (rawcode, variables) = check!(read_file(&codepath, &name));
		let (output, statics) = compile_code(rawcode, &variables, &name, 0, &options)?;
		code = statics + &output;
		if !cli.dontsave {
			compiledname =
				String::from(path.display().to_string().strip_suffix(".clue").unwrap()) + ".lua";
			check!(fs::write(&compiledname, &code))
		}
	} else {
		return Err(String::from("The given path doesn't exist"));
	}

	if options.env_debug {
		let newoutput = format!(include_str!("debug.lua"), &code);
		check!(fs::write(compiledname, &newoutput));
		#[cfg(feature = "mlua")]
		if cli.execute {
			execute_lua_code(&newoutput)
		}
	} else {
		#[cfg(feature = "mlua")]
		if cli.execute {
			execute_lua_code(&code)
		}
	}
	Ok(())
}
/*
#[cfg(test)]
mod test {
	use clue_core::env::Options;

	use crate::compile_folder;

	#[test]
	fn compilation_success() {
		compile_folder("../examples/", String::new(), &Options::default()).unwrap();
	}
}*/
