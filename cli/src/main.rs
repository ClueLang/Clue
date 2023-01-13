use ahash::AHashMap;
use clap::{crate_version, Parser};
use clue::{check_for_files, lock_and_pop, wait_threads, PreprocessorAnalyzerData, ThreadData};
use clue_core::{
	check,
	code::*,
	compiler::*,
	env::{ContinueMode, Options},
	format_clue,
	parser::*,
	preprocessor::*,
	scanner::*,
};

use flume::Sender;
use std::cmp::min;

use crossbeam_queue::SegQueue;
use std::{ffi::OsStr, fmt::Display, fs, path::Path, sync::Arc, thread, time::Instant};

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
		name, options,
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

fn compile_folder<P: AsRef<Path>>(
	file_path: P,
	rpath: String,
	options: &Options,
) -> Result<(String, String), String>
where
	P: AsRef<OsStr> + Display,
{
	let files = check!(check_for_files(file_path, rpath));
	let files_len = files.len();
	let threads_count = min(files_len, num_cpus::get() * 2);
	let codes = SegQueue::new();
	let files = Arc::new(files);
	let mut errored = 0;
	let mut variables = vec![];
	let mut output = String::with_capacity(files_len * 512) + "\n";
	let mut statics = String::with_capacity(512);

	let (tx, rx) = flume::unbounded();

	let mut threads = Vec::with_capacity(threads_count);

	for _ in 0..threads_count {
		// this `.clone()` is used to create new pointers
		// that can be used from inside the newly created thread
		let files = files.clone();
		let tx = tx.clone();

		let thread = thread::spawn(move || preprocessor_analyzer(files, tx));

		threads.push(thread);
	}

	wait_threads(threads);

	while let Ok(data) = rx.try_recv() {
		if data.errored {
			errored += 1;
			continue;
		}

		variables.push(data.variables);
		codes.push(data.codes);
	}

	match errored {
		0 => {}
		1 => return Err(String::from("1 file failed to compile!")),
		n => return Err(format!("{n} files failed to compile!")),
	}

	let variables = Arc::new(
		variables
			.iter()
			.flat_map(|file_variables| file_variables.to_owned())
			.collect::<AHashMap<Code, PPVar>>(),
	);

	let mut threads = Vec::with_capacity(threads_count);
	let (tx, rx) = flume::unbounded();
	let codes = Arc::new(codes);

	for _ in 0..threads_count {
		let tx = tx.clone();
		let options = options.clone();
		let codes = codes.clone();
		let variables = variables.clone();

		let thread =
			thread::spawn(move || analyze_and_compile(tx, &options, codes, variables.clone()));

		threads.push(thread);
	}

	wait_threads(threads);

	while let Ok(data) = rx.try_recv() {
		if data.errored {
			errored += 1;
			continue;
		}

		output += &data.output;
		statics += &data.static_vars;
	}

	match errored {
		0 => Ok((output.chars().collect(), statics.chars().collect())),
		1 => Err(String::from("1 file failed to compile!")),
		n => Err(format!("{n} files failed to compile!")),
	}
}

fn analyze_and_compile(
	tx: Sender<ThreadData>,
	options: &Options,
	codes: Arc<SegQueue<(Vec<(Code, bool)>, String)>>,
	variables: Arc<AHashMap<Code, PPVar>>,
) {
	loop {
		let (codes, realname) = match lock_and_pop(codes.clone()) {
			None => break,
			Some((codes, realname)) => (codes, realname),
		};

		let (code, static_vars) = match compile_code(codes, &variables, &realname, 2, &options) {
			Ok(t) => t,
			Err(e) => {
				tx.send(ThreadData {
					errored: true,
					output: "".to_string(),
					static_vars: "".to_string(),
				})
				.unwrap();
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

		tx.send(ThreadData {
			errored: false,
			output: string,
			static_vars,
		})
		.unwrap();
	}
}

fn preprocessor_analyzer(
	files: Arc<SegQueue<(String, String)>>,
	tx: Sender<PreprocessorAnalyzerData>,
) {
	loop {
		let (filename, realname) = match lock_and_pop(files.clone()) {
			None => break,
			Some((filename, realname)) => (filename, realname),
		};

		let (file_codes, file_variables) = match read_file(&filename, &filename) {
			Ok(t) => t,
			Err(e) => {
				tx.send(PreprocessorAnalyzerData {
					errored: true,
					codes: Default::default(),
					variables: Default::default(),
				})
				.unwrap();
				println!("Error: {}", e);
				continue;
			}
		};

		tx.send(PreprocessorAnalyzerData {
			errored: false,
			codes: (file_codes, realname),
			variables: file_variables,
		})
		.unwrap();
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

#[cfg(test)]
mod test {
	use clue_core::env::Options;

	use crate::compile_folder;

	#[test]
	fn compilation_success() {
		compile_folder("../examples/", String::new(), &Options::default()).unwrap();
	}
}
