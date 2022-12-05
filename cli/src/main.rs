use clue_core as clue;
use clap::Parser;
use clue::env::ContinueMode;
use clue::{
	check, format_clue, preprocessor::*, compiler::*, flag, parser::*, scanner::*,
	ENV_DATA /*, LUA_G*/
};
use std::cmp::min;
use std::sync::{Arc, Mutex};
use std::thread::spawn;
use std::{ffi::OsStr, fmt::Display, fs, fs::File, io::prelude::*, path::Path, time::Instant};

macro_rules! println {
    ($($rest:tt)*) => {
        #[cfg(not(feature = "devtimer"))]
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

fn add_to_output(string: &str) {
	ENV_DATA
		.write()
		.expect("Can't lock env_data")
		.add_output_code(String::from(string));
}

fn compile_code(mut code: String, name: String, scope: usize) -> Result<String, String> {
	let time = Instant::now();
	if code.contains('@') || code.contains('$') {
		code = preprocess_code(code, &mut 1usize, &name)?.0.iter().collect();
	}
	let tokens: Vec<Token> = scan_code(code, name.clone())?;
	if flag!(env_tokens) {
		println!("Scanned tokens of file \"{}\":\n{:#?}", name, tokens);
	}
	let ctokens = parse_tokens(
		tokens,
		/*if flag!(env_types) != TypesMode::NONE {
			Some(AHashMap::default())
		} else {
			None
		},*/
		name.clone(),
	)?;
	if flag!(env_struct) {
		println!("Parsed structure of file \"{}\":\n{:#?}", name, ctokens);
	}
	let code = compile_tokens(scope, ctokens);
	if flag!(env_output) {
		println!("Compiled Lua code of file \"{}\":\n{}", name, code);
	}
	println!(
		"Compiled file \"{}\" in {} seconds!",
		name,
		time.elapsed().as_secs_f32()
	);
	Ok(code)
}

fn compile_file<P: AsRef<Path>>(path: P, name: String, scope: usize) -> Result<String, String>
where
	P: AsRef<OsStr> + Display,
{
	let mut code: String = String::with_capacity(512);
	check!(check!(File::open(path)).read_to_string(&mut code));
	compile_code(code, name, scope)
}

fn check_for_files<P: AsRef<Path>>(path: P, rpath: String) -> Result<Vec<(String, String)>, std::io::Error>
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

#[cfg(not(feature = "devtimer"))]
fn compile_folder<P: AsRef<Path>>(path: P, rpath: String) -> Result<(), String>
where
	P: AsRef<OsStr> + Display,
{
	let files = Arc::new(Mutex::new(check!(check_for_files(path, rpath))));
	let threads_count = min(files.lock().unwrap().len(), num_cpus::get() * 2);
	let errored = Arc::new(Mutex::new(0u8));
	let mut threads = Vec::with_capacity(threads_count);
	for _ in 0..threads_count {
		// this `.clone()` is used to create a new pointer to the outside `files`
		// that can be used from inside the newly created thread
		let files = files.clone();
		let errored = errored.clone();
		let thread = spawn(move || loop {
			// Acquire the lock, check the files to compile, get the file to compile and then drop the lock
			let (filename, realname) = {
				let mut files = files.lock().unwrap();
				if files.is_empty() {
					break;
				}
				files.pop().unwrap()
			};
			let code = match compile_file(&filename, filename.clone(), 2) {
				Ok(t) => t,
				Err(e) => {
					*errored.lock().unwrap() += 1;
					println!("Error: {}", e);
					continue;
				}
			};
			add_to_output(&format_clue!(
				"\t[\"",
				realname.strip_suffix(".clue").unwrap(),
				"\"] = function()\n",
				code,
				"\n\tend,\n"
			));
		});
		threads.push(thread);
	}
	for thread in threads {
		thread.join().unwrap();
	}
	let errored = *errored.lock().unwrap();
	match errored {
		0 => Ok(()),
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

#[cfg(not(feature = "devtimer"))]
fn main() -> Result<(), String> {
	let cli = Cli::parse();
	if cli.license {
		println!(include_str!("../../LICENSE"));
		return Ok(());
	} else if cli.types.is_some() {
		//TEMPORARY PLACEHOLDER UNTIL 4.0
		return Err(String::from("Type checking is not supported yet!"));
	}
	ENV_DATA.write().expect("Can't lock env_data").set_data(
		cli.tokens,
		cli.r#struct,
		cli.output,
		cli.jitbit,
		cli.r#continue,
		cli.rawsetglobals,
		cli.debug,
		//cli.types,
		//cli.std,
	);
	if let Some(bit) = &flag!(env_jitbit) {
		add_to_output(&format!("local {bit} = require(\"bit\");\n"));
	}
	/*if flag!(env_types) != TypesMode::NONE {
		*check!(LUA_G.write()) = match flag!(env_std) {
			LuaSTD::LUA54 => Some(AHashMap::from_iter([(String::from("print"), LuaType::NIL)])), //PLACEHOLDER
			_ => Some(AHashMap::default()),
		};
	}*/
	let codepath = cli.path.unwrap();
	if cli.pathiscode {
		let code = compile_code(codepath, String::from("(command line)"), 0)?;
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
		add_to_output("--STATICS\n");
		compile_folder(&codepath, String::new())?;
		let output = ENV_DATA
			.read()
			.expect("Can't lock env_data")
			.output_code()
			.to_string();
		let (statics, output) = output.rsplit_once("--STATICS").unwrap();
		ENV_DATA
			.write()
			.expect("Can't lock env_data")
			.rewrite_output_code(match cli.base {
				Some(filename) => {
					let base = match fs::read(filename) {
						Ok(base) => base,
						Err(_) => return Err(String::from("The given custom base was not found!")),
					};
					check!(std::str::from_utf8(&base))
						.to_string()
						.replace("--STATICS\n", statics)
						.replace('§', output)
				}
				None => include_str!("base.lua")
					.replace("--STATICS\n", statics)
					.replace('§', output),
			});
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
			check!(fs::write(
				&compiledname,
				ENV_DATA.read().expect("Can't lock env_data").output_code()
			))
		}
	} else if path.is_file() {
		let code = compile_file(
			&codepath,
			path.file_name().unwrap().to_string_lossy().into_owned(),
			0,
		)?;
		add_to_output(&code);
		if !cli.dontsave {
			compiledname = String::from(path.display().to_string().strip_suffix(".clue").unwrap()) + ".lua";
			check!(fs::write(
				&compiledname,
				ENV_DATA.read().expect("Can't lock env_data").output_code()
			))
		}
	} else {
		return Err(String::from("The given path doesn't exist"));
	}
	let output = ENV_DATA.read().expect("Can't lock env_data");
	if flag!(env_debug) {
		let newoutput = format!(include_str!("debug.lua"), output.output_code());
		check!(fs::write(compiledname, &newoutput));
		#[cfg(feature = "mlua")]
		if cli.execute {
			execute_lua_code(&newoutput)
		}
	} else {
		#[cfg(feature = "mlua")]
		if cli.execute {
			execute_lua_code(output.output_code())
		}
	}
	Ok(())
}

#[cfg(feature = "devtimer")]
fn compile_multi_files_bench(files: Vec<String>) {
	let threads_count = min(files.len(), num_cpus::get() * 2);
	let files = Arc::new(Mutex::new(files));
	let mut threads = Vec::with_capacity(threads_count);

	for _ in 0..threads_count {
		// this `.clone()` is used to create a new pointer to the outside `files`
		// that can be used from inside the newly created thread
		let files = files.clone();

		let thread = spawn(move || loop {
			let file: String;

			// Acquire the lock, check the files to compile, get the file to compile and then drop the lock
			{
				let mut files = files.lock().unwrap();

				if files.is_empty() {
					break;
				}

				file = files.pop().unwrap();
			}

			compile_code(file, String::new(), 2).unwrap();
		});

		threads.push(thread);
	}

	for thread in threads {
		thread.join().unwrap();
	}
}

#[cfg(feature = "devtimer")]
fn run_benchmark(iters: usize, func: impl Fn()) -> Vec<u128> {
	let mut timer = devtimer::SimpleTimer::new();
	let mut results = Vec::with_capacity(iters);

	for i in 0..iters {
		std::println!("Running iter {} ...", i + 1);
		timer.start();
		func();
		timer.stop();
		results.push(timer.time_in_nanos().unwrap());
	}

	results.sort();

	results
}

#[cfg(feature = "devtimer")]
fn main() {
	let files = check_for_files("examples/", String::new())
		.expect("Unexpected error happened in checking for files to compile")
		.iter()
		.map(|file| fs::read_to_string(file.0.clone()).unwrap())
		.collect::<Vec<String>>();

	let bench_results = run_benchmark(10000, || compile_multi_files_bench(files.clone()));

	let mut avg = 0;
	let mut top_avg = 0;
	let top_len = bench_results.len() as f64 * 0.01;

	for (i, value) in bench_results.iter().enumerate() {
		if (i as f64) <= top_len {
			top_avg += value;
		}
		avg += value;
	}

	avg /= bench_results.len() as u128;
	top_avg /= top_len as u128;

	std::println!();
	std::println!("Slowest: {} ns", bench_results.last().unwrap());
	std::println!("Fastest: {} ns", bench_results.first().unwrap());
	std::println!("Average: {} ns/iter", avg);
	std::println!("Top 1% : {} ns/iter", top_avg);
}

#[cfg(test)]
mod test {
	use crate::compile_folder;

	#[test]
	fn compilation_success() {
		compile_folder("../examples/", String::new()).unwrap();
	}
}
