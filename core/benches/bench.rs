use ahash::AHashMap;
use criterion::{criterion_group, criterion_main, Criterion};
use clue_core as clue;
use clue::{
	compiler::*,
	parser::*,
	preprocessor::*,
	scanner::*,
	env::Options
};
use std::{
	cmp::min,
	sync::{Arc, Mutex},
	thread,
	ffi::OsStr,
	fmt::Display,
	fs,
	path::Path,
};

fn compile_code(
	mut codes: Vec<(Code, bool)>,
	variables: &PPVars,
	name: &String,
	scope: usize,
	options: &Options,
) -> Result<(String, String), String> {
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
	let (ctokens, statics) = parse_tokens(tokens, name, options)?;
	let code = Compiler::new(options).compile_tokens(scope, ctokens);
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

fn compile_multi_files_bench(files: Vec<(String, String)>) {
	let threads_count = min(files.len(), num_cpus::get() * 2);
	let codes = Arc::new(Mutex::new(Vec::with_capacity(files.len())));
	let files = Arc::new(Mutex::new(files));
	let variables = Arc::new(Mutex::new(Vec::new()));
	let mut threads = Vec::with_capacity(threads_count);
	for _ in 0..threads_count {
		let files = files.clone();
		let codes = codes.clone();
		let variables = variables.clone();
		let thread = thread::spawn(move || loop {
			let (filename, realname) = {
				let mut files = files.lock().unwrap();
				if files.is_empty() {
					break;
				}
				files.pop().unwrap()
			};
			let (file_codes, file_variables) = read_file(&filename, &filename).unwrap();
			codes.lock().unwrap().push((file_codes, realname));
			variables.lock().unwrap().push(file_variables);
		});
		threads.push(thread);
	}
	for thread in threads {
		thread.join().unwrap();
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
		let codes = codes.clone();
		let variables = variables.clone();
		let thread = thread::spawn(move || loop {
			let (codes, realname) = {
				let mut codes = codes.lock().unwrap();
				if codes.is_empty() {
					break;
				}
				codes.pop().unwrap()
			};
			compile_code(codes, &variables, &realname, 2, &Options::default()).unwrap();
		});
		threads.push(thread);
	}
	for thread in threads {
		thread.join().unwrap();
	}
}

fn benchmark(c: &mut Criterion) {
	let files = check_for_files(
		env!("CARGO_MANIFEST_DIR").to_owned() + "/../" + "examples/",
		String::new(),
	)
	.expect("Unexpected error happened in checking for files to compile");

	c.bench_function("compile_multi_files_bench", |b| {
		b.iter(|| compile_multi_files_bench(files.clone()))
	});
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
