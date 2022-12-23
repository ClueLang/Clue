use ahash::AHashMap;
use clue_core::{
	compiler::compile_tokens,
	parser::parse_tokens,
	preprocessor::{preprocess_code, to_preprocess},
	scanner::{scan_code, Token},
};
use criterion::{criterion_group, criterion_main, Criterion};
use std::env;
use std::{
	cmp::min,
	ffi::OsStr,
	fmt::Display,
	fs,
	path::Path,
	sync::{Arc, Mutex},
	thread::spawn,
};

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

fn compile_code(mut code: String, name: String, scope: usize) -> Result<String, String> {
	if to_preprocess(&code) {
		code = preprocess_code(code, None, AHashMap::new(), &mut 1usize, &name)?
			.0
			.iter()
			.collect();
	}
	let tokens: Vec<Token> = scan_code(code, name.clone())?;

	let ctokens = parse_tokens(
		tokens,
		/*if flag!(env_types) != TypesMode::NONE {
			Some(AHashMap::default())
		} else {
			None
		},*/
		name.clone(),
	)?;

	let code = compile_tokens(scope, ctokens);
	Ok(code)
}

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

fn benchmark(c: &mut Criterion) {
	let files = check_for_files(
		env!("CARGO_MANIFEST_DIR").to_owned() + "/../" + "examples/",
		String::new(),
	)
	.expect("Unexpected error happened in checking for files to compile")
	.iter()
	.map(|file| fs::read_to_string(file.0.clone()).unwrap())
	.collect::<Vec<String>>();

	c.bench_function("compile_multi_files_bench", |b| {
		b.iter(|| compile_multi_files_bench(files.clone()))
	});
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
