use ahash::AHashMap;
use clue::{code::*, compiler::*, env::Options, parser::*, preprocessor::*, scanner::*};
use clue_core as clue;
use criterion::{criterion_group, criterion_main, Criterion};
use crossbeam_queue::SegQueue;
use flume::Sender;
use std::thread::JoinHandle;
use std::{cmp::min, ffi::OsStr, fmt::Display, fs, path::Path, sync::Arc, thread};

struct PreprocessorAnalyzerData {
	pub codes: (Vec<(Code, bool)>, String),
	pub variables: PPVars,
}

struct ThreadData {
	pub output: String,
}

fn wait_threads(threads: Vec<JoinHandle<()>>) {
	for thread in threads {
		thread.join().unwrap();
	}
}

fn lock_and_pop<A, B>(mutex: Arc<SegQueue<(A, B)>>) -> Option<(A, B)> {
	if mutex.is_empty() {
		return None;
	}

	Some(mutex.pop().unwrap())
}

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

fn analyze_and_compile(
	tx: Sender<ThreadData>,
	options: &Options,
	codes: Arc<SegQueue<(Vec<(Code, bool)>, String)>>,
	variables: &Arc<AHashMap<Code, PPVar>>,
) {
	loop {
		let (codes, realname) = match lock_and_pop(codes.clone()) {
			None => break,
			Some((codes, realname)) => (codes, realname),
		};

		let (code, _static_vars) = match compile_code(codes, &variables, &realname, 2, &options) {
			Ok(t) => t,
			Err(e) => {
				tx.send(ThreadData {
					output: "".to_string(),
				})
				.unwrap();
				println!("Error: {}", e);
				continue;
			}
		};

		tx.send(ThreadData { output: code }).unwrap();
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
					codes: Default::default(),
					variables: Default::default(),
				})
				.unwrap();
				println!("Error: {}", e);
				continue;
			}
		};

		tx.send(PreprocessorAnalyzerData {
			codes: (file_codes, realname),
			variables: file_variables,
		})
		.unwrap();
	}
}

fn check_for_files<P: AsRef<Path>>(
	path: P,
	rpath: String,
) -> Result<SegQueue<(String, String)>, std::io::Error>
where
	P: AsRef<OsStr> + Display,
{
	let files = SegQueue::new();
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
			let inside_files = check_for_files(filepath_name, realname + ".")?;
			let _ = inside_files
				.into_iter()
				.map(|file| files.push(file.to_owned()));
		} else if filepath_name.ends_with(".clue") {
			files.push((filepath_name, realname));
		}
	}
	Ok(files)
}

fn compile_folder(files: Arc<SegQueue<(String, String)>>) {
	let files_len = files.len();
	let threads_count = min(files_len, num_cpus::get() * 2);
	let codes = SegQueue::new();
	let mut variables = Vec::with_capacity(64);
	let mut output = String::with_capacity(files_len * 512) + "\n";

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
		variables.push(data.variables);
		codes.push(data.codes);
	}

	let variables = Arc::new(
		variables
			.into_iter()
			.flat_map(|file_variables| file_variables.to_owned())
			.collect::<AHashMap<Code, PPVar>>(),
	);

	let mut threads = Vec::with_capacity(threads_count);
	let (tx, rx) = flume::unbounded();
	let codes = Arc::new(codes);

	for _ in 0..threads_count {
		let tx = tx.clone();
		let codes = codes.clone();
		let variables = variables.clone();

		let thread =
			thread::spawn(move || analyze_and_compile(tx, &Options::default(), codes, &variables));

		threads.push(thread);
	}

	wait_threads(threads);

	while let Ok(data) = rx.try_recv() {
		output += &data.output;
	}
}

fn benchmark(c: &mut Criterion) {
	let files = Arc::new(
		check_for_files(
			env!("CARGO_MANIFEST_DIR").to_owned() + "/../" + "examples/",
			String::new(),
		)
		.expect("Unexpected error happened in checking for files to compile"),
	);

	c.bench_function("compile_multi_files_bench", |b| {
		b.iter(|| compile_folder(files.clone()))
	});
}

criterion_group!(benches, benchmark);
criterion_main!(benches);
