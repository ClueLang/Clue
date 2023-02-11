use ahash::AHashMap;
use clue_core::{check, format_clue};
use clue_core::code::Code;
use clue_core::env::Options;
use clue_core::preprocessor::{PPVars, PPCode, PPVar, read_file};
use crossbeam_queue::SegQueue;
use flume::Sender;
use std::ffi::OsStr;
use std::fmt::Display;
use std::fs;
use std::path::Path;
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use std::cmp;

use crate::compile_code;

type CodeQueue<'a> = SegQueue<(PPCode, String, String)>;

struct PreprocessorAnalyzerData {
	errored: bool,
	codes: (PPCode, String, String),
	pub variables: PPVars,
}

struct ThreadData {
	errored: bool,
	output: String,
	static_vars: String,
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
			for file in check_for_files(filepath_name, realname + ".")? {
				files.push(file)
			}
		} else if filepath_name.ends_with(".clue") {
			files.push((filepath_name, realname));
		}
	}
	Ok(files)
}

fn wait_threads(threads: Vec<JoinHandle<()>>) {
	for thread in threads {
		thread.join().expect("Join shouldn't panic");
	}
}

pub fn compile_folder<P: AsRef<Path>>(
	file_path: P,
	rpath: String,
	options: &Options,
) -> Result<(String, String), String>
where
	P: AsRef<OsStr> + Display,
{
	let files = check!(check_for_files(file_path, rpath));
	let files_len = files.len();
	let threads_count = cmp::min(files_len, num_cpus::get() * 2);
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
		let options = options.clone();

		let thread = thread::spawn(move || preprocess_file_dir(files, tx, &options));

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

		let thread = thread::spawn(move || compile_file_dir(tx, &options, codes, variables));

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

fn preprocess_file_dir(
	files: Arc<SegQueue<(String, String)>>,
	tx: Sender<PreprocessorAnalyzerData>,
	options: &Options,
) {
	loop {
		let (filename, realname) = match files.pop() {
			None => break,
			Some((filename, realname)) => (filename, realname),
		};

		let (file_codes, file_variables) = match read_file(&filename, &filename, options) {
			Ok(t) => t,
			Err(e) => {
				tx.send(PreprocessorAnalyzerData {
					errored: true,
					codes: Default::default(),
					variables: Default::default(),
				})
				.unwrap();
				println!("Error: {e}");
				continue;
			}
		};

		tx.send(PreprocessorAnalyzerData {
			errored: false,
			codes: (file_codes, filename, realname),
			variables: file_variables,
		})
		.unwrap();
	}
}

fn compile_file_dir(
	tx: Sender<ThreadData>,
	options: &Options,
	codes: Arc<CodeQueue>,
	variables: Arc<AHashMap<Code, PPVar>>,
) {
	loop {
		let (codes, filename, realname) = match codes.pop() {
			None => break,
			Some(codes) => codes,
		};

		let (code, static_vars) = match compile_code(codes, &variables, &filename, 2, options) {
			Ok(t) => t,
			Err(e) => {
				tx.send(ThreadData {
					errored: true,
					output: "".to_owned(),
					static_vars: "".to_owned(),
				})
				.unwrap();
				println!("Error: {e}");
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
