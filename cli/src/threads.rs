use clue_core::env::Options;
use clue_core::errors::print_errors;
use clue_core::preprocessor::{read_file, PPCode, PPVars};
use clue_core::{check, format_clue};
use crossbeam_queue::SegQueue;
use flume::Sender;
use std::cmp;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;
use std::thread::{self, JoinHandle};
use colored::*;

use crate::compile_code;

type CodeQueue = SegQueue<(PPCode, String, String)>;

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

fn check_for_files(
	path: PathBuf,
	rpath: String,
) -> Result<SegQueue<(PathBuf, String)>, std::io::Error> {
	let files = SegQueue::new();
	for entry in fs::read_dir(&path)? {
		let name = entry?
			.path()
			.file_name()
			.unwrap()
			.to_string_lossy()
			.into_owned();
		let filepath = path.join(&name);
		let realname = rpath.clone() + &name;
		if filepath.is_dir() {
			for file in check_for_files(filepath, realname + ".")? {
				files.push(file)
			}
		} else if filepath.extension().map_or(false, |extension| extension == "clue") {
			files.push((filepath, realname));
		}
	}
	Ok(files)
}

fn wait_threads(threads: Vec<JoinHandle<()>>) {
	for thread in threads {
		thread.join().expect("Join shouldn't panic");
	}
}

pub fn compile_folder(
	file_path: impl Into<PathBuf>,
	rpath: String,
	options: Options,
) -> Result<(String, String), String> {
	let files = check!(check_for_files(file_path.into(), rpath));
	let files_len = files.len();
	let threads_count = cmp::min(files_len, num_cpus::get() * 2);
	let codes = SegQueue::new();
	let files = Arc::new(files);
	let options = Arc::new(options);
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

		let thread = thread::spawn(move || preprocess_file_dir(tx, &options, files));

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

	let variables = Arc::new(
		variables
			.into_iter()
			.flatten()
			.collect::<PPVars>(),
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
	tx: Sender<PreprocessorAnalyzerData>,
	options: &Options,
	files: Arc<SegQueue<(PathBuf, String)>>,
) {
	loop {
		let (filename, filepath, realname) = match files.pop() {
			None => break,
			Some((filepath, realname)) => (
				filepath.to_string_lossy().into_owned(),
				filepath,
				realname
			),
		};

		let (file_codes, file_variables) = match read_file(filepath, &filename, options) {
			Ok(t) => t,
			Err(e) => {
				tx.send(PreprocessorAnalyzerData {
					errored: true,
					codes: Default::default(),
					variables: Default::default(),
				}).unwrap();
				#[cfg(feature = "lsp")]
				print_errors(options.env_symbols);
				#[cfg(not(feature = "lsp"))]
				print_errors();
				eprintln!("{}: {e}", "Error".red().bold());
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
	variables: Arc<PPVars>,
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
				}).unwrap();
				#[cfg(feature = "lsp")]
				print_errors(options.env_symbols);
				#[cfg(not(feature = "lsp"))]
				print_errors();
				eprintln!("{}: {e}", "Error".red().bold());
				continue;
			}
		};

		let string = format_clue!(
			"\t[\"",
			realname.strip_suffix(".clue").unwrap(),
			"\"] = function(...)\n",
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
