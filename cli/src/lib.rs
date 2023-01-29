use clue_core::preprocessor::{PPVars, PPCode};
use crossbeam_queue::SegQueue;
use std::ffi::OsStr;
use std::fmt::Display;
use std::fs;
use std::path::Path;
use std::thread::JoinHandle;

pub type CodeQueue = SegQueue<(PPCode, String)>;

pub struct PreprocessorAnalyzerData {
	pub errored: bool,
	pub codes: (PPCode, String),
	pub variables: PPVars,
}

pub struct ThreadData {
	pub errored: bool,
	pub output: String,
	pub static_vars: String,
}

pub fn check_for_files<P: AsRef<Path>>(
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

pub fn wait_threads(threads: Vec<JoinHandle<()>>) {
	for thread in threads {
		thread.join().expect("Join shouldn't panic");
	}
}
