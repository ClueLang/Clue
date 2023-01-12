use clue_core::code::Code;
use clue_core::preprocessor::PPVars;
use std::ffi::OsStr;
use std::fmt::Display;
use std::fs;
use std::path::Path;
use std::sync::{Arc, Mutex};
use std::thread::JoinHandle;

pub struct PreprocessorAnalyzerData {
	pub errored: bool,
	pub codes: (Vec<(Code, bool)>, String),
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

pub fn wait_threads(threads: Vec<JoinHandle<()>>) {
	for thread in threads {
		thread.join().unwrap();
	}
}

pub fn lock_and_pop<A, B>(mutex: Arc<Mutex<Vec<(A, B)>>>) -> Option<(A, B)> {
	let mut mutex = mutex.lock().unwrap();

	if mutex.is_empty() {
		return None;
	}

	Some(mutex.pop().unwrap())
}
