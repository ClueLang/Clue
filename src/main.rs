#![allow(non_snake_case)]

use std::io;
use std::env;
use std::fs;
use std::path::Path;

//Version 5 Alpha 0.0

fn IterateFolder(path: &Path, func: &dyn Fn(fs::DirEntry) -> ()) -> io::Result<()> {
	for entry in fs::read_dir(path)? {
		let entry = entry?;
		func(entry);
	}
	Ok(())
}

fn CompileFile(entry: fs::DirEntry) {
	//todo
}

fn CompileFolder(path: &Path) -> Result<(), String> {
	let result = IterateFolder(path, &CompileFile);
	if result.is_err() {
		return Err(match result.err() {
			Some(error) => error.to_string(),
			None => String::new()
		})
	}
	Ok(())
}

fn main() -> Result<(), String> {
	let args: Vec<String> = env::args().collect();
	let mut codepath: String = String::new();
	if args.len() == 1 {
		println!("Please insert path to code directory: ");
		io::stdin()
			.read_line(&mut codepath)
			.expect("Failed to read path!");
	} else {
		codepath = args[1].clone();
	}
	if !Path::new(&codepath).is_dir() {
		return Err(String::from("The given path doesn't exist"));
	}
	if Path::new(&(codepath.clone() + "\\.clue")).is_dir() {
		fs::remove_dir_all(Path::new(&(codepath.clone() + "\\.clue")))
			.expect("Faied to remove previous output directory!");
	}
	fs::create_dir_all(Path::new(&(codepath.clone() + "\\.clue")))
		.expect("Failed to create output directory!");
	CompileFolder(Path::new(&codepath))?;
	Ok(())
}