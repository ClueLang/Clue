#![allow(non_snake_case)]

use std::io;
use std::env;
use std::fs;
use std::path::Path;

//Version 6 Alpha 0.0

/*fn CompileFile(entry: fs::DirEntry) {
	println!("{}", entry.path().file_name().unwrap().to_string_lossy().into_owned());
	//todo
}*/

/*fn IterateFolder(path: &Path) -> io::Result<()> {
	for entry in fs::read_dir(path)? {
		let entry = entry?;
		func(entry);
	}
	Ok(())
}*/

macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string())
		}
	};
}

fn CompileFolder(path: &Path) -> Result<(), String> {
	for entry in check!(fs::read_dir(path)) {
		let entry = check!(entry);
		let name: String = entry.path().file_name().unwrap().to_string_lossy().into_owned();
		if name != ".clue" {
			let path = Path::new("");
		}
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
	let path: &Path = Path::new(&codepath);
	if !path.is_dir() {
		return Err(String::from("The given path doesn't exist"));
	}
	let outputname: String = codepath.clone() + "\\.clue";
	let outputpath: &Path = Path::new(&outputname);
	if outputpath.is_dir() {
		check!(fs::remove_dir_all(outputpath));
	}
	check!(fs::create_dir_all(outputpath));
	CompileFolder(path)?;
	Ok(())
}