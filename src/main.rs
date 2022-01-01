#![allow(non_snake_case)]

use std::io;
use std::env;
use std::fs;
use std::path::Path;
use std::process;

fn error(msg: &str) {
	println!("{}", &msg);
	process::exit(1)
}

fn CompileFolder(path: String) {
	for entry in fs::read_dir(path) {

	}
}

fn main() {
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
	if !Path::new(&codepath).is_dir() {error("Given path is invalid!");}
	fs::create_dir(codepath.clone() + "\\.clue")
		.expect("Failed to create output directory!");
		CompileFolder(codepath);
}