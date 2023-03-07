use clap::{Parser, Subcommand};
use builder::*;
use libraries::InstallArgs;

mod threads;
mod builder;
mod libraries;

#[derive(Parser)]
#[clap(
	version,
	about = "C/Rust like programming language that compiles into Lua code\nMade by Maiori (https://github.com/ClueLang/Clue)",
	long_about = None
)]
#[clap(propagate_version = true)]
struct Cli {
	#[clap(subcommand)]
	command: Commands,
}

#[derive(Subcommand)]
enum Commands {
	/// Print licensing information
	License,

	/// Compile the given file/directory
	Build(BuildArgs),

	/// Install the given library
	Install(InstallArgs),
}

fn main() -> Result<(), String> {
	use Commands::*;
	let cli = Cli::parse();
	match cli.command {
		License => {
			print!(include_str!("../LICENSE"));
			Ok(())
		}
		Build(args) => build(args),
		Install(args) => todo!()
	}
}

#[cfg(test)]
mod tests {
	use clue_core::env::Options;
	use crate::threads::compile_folder;

	#[test]
	fn compilation_success() {
		compile_folder("../examples/", String::new(), Options::default()).unwrap();
	}
}
