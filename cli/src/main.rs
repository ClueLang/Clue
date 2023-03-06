use clap::{Parser, Subcommand};
use builder::*;
use clue_core::check;
use curl::easy::Easy;
use updater::*;

mod threads;
mod builder;
mod updater;

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

	/// Update the compiler
	Update(UpdateOptions)
}

pub fn curl(url: &str) -> Result<String, String> {
	let mut handle = Easy::new();
	let mut output = Vec::new();
	check!(handle.url(url));
	{
		let mut transfer = handle.transfer();
		check!(transfer.write_function(|data| {
			output.extend_from_slice(data);
			Ok(data.len())
		}));
		check!(transfer.perform());
	}
	Ok(check!(String::from_utf8(output)))
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
		Update(args) => update(args),
	}
}

#[cfg(test)]
mod test {
	use clue_core::env::Options;

	use crate::threads::compile_folder;

	#[test]
	fn compilation_success() {
		compile_folder("../examples/", String::new(), Options::default()).unwrap();
	}
}
