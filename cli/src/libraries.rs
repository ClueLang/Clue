use std::{env, fs, io, process::Command, path::PathBuf};
use clap::Args;

#[derive(Args)]
pub struct InstallArgs {
	url: String,
}

fn attempt_installation(args: InstallArgs, temp_dir: &mut PathBuf) -> io::Result<()> {
	println!("Reading Clue.toml...");
	temp_dir.push("Clue.toml");
	let txt = fs::read_to_string(temp_dir.as_path())?;
	Ok(())
}

pub fn install(args: InstallArgs) -> io::Result<()> {
	let mut path = fs::canonicalize(env::current_exe()?)?;
	path.pop();
	println!("Cloning repository...");
	let mut temp_dir = env::temp_dir();
	temp_dir.push("clue");
	let git = Command::new("git")
		.args(["clone", &args.url, temp_dir.to_str().unwrap(), "--single-branch"])
		.spawn()?
		.wait()?;
	if git.success() {
		let success = attempt_installation(args, &mut temp_dir);
		if success.is_err() {
			println!("An error happened during installation!");
		}
		println!("Cleaning up...");
		temp_dir.pop();
		fs::remove_dir_all(temp_dir.as_path())?;
		success
	} else {
		Err(io::Error::new(io::ErrorKind::Other, "Failed to clone repository!"))
	}
}
