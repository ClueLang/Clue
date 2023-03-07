use std::{env, fs, io, process::Command, path::PathBuf, str::FromStr};
use semver::{VersionReq, Version};
use serde::Deserialize;
use clap::{Args, crate_version};

#[derive(Args)]
pub struct InstallArgs {
	/// URL to the library repository
	url: String,

	/// Install the specified branch of the repository
	#[clap(short, long)]
	branch: Option<String>
}

#[derive(Deserialize)]
pub struct Library {
	name: String,
	clue: String,
}

#[derive(Deserialize)]
pub struct Module {
	
}

fn attempt_installation(_args: InstallArgs, temp_dir: &mut PathBuf) -> io::Result<()> {
	use io::{Error, ErrorKind::{self, Other}};

	println!("Reading Clue.toml...");
	temp_dir.push("Clue.toml");
	let lib: Library = toml::from_str(&fs::read_to_string(temp_dir.as_path())?)
		.map_err(|e| {
			eprintln!("{e}");
			Error::new(ErrorKind::InvalidData, "Failed to read Clue.toml!")
		})?;
	println!("Library name: {}", lib.name);
	println!("Required Clue version: {}", lib.clue);
	match VersionReq::from_str(&format!(">={}", lib.clue)) {
		Ok(version_req) => {
			if !version_req.matches(&Version::from_str(crate_version!().split('-').next().unwrap()).unwrap()) {
				return Err(Error::new(
					Other,
					format!("'{}' requires a more recent version of Clue! ({})", lib.name, lib.clue)
				));
			}
		}
		Err(e) => return Err(Error::new(Other, e.to_string())),
	}
	Ok(())
}

pub fn install(args: InstallArgs) -> io::Result<()> {
	let mut path = fs::canonicalize(env::current_exe()?)?;
	path.pop();
	println!("Cloning repository...");
	let mut temp_dir = env::temp_dir();
	temp_dir.push("clue");
	let mut git_args = vec![
		String::from("clone"),
		args.url.clone(),
		temp_dir.to_string_lossy().to_string(),
		String::from("--single-branch")
	];
	if let Some(branch) = &args.branch {
		git_args.push(format!("--branch={branch}"));
	}
	if Command::new("git").args(git_args).spawn()?.wait()?.success() {
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
