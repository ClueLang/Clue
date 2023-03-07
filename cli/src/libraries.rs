use clap::Args;
use clue_core::check;
use curl::easy::Easy;

#[derive(Args)]
pub struct InstallArgs {

}

fn curl(url: &str) -> Result<String, String> {
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