extern crate proc_macro;

#[cfg(feature = "map")]
extern crate litrs;

#[cfg(feature = "version")]
extern crate toml;

#[cfg(any(feature = "map", feature = "version"))]
use proc_macro::TokenStream;

#[cfg(feature = "map")]
#[proc_macro]
pub fn generate_map(item: TokenStream) -> TokenStream {
	use proc_macro::{TokenTree::*, Delimiter::*};
	let array = item.into_iter().collect::<Vec<_>>().pop().expect("You must pass an array!");
	if let Group(array) = array {
		if array.delimiter() != Bracket {
			panic!("The passed argument must be an array!");
		}
		println!("{array:#?}");
	} else {
		panic!("The passed argument must be an array!");
	}
	"".parse().unwrap()
}

#[cfg(feature = "version")]
#[proc_macro]
pub fn clue_version(_item: TokenStream) -> TokenStream {
	use toml::Value;
	format!(
		"\"{}\"",
		include_str!("../../core/Cargo.toml")
			.parse::<Value>().unwrap()["package"]
			.as_table().unwrap()["version"]
			.as_str().unwrap()
	).parse().unwrap()
}