#[cfg(feature = "map")]
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
