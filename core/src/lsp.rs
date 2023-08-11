#![cfg(feature = "lsp")]

use serde::Serialize;
use serde_json::json;
use std::{
	collections::hash_map::DefaultHasher,
	hash::{Hash, Hasher},
	ops::Range,
};

#[derive(Serialize)]
pub enum SymbolKind {
	VARIABLE,
	FUNCTION,
	PSEUDO,
	ENUM,
	CONSTANT,
	MACRO,
	ARGUMENT
}

#[derive(Serialize)]
pub enum SymbolModifier {
	LOCAL, GLOBAL, STATIC
}

fn hash_string(string: &str) -> u64 {
	let mut hasher = DefaultHasher::new();
	string.hash(&mut hasher);
	hasher.finish()
}

pub fn send_definition(
	token: &str,
	value: String,
	location: Range<impl Into<(usize, usize)>>,
	kind: SymbolKind,
	modifiers: &[SymbolModifier],
) {
	let start = location.start.into();
	let end = location.end.into();
	println!(
		"DEFINITION {}",
		json!({
			"id": hash_string(token),
			"token": token,
			"value": value,
			"location": {
				"start": {
					"line": start.0,
					"column": start.1,
				},
				"end": {
					"line": end.0,
					"column": end.1,
				}
			},
			"kind": kind,
			"modifiers": modifiers
		})
	)
}

#[cfg(test)]
mod tests {
    use crate::lsp::hash_string;

	#[test]
	fn check_hash() {
		assert!(
			hash_string("test_string") == hash_string("test_string"),
			"hasing the same string twice gave different results!"
		);
		assert!(
			hash_string("test_string") != hash_string("test_strinh"),
			"somehow hashing 2 different strings gave the same result!"
		);
	}
}