#![cfg(feature = "lsp")]

use crate::code::Position;
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

fn hash_variable(string: &str, start: Position, end: Position) -> u64 {
	let mut hasher = DefaultHasher::new();
	string.hash(&mut hasher);
	start.hash(&mut hasher);
	end.hash(&mut hasher);
	hasher.finish()
}

pub fn send_definition(
	token: &str,
	value: String,
	location: Range<impl Into<Position>>,
	kind: SymbolKind
) {
	let start = location.start.into();
	let end = location.end.into();
	println!(
		"DEFINITION {}",
		json!({
			"id": hash_variable(token, start, end),
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
			"kind": kind
		})
	)
}

#[cfg(test)]
mod tests {
    use crate::lsp::hash_variable;

	#[test]
	fn check_hash() {
		assert!(
			hash_variable("test_string", (1, 1), (1, 1)) == hash_variable("test_string", (1, 1), (1, 1)),
			"hasing the same variable twice gave different results!"
		);
		assert!(
			hash_variable("test_string", (1, 1), (1, 1)) != hash_variable("test_string", (1, 1), (1, 3)),
			"hasing the same string twice with different positions gave the same result!"
		);
		assert!(
			hash_variable("test_string", (1, 1), (1, 1)) != hash_variable("test_strinh", (2, 2), (2, 2)),
			"somehow hashing 2 different variables gave the same result!"
		);
	}
}