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

fn hash_variable(token: &str, start: Position, end: Position, filename: &str) -> u64 {
	let mut hasher = DefaultHasher::new();
	token.hash(&mut hasher);
	start.hash(&mut hasher);
	end.hash(&mut hasher);
	filename.hash(&mut hasher);
	hasher.finish()
}

pub fn send_definition(
	token: &str,
	value: String,
	location: &Range<Position>,
	filename: &String,
	kind: SymbolKind
) {
	let start = location.start;
	let end = location.end;
	println!(
		"DEFINITION {}",
		json!({
			"id": hash_variable(token, start, end, filename),
			"token": token,
			"value": value,
			"location": {
				"file": filename,
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
			hash_variable("test_string", (1, 1), (1, 1), "test.clue") ==
			hash_variable("test_string", (1, 1), (1, 1), "test.clue"),
			"hasing the same variable twice gave different results!"
		);
		assert!(
			hash_variable("test_string", (1, 1), (1, 1), "test.clue") !=
			hash_variable("test_strinh", (1, 1), (1, 1), "test.clue"),
			"hashing 2 similar variables with different tokens gave the same result!"
		);
		assert!(
			hash_variable("test_string", (1, 1), (1, 1), "test.clue") !=
			hash_variable("test_string", (1, 1), (1, 2), "test.clue"),
			"hashing 2 similar variables with different positions gave the same result!"
		);
		assert!(
			hash_variable("test_string", (1, 1), (1, 1), "test.clue") !=
			hash_variable("test_string", (1, 1), (1, 1), "tesu.clue"),
			"hashing 2 similar variables with different filenames gave the same result!"
		);
	}
}