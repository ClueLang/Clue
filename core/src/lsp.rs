#![cfg(feature = "lsp")]

use crate::code::Position;
use colored::ColoredString;
use serde::Serialize;
use serde_json::{json, Value};
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

fn hash_variable(token: &str, location: &Range<Position>, filename: &str) -> u64 {
	let mut hasher = DefaultHasher::new();
	token.hash(&mut hasher);
	location.start.hash(&mut hasher);
	location.end.hash(&mut hasher);
	filename.hash(&mut hasher);
	hasher.finish()
}

fn make_location(filename: &str, location: &Range<Position>) -> Value {
	json!({
		"file": filename,
		"start": {
			"line": location.start.0,
			"column": location.start.1,
		},
		"end": {
			"line": location.end.0,
			"column": location.end.1,
		}
	})
}

pub fn send_definition(
	token: &str,
	value: String,
	location: &Range<Position>,
	filename: &str,
	kind: SymbolKind
) {
	println!(
		"DEFINITION {}",
		json!({
			"id": hash_variable(token, location, filename),
			"token": token,
			"value": value,
			"location": make_location(filename, location),
			"kind": kind
		})
	)
}
/*
path: '',
        line: 0,
        character: 0,
        message: '',
*/
#[inline]
pub fn make_error_string(
	kind: &ColoredString,
	message: &String,
	filename: &str,
	location: Range<Position>
) -> String {
	format!(
		"{} {}",
		kind.to_uppercase(),
		json!({
			"message": message,
			"location": make_location(filename, &location)
		})
	)
}

#[cfg(test)]
mod tests {
    use crate::lsp::hash_variable;

	#[test]
	fn check_hash() {
		assert!(
			hash_variable("test_string", &((1, 1)..(1, 1)), "test.clue") ==
			hash_variable("test_string", &((1, 1)..(1, 1)), "test.clue"),
			"hasing the same variable twice gave different results!"
		);
		assert!(
			hash_variable("test_string", &((1, 1)..(1, 1)), "test.clue") !=
			hash_variable("test_strinh", &((1, 1)..(1, 1)), "test.clue"),
			"hashing 2 similar variables with different tokens gave the same result!"
		);
		assert!(
			hash_variable("test_string", &((1, 1)..(1, 1)), "test.clue") !=
			hash_variable("test_string", &((1, 1)..(1, 2)), "test.clue"),
			"hashing 2 similar variables with different positions gave the same result!"
		);
		assert!(
			hash_variable("test_string", &((1, 1)..(1, 1)), "test.clue") !=
			hash_variable("test_string", &((1, 1)..(1, 1)), "tesu.clue"),
			"hashing 2 similar variables with different filenames gave the same result!"
		);
	}
}