#![cfg(feature = "lsp")]

use serde::Serialize;
use serde_json::json;
use std::{
	collections::hash_map::DefaultHasher,
	hash::{Hash, Hasher},
	ops::Range,
};

//BACKPORTED FROM BETTER_ERRORS
pub struct TokenPosition {
	pub line: usize,
	pub column: usize,
	//pub index: usize,
}

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

pub fn send_symbol(
	token: &str,
	value: String,
	location: Range<TokenPosition>,
	kind: SymbolKind,
	modifiers: &[SymbolModifier],
) {
	println!(
		"DEFINITION {}",
		json!({
			"id": hash_string(token),
			"token": token,
			"value": value,
			"location": {
				"start": {
					"line": location.start.line,
					"column": location.start.column,
				},
				"end": {
					"line": location.end.line,
					"column": location.end.column,
				}
			},
			"kind": kind,
			"modifiers": modifiers
		})
	)
}