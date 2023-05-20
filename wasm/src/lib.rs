//! The clue_wasm crate contains a WebAssembly and JavaScript compatible API for the [`clue_core`] crate
//! It exposes the same [`Clue`] struct as the [`clue_core`] crate (with methods related to files not included)
//! and the [`get_version`] function

use clue_core::{
	code::Code,
	env::{BitwiseMode, ContinueMode, LuaVersion},
	parser::Expression,
	scanner::Token,
	Clue as ClueCore,
};
use wasm_bindgen::prelude::*;

/// Returns the version of the `clue` crate.
#[wasm_bindgen(js_name = "getVersion")]
pub fn get_version() -> String {
	env!("CARGO_PKG_VERSION").to_string()
}

/// The Clue WebAssembly API.
/// uses the `clue_core` crate.
#[wasm_bindgen]
pub struct Clue {
	inner: ClueCore,
}

#[wasm_bindgen]
impl Clue {
	/// Create a new `Clue` instance.
	#[wasm_bindgen(constructor)]
	pub fn new() -> Self {
		Clue {
			inner: ClueCore::new(),
		}
	}

	/// Sets the `tokens` option
	/// If `tokens` is `true` then then the `tokens` option will be enabled
	/// If `tokens` is `false` then then the `tokens` option will be disabled
	pub fn tokens(&mut self, env_tokens: bool) {
		self.inner.tokens(env_tokens);
	}

	/// Sets the `struct` option
	/// If `struct` is `true` then then the `struct` option will be enabled
	/// If `struct` is `false` then then the `struct` option will be disabled
	#[wasm_bindgen(js_name = "envStruct")]
	pub fn env_struct(&mut self, env_tokens: bool) {
		self.inner.env_struct(env_tokens);
	}

	/// Sets the `bitwise_mode` option
	/// The `bitwise_mode` option is used to set the bitwise mode
	/// The `bitwise_mode` option can be set to `clue`, `library`, or `vanilla`
	///
	/// # Errors
	/// If the `bitwise_mode` option is set to an invalid value then an error will be thrown
	#[wasm_bindgen(js_name = "bitwiseMode")]
	pub fn bitwise_mode(&mut self, mode: String) -> Result<(), String> {
		self.inner.bitwise_mode(match mode.as_str() {
			"clue" => BitwiseMode::Clue,
			"library" => BitwiseMode::Library,
			"vanilla" => BitwiseMode::Vanilla,
			_ => return Err(format!("Invalid bitwise mode {}", mode)),
		});
		Ok(())
	}

	/// Sets the `continue_mode` option
	/// The `continue_mode` option is used to set the continue mode
	/// The `continue_mode` option can be set to `simple`, `luajit`, `goto`, or `moonscript`
	///
	/// # Errors
	/// If the `continue_mode` option is set to an invalid value then an error will be thrown
	#[wasm_bindgen(js_name = "continueMode")]
	pub fn continue_mode(&mut self, mode: String) -> Result<(), String> {
		self.inner.continue_mode(match mode.as_str() {
			"simple" => ContinueMode::Simple,
			"luajit" => ContinueMode::LuaJIT,
			"goto" => ContinueMode::Goto,
			"moonsrcipt" => ContinueMode::MoonScript,
			_ => return Err(format!("Invalid continue mode {}", mode)),
		});
		Ok(())
	}

	/// Sets the `rawsetglobals` option
	/// If `rawsetglobals` is `true` then then the `rawsetglobals` option will be enabled
	/// If `rawsetglobals` is `false` then then the `rawsetglobals` option will be disabled
	pub fn rawsetglobals(&mut self, env_rawsetglobal: bool) {
		self.inner.rawsetglobals(env_rawsetglobal);
	}

	/// Sets the `debug` option
	/// If `debug` is `true` then then the `debug` option will be enabled
	/// If `debug` is `false` then then the `debug` option will be disabled
	/// The `debug` option is used to enable debug mode
	pub fn debug(&mut self, env_debug: bool) {
		self.inner.debug(env_debug);
	}

	/// Sets the `output` option
	/// If `output` is `true` then then the `output` option will be enabled
	/// If `output` is `false` then then the `output` option will be disabled
	/// When the `output` option is enabled, the output will be printed to the console
	pub fn output(&mut self, env_output: bool) {
		self.inner.output(env_output);
	}

	/// Sets the `target` option
	/// The `target` option is used to set the target Lua version
	/// The `target` option can be set to `5.1`, `5.2`, `5.3`, `5.4`, `blua`, or `luajit`
	///
	/// # Errors
	/// If the `target` option is set to an invalid value then an error will be thrown
	pub fn target(&mut self, version: Option<String>) -> Result<(), String> {
		self.inner.target(match version.as_deref() {
			Some("5.1") => Some(LuaVersion::Lua51),
			Some("5.2") => Some(LuaVersion::Lua52),
			Some("5.3") => Some(LuaVersion::Lua53),
			Some("5.4") => Some(LuaVersion::Lua54),
			Some("blua") => Some(LuaVersion::BLUA),
			Some("luajit") => Some(LuaVersion::LuaJIT),
			None => None,
			Some(version) => return Err(format!("Invalid Lua version {}", version)),
		});
		Ok(())
	}

	/// Sets the `target_os` option
	/// The `target_os` option is used to set the target operating system
	/// The `target_os` option can be set to `windows`, `linux`, `macos`, `ios`, `android`, `freebsd`, `openbsd`, `netbsd`, `dragonfly`, `emscripten`, `wasi`, or `unknown`
	#[wasm_bindgen(js_name = "targetOs")]
	pub fn target_os(&mut self, os: String) {
		self.inner.target_os(os);
	}
}

#[wasm_bindgen]
impl Clue {
	/// Preprocesses the given code
	/// Takes a string of code and returns a preprocessed version of the code
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "preprocessCode")]
	pub fn preprocess_code(&self, code: String) -> Result<JsValue, String> {
		let preprocessed = self.inner.preprocess_code(code)?;
		serde_wasm_bindgen::to_value(&preprocessed).map_err(|err| err.to_string())
	}

	/// Scans the given preprocessed code
	/// Takes a Code object and returns an array of tokens
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "scanPreprocessed")]
	pub fn scan_preprocessed(&self, code: JsValue) -> Result<JsValue, String> {
		let code: Code = serde_wasm_bindgen::from_value(code).map_err(|err| err.to_string())?;
		let scanned = self.inner.scan_preprocessed(code)?;
		serde_wasm_bindgen::to_value(&scanned).map_err(|err| err.to_string())
	}

	/// Scans the given code
	/// Takes a string of code and returns an array of tokens
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "scanCode")]
	pub fn scan_code(&self, code: String) -> Result<JsValue, String> {
		let scanned = self.inner.scan_code(code)?;
		serde_wasm_bindgen::to_value(&scanned).map_err(|err| err.to_string())
	}

	/// Parses the given preprocessed code
	/// Takes a Code object and returns an Expression object
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "parsePreprocessed")]
	pub fn parse_preprocessed(&self, code: JsValue) -> Result<JsValue, String> {
		let code: Code = serde_wasm_bindgen::from_value(code).map_err(|err| err.to_string())?;
		let parsed = self.inner.parse_preprocessed(code)?;
		serde_wasm_bindgen::to_value(&parsed).map_err(|err| err.to_string())
	}

	/// Parses the given tokens
	/// Takes an array of tokens and returns an Expression object
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "parseTokens")]
	pub fn parse_tokens(&self, tokens: JsValue) -> Result<JsValue, String> {
		let tokens: Vec<Token> =
			serde_wasm_bindgen::from_value(tokens).map_err(|err| err.to_string())?;
		let parsed = self.inner.parse_tokens(tokens)?;
		serde_wasm_bindgen::to_value(&parsed).map_err(|err| err.to_string())
	}

	/// Parses the given code
	/// Takes a string of code and returns an Expression object
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "parseCode")]
	pub fn parse_code(&self, code: String) -> Result<JsValue, String> {
		let parsed = self.inner.parse_code(code)?;
		serde_wasm_bindgen::to_value(&parsed).map_err(|err| err.to_string())
	}

	/// Compiles the given preprocessed code
	/// Takes a Code object and returns a string of Lua code
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "compilePreprocessed")]
	pub fn compile_preprocessed(&self, code: JsValue) -> Result<String, String> {
		let code: Code = serde_wasm_bindgen::from_value(code).map_err(|err| err.to_string())?;

		self.inner.compile_preprocessed(code)
	}

	/// Compiles the given tokens
	/// Takes an array of tokens and returns a string of Lua code
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "compileTokens")]
	pub fn compile_tokens(&self, tokens: JsValue) -> Result<String, String> {
		let tokens: Vec<Token> =
			serde_wasm_bindgen::from_value(tokens).map_err(|err| err.to_string())?;

		self.inner.compile_tokens(tokens)
	}

	/// Compiles the given parse result
	/// Takes a tuple of an Expression object and the static variables and returns a string of Lua code
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "compileAst")]
	pub fn compile_ast(&self, parse_result: JsValue) -> Result<String, String> {
		let parse_result: (Expression, String) =
			serde_wasm_bindgen::from_value(parse_result).map_err(|err| err.to_string())?;

		self.inner.compile_ast(parse_result)
	}

	/// Compiles the given code
	/// Takes a string of code and returns a string of Lua code
	///
	/// # Errors
	/// If the code is invalid then an error will be thrown
	#[wasm_bindgen(js_name = "compileCode")]
	pub fn compile_code(&self, code: String) -> Result<String, String> {
		self.inner.compile_code(code)
	}
}

impl Default for Clue {
	fn default() -> Self {
		Self::new()
	}
}

#[cfg(test)]
mod tests {
	use super::*;
	use wasm_bindgen_test::*;

	#[wasm_bindgen_test]
	fn test_version() {
		assert_eq!(get_version(), env!("CARGO_PKG_VERSION"));
	}

	#[wasm_bindgen_test]
	fn test_compiles() {
		let clue = Clue::new();
		clue.compile_code(include_str!("../../examples/fizzbuzz.clue").to_owned())
			.unwrap();
	}
}
