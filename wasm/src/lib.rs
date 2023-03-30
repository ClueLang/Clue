use clue_core::{
	code::Code,
	env::{BitwiseMode, ContinueMode, LuaVersion},
	parser::Expression,
	scanner::Token,
	Clue as ClueCore,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Clue {
	inner: ClueCore,
}

#[wasm_bindgen]
impl Clue {
	#[wasm_bindgen(constructor)]
	pub fn new() -> Self {
		Clue {
			inner: ClueCore::new(),
		}
	}

	pub fn tokens(&mut self, env_tokens: bool) {
		self.inner.tokens(env_tokens);
	}

	pub fn env_struct(&mut self, env_tokens: bool) {
		self.inner.env_struct(env_tokens);
	}

	pub fn bitwise_mode(&mut self, mode: String) {
		self.inner.bitwise_mode(match mode.as_str() {
			"clue" => BitwiseMode::Clue,
			"library" => BitwiseMode::Library,
			"vanilla" => BitwiseMode::Vanilla,
			_ => panic!("Invalid bitwise mode {}", mode),
		});
	}

	pub fn continue_mode(&mut self, mode: String) {
		self.inner.continue_mode(match mode.as_str() {
			"simple" => ContinueMode::Simple,
			"luajit" => ContinueMode::LuaJIT,
			"goto" => ContinueMode::Goto,
			"moonsrcipt" => ContinueMode::MoonScript,
			_ => panic!("Invalid continue mode {}", mode),
		});
	}

	pub fn rawsetglobals(&mut self, env_rawsetglobal: bool) {
		self.inner.rawsetglobals(env_rawsetglobal);
	}

	pub fn debug(&mut self, env_debug: bool) {
		self.inner.debug(env_debug);
	}

	pub fn output(&mut self, env_output: bool) {
		self.inner.output(env_output);
	}

	pub fn target(&mut self, version: Option<String>) {
		self.inner
			.target(version.map(|version| match version.as_str() {
				"5.1" => LuaVersion::Lua51,
				"5.2" => LuaVersion::Lua52,
				"5.3" => LuaVersion::Lua53,
				"5.4" => LuaVersion::Lua54,
				"blua" => LuaVersion::BLUA,
				"luajit" => LuaVersion::LuaJIT,
				_ => panic!("Invalid Lua version {}", version),
			}));
	}

	pub fn target_os(&mut self, os: String) {
		self.inner.target_os(os);
	}
}

#[wasm_bindgen]
impl Clue {
	pub fn preprocess_code(&self, code: String) -> Result<JsValue, String> {
		let preprocessed = self.inner.preprocess_code(code)?;
		serde_wasm_bindgen::to_value(&preprocessed).map_err(|err| err.to_string())
	}

	pub fn scan_preprocessed(&self, code: JsValue) -> Result<JsValue, String> {
		let code: Code = serde_wasm_bindgen::from_value(code).map_err(|err| err.to_string())?;
		let scanned = self.inner.scan_preprocessed(code)?;
		serde_wasm_bindgen::to_value(&scanned).map_err(|err| err.to_string())
	}

	pub fn scan_code(&self, code: String) -> Result<JsValue, String> {
		let scanned = self.inner.scan_code(code)?;
		serde_wasm_bindgen::to_value(&scanned).map_err(|err| err.to_string())
	}

	pub fn parse_preprocessed(&self, code: JsValue) -> Result<JsValue, String> {
		let code: Code = serde_wasm_bindgen::from_value(code).map_err(|err| err.to_string())?;
		let parsed = self.inner.parse_preprocessed(code)?;
		serde_wasm_bindgen::to_value(&parsed).map_err(|err| err.to_string())
	}

	pub fn parse_tokens(&self, tokens: JsValue) -> Result<JsValue, String> {
		let tokens: Vec<Token> =
			serde_wasm_bindgen::from_value(tokens).map_err(|err| err.to_string())?;
		let parsed = self.inner.parse_tokens(tokens)?;
		serde_wasm_bindgen::to_value(&parsed).map_err(|err| err.to_string())
	}

	pub fn parse_code(&self, code: String) -> Result<JsValue, String> {
		let parsed = self.inner.parse_code(code)?;
		serde_wasm_bindgen::to_value(&parsed).map_err(|err| err.to_string())
	}

	pub fn compile_preprocessed(&self, code: JsValue) -> Result<String, String> {
		let code: Code = serde_wasm_bindgen::from_value(code).map_err(|err| err.to_string())?;

		self.inner.compile_preprocessed(code)
	}

	pub fn compile_tokens(&self, tokens: JsValue) -> Result<String, String> {
		let tokens: Vec<Token> =
			serde_wasm_bindgen::from_value(tokens).map_err(|err| err.to_string())?;

		self.inner.compile_tokens(tokens)
	}

	pub fn compile_ast(&self, parse_result: JsValue) -> Result<String, String> {
		let parse_result: (Expression, String) =
			serde_wasm_bindgen::from_value(parse_result).map_err(|err| err.to_string())?;

		self.inner.compile_ast(parse_result)
	}

	pub fn compile_code(&self, code: String) -> Result<String, String> {
		self.inner.compile_code(code)
	}
}

impl Default for Clue {
	fn default() -> Self {
		Self::new()
	}
}
