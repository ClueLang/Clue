use clue_core::{
	env::{BitwiseMode, ContinueMode, LuaVersion},
	Clue as ClueCore,
};
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct Clue {
	inner: ClueCore,
}

#[wasm_bindgen]
impl Clue {
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

impl Default for Clue {
	fn default() -> Self {
		Self::new()
	}
}
