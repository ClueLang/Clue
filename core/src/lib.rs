#[cfg(feature = "rpmalloc")]
#[global_allocator]
static ALLOC: rpmalloc::RpMalloc = rpmalloc::RpMalloc;

pub mod compiler;
pub mod env;
pub mod parser;
pub mod preprocessor;
pub mod scanner;

pub type CodeChar = (char, usize);
pub type Code = Vec<CodeChar>;
pub trait CodeExt {
	fn to_string(&self) -> String;
	fn from_str(value: &str, line: usize) -> Self;
}

impl CodeExt for Code {
	fn to_string(&self) -> String {
		let mut result = String::new();
		for (c, _) in self {
			result.push(*c);
		}
		result
	}
	
	fn from_str(chars: &str, line: usize) -> Self {
		let mut result = Self::new();
		for c in chars.chars() {
			result.push((c, line));
		}
		result
	}
}

#[macro_export]
macro_rules! check {
	($tocheck: expr) => {
		match $tocheck {
			Ok(t) => t,
			Err(e) => return Err(e.to_string()),
		}
	};
}

#[macro_export]
macro_rules! format_clue {
	($($strings:expr),+) => {{
		use std::ops::AddAssign;
		let mut len_format_clue = 0;
		$(len_format_clue.add_assign(AsRef::<str>::as_ref(&$strings).len());)+
		let mut output_format_clue = String::with_capacity(len_format_clue);
		$(output_format_clue.push_str($strings.as_ref());)+
		output_format_clue
	}};
}
