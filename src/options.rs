use std::env;
use std::collections::HashSet;

static mut UNSAFE_ENV_TOKENS: bool = false;
static mut UNSAFE_ENV_STRUCT: bool = false;
static mut UNSAFE_ENV_DONTSAVE: bool = false;
pub static ENV_TOKENS: &bool = unsafe {&UNSAFE_ENV_TOKENS};
pub static ENV_STRUCT: &bool = unsafe {&UNSAFE_ENV_STRUCT};
pub static ENV_DONTSAVE: &bool = unsafe {&UNSAFE_ENV_DONTSAVE};

pub fn SetupEnv() {
    let args: HashSet<String> = env::args().collect();
    unsafe {
		UNSAFE_ENV_TOKENS = args.contains(&String::from("-tokens"));
		UNSAFE_ENV_STRUCT = args.contains(&String::from("-struct"));
		UNSAFE_ENV_DONTSAVE = args.contains(&String::from("-dontsave"));
	}
}