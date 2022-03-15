static mut name: String = String::new();
static mut current: usize = 0;
static mut max: usize = 0;

pub fn StartBar(newname: &str, newmax: usize) {
	unsafe {
		name = String::from(newname);
		current = 0;
		max = newmax;
	}
}

pub fn UpdateBar() {
	unsafe {
		let mut bar = String::new();
		let tot = 100 * current / max;
		for i in 1..20 {
			bar += if i * 5 <= tot {"#"} else {" "};
		}
		print!("{} [#{}] {}% done.\r", name, bar, tot);
		current += 1;
	}
}