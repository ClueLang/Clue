pub static mut progressbar: usize = 0;
pub static mut maxbar: usize = 0;
static mut prevprog: usize = 0;
static mut name: String = String::new();

pub fn StartBar(newname: &str, newmax: usize) {
	unsafe {
		name = String::from(newname);
		progressbar = 0;
		maxbar = newmax;
	}
}

pub fn UpdateBar(limit: usize) {
	unsafe {
		if maxbar < limit || progressbar == prevprog {return}
		let mut bar = String::new();
		let tot = 100 * progressbar / maxbar;
		for i in 1..20 {
			bar += if i * 5 <= tot {"#"} else {" "};
		}
		print!("{} [#{}] {}{}% done.\r", name, bar, if tot < 10 {' '} else {'\0'}, tot);
		prevprog = progressbar;
	}
}