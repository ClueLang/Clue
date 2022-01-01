#![allow(non_snake_case)]

use std::io;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut path: String = String::new();
    if args.len() == 1 {
        println!("Please insert path to code directory: ");
        io::stdin()
            .read_line(&mut path)
            .expect("Failed to read path!");
    } else {
        path = args[1].clone();
    }
    
}