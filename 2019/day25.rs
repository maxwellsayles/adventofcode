use std::fs;
use std::io;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

fn main() {
    let code = fs::read_to_string("day25.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();
    let mut comp = IntcodeComputer::new(vec![], &code);
    loop {
	comp.run();
	while let Some(c) = comp.remove_output() {
	    print!("{}", (c as u8) as char);
	}
	let mut buffer = String::new();
	io::stdin().read_line(&mut buffer).unwrap();
	for c in buffer.chars() {
	    comp.add_input(c as i64);
	}
    }
}
