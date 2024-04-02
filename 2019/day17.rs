use std::char;
use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;


fn main() {
    let code = fs::read_to_string("day17.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut comp = IntcodeComputer::new(vec![], &code);
    comp.run();

    while let Some(c) = comp.remove_output() {
	print!("{}", char::from_u32(c as u32).unwrap());
    }
}
