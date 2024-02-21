use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

fn main() {
    let contents = fs::read_to_string("day13.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut comp = IntcodeComputer::new(Vec::new(), &code);
    comp.run();
    let mut i = 0;
    let mut cnt = 0;
    while let Some(v) = comp.remove_output() {
	if i == 2 && v == 2 {
	    cnt += 1;
	}
	i = (i + 1) % 3;
    }
    println!("{}", cnt);
}
