use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

fn in_beam(code: &Vec<i64>, x: i64, y: i64) -> bool {
    if x < 0 || y < 0 {
	return false;
    }
    let mut comp = IntcodeComputer::new(vec![x, y], &code);
    comp.run();
    comp.remove_output().unwrap() == 1
}

// x,y represents the bottom-left corner of the box
fn is_big_nuff(code: &Vec<i64>, x: i64, y: i64) -> bool {
    in_beam(&code, x, y)
	&& in_beam(&code, x + 99, y)
	&& in_beam(&code, x + 99, y - 99)
	&& in_beam(&code, x, y - 99)
}

fn part1(code: &Vec<i64>) {
    let mut cnt = 0;
    for y in 0..50 {
	for x in 0..50 {
	    if in_beam(&code, x, y) {
		cnt += 1;
	    }
	}
    }
    println!("{}", cnt);
}

fn part2(code: &Vec<i64>) {
    // Traverse the left edge of the tractor beam until it is big enough.
    let mut x = 0;
    let mut y = 10; // Start a random amount down the beam, since it cuts out early on
    loop {
	// Find the first x value inside of the beam.
	while !in_beam(&code, x, y) {
	    x += 1;
	}
	if is_big_nuff(&code, x, y) {
	    println!("{} {} {}", x, y, x * 10000 + y - 99);
	    return;
	}
	y += 1;
    }
}

fn main() {
    let code = fs::read_to_string("day19.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();
    part1(&code);
    part2(&code);
}
