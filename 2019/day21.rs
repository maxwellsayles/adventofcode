use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;


fn input_string(comp: &mut IntcodeComputer, s: &str) {
    for c in s.chars() {
	comp.add_input(c as i64);
    }
    comp.add_input('\n' as i64);
}

fn part1(code: &Vec<i64>) {
    let mut comp = IntcodeComputer::new(vec![], &code);

    let prog = vec![
	"NOT A J",  // Jump if A is a hole
	"NOT B T",  // Jump if B is a hole
	"OR T J",
	"NOT C T",  // Jump if C is a hole
	"OR T J",
	"AND D J",  // Do not jump if D is a hole.
    ];
    for p in prog.iter() {
	input_string(&mut comp, p);
    }
    input_string(&mut comp, "WALK");
    comp.run();
    while let Some(c) = comp.remove_output() {
	if c > 255 {
	    println!("{}", c);
	}
    }
}

fn main() {
    let code = fs::read_to_string("day21.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();
    part1(&code);
}
