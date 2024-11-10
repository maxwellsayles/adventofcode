use std::env;
use std::fs;
use std::io;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

fn repl(code: &Vec<i64>) {
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

const HELP: &str = "day25 without any arguments will run the solutions.
Options:
\t-r\tRun game inside the REPL.
";

fn main() {
    let code = fs::read_to_string("day25.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let args: Vec<String> = env::args().collect();
    if args.len() > 1 {
	match args[1].as_str() {
	    "-r" => repl(&code),
	    _ => print!("{}", HELP),
	}
    }
}
