use regex::Regex;
use std::collections::{ HashSet };
use std::env;
use std::fs;
use std::io;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

const HELP: &str = "day25 without any arguments will run the solutions.
Options:
\t-r\tRun game inside the REPL.
";

fn to_comp(comp: &mut IntcodeComputer, buff: &String) {
    for c in buff.chars() {
	comp.add_input(c as i64);
    }
}

fn from_comp(comp: &mut IntcodeComputer) -> String {
    let mut buffer = String::new();
    while let Some(c) = comp.remove_output() {
	buffer.push((c as u8) as char);
    }
    buffer
}

fn repl(code: &Vec<i64>) {
    let mut comp = IntcodeComputer::new(vec![], &code);
    loop {
	comp.run();
	print!("{}", from_comp(&mut comp));
	let mut buffer = String::new();
	io::stdin().read_line(&mut buffer).unwrap();
	to_comp(&mut comp, &buffer);
    }
}

struct State {
    comp: IntcodeComputer,
    pos: (i32, i32),
    visited: HashSet<(i32, i32)>,
    ignored_items: HashSet<String>,
}

impl State {
    fn new(code: &Vec<i64>, ignored_items: HashSet<String>) -> Self {
	Self {
	    comp: IntcodeComputer::new(vec![], &code),
	    pos: (0, 0),
	    visited: HashSet::new(),
	    ignored_items,
	}
    }

    fn step_comp(&mut self, input: &str) -> String {
	println!("{}", input);
	if input.len() > 0 {
	    to_comp(&mut self.comp, &(String::from(input) + &"\n"));
	}
	self.comp.run();
	let buff = from_comp(&mut self.comp);
	print!("{}", buff);
	buff
    }

    fn try_move(&mut self, dir: &str, dx: i32, dy: i32) -> bool {
	let buff = self.step_comp(dir);

	// Test if move was successful.
	let re_fail = Regex::new(r"You can't go that way.").unwrap();
	if re_fail.is_match(buff.as_str()) {
	    return false
	}
	let re_ejected =
	    Regex::new(r"you are ejected back to the checkpoint").unwrap();
	if re_ejected.is_match(buff.as_str()) {
	    return false
	}

	// Take item if there is an item.
	let re_items = Regex::new(r"Items here:\n(- (.*)\n)+").unwrap();
	for item_capture in re_items.captures_iter(buff.as_str()) {
	    let item = item_capture.get(2).unwrap().as_str();
	    if !self.ignored_items.contains(&String::from(item)) {
		self.step_comp(&(String::from("take ") + item).as_str());
	    }
	}

	self.visited.insert(self.pos.clone());
	self.pos = (self.pos.0 + dx, self.pos.1 + dy);
	true
    }

    fn step(&mut self) {
	if self.visited.contains(&self.pos) {
	    return;
	}

	if self.try_move("north", 0, -1) {
	    self.step();
	    assert!(self.try_move("south", 0, 1));
	}
	if self.try_move("east", 1, 0) {
	    self.step();
	    assert!(self.try_move("west", -1, 0));
	}
	if self.try_move("south", 0, 1) {
	    self.step();
	    assert!(self.try_move("north", 0, -1));
	}
	if self.try_move("west", -1, 0) {
	    self.step();
	    assert!(self.try_move("east", 1, 0));
	}
    }

    fn run(&mut self) {
	// Run one step and get output. Then iterate commands.
	self.step_comp(&String::new());
	self.step();
	self.step_comp("inv");
    }
}

fn part1(code: &Vec<i64>) {
    let ignored_items = HashSet::from([
	"photons",
	"infinite loop",
	"escape pod",
	"molten lava",
	"giant electromagnet",
    ].map(|i| String::from(i)));
    let mut state = State::new(&code, ignored_items);
    state.run();
}

fn main() {
    let code = fs::read_to_string("day25.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let args: Vec<String> = env::args().collect();
    match args.len() {
	1 => part1(&code),
	2 => match args[1].as_str() {
	    "-r" => repl(&code),
	    _ => print!("{}", HELP),
	},
	_ => print!("{}", HELP),
    }
}

