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
    room: String,
    doors: HashSet<String>,
    items: HashSet<String>,
    visited: HashSet<String>,
    ignored_items: HashSet<String>,
}

impl State {
    fn new(code: &Vec<i64>, ignored_items: HashSet<String>) -> Self {
	Self {
	    comp: IntcodeComputer::new(vec![], &code),
	    room: String::new(),
	    doors: HashSet::new(),
	    items: HashSet::new(),
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

    fn update_room_state(&mut self, buff: &str) {
	// Update the room name.
	let re_room = Regex::new("== (.*) ==").unwrap();
	self.room = match re_room.captures(buff) {
	    Some(capture) => capture.get(1).unwrap().as_str().to_string(),
	    None => String::new(),
	};

	// Update doors and items.
	self.doors = Self::parse_bullets(buff, "Doors here lead:\n")
	    .unwrap_or(HashSet::new());
	self.items = Self::parse_bullets(buff, "Items here:\n")
	    .unwrap_or(HashSet::new());
    }

    fn try_move(&mut self, dir: &str) -> bool {
	if !self.doors.contains(dir) {
	    return false
	}

	let buff_binding = self.step_comp(dir);
	let buff = buff_binding.as_str();

	// Test if move was successful.
	let re_fail = Regex::new(r"You can't go that way.").unwrap();
	if re_fail.is_match(buff) {
	    return false
	}
	let re_ejected =
	    Regex::new(r"you are ejected back to the checkpoint").unwrap();
	if re_ejected.is_match(buff) {
	    return false
	}

	self.update_room_state(buff);
	true
    }

    fn step_take_items(&mut self) {
	// Take items if there are any.
	for item in self.items.clone().iter() {
	    if !self.ignored_items.contains(item.as_str()) {
		self.step_comp(&(String::from("take ") + item).as_str());
	    }
	}

	if self.visited.contains(&self.room) {
	    return
	}
	self.visited.insert(self.room.clone());

	if self.try_move("north") {
	    self.step_take_items();
	    assert!(self.try_move("south"));
	}
	if self.try_move("east") {
	    self.step_take_items();
	    assert!(self.try_move("west"));
	}
	if self.try_move("south") {
	    self.step_take_items();
	    assert!(self.try_move("north"));
	}
	if self.try_move("west") {
	    self.step_take_items();
	    assert!(self.try_move("east"));
	}
    }

    fn parse_bullets(buff: &str, prefix: &str) -> Option<HashSet<String>> {
	let re_group = Regex::new(&format!("{}(- .*\n)*", prefix)).unwrap();
	match re_group.captures(&buff) {
	    Some(group_capture) => {
		let re_item = Regex::new(r"- (.*)").unwrap();
		let mut res = HashSet::new();
		for l in group_capture.get(0).unwrap().as_str().lines() {
		    match re_item.captures(l) {
			Some(capture) => res.insert(capture.get(1).unwrap().as_str().to_string()),
			None => true,
		    };
		}
		Some(res)
	    },
	    None => None,
	}
    }

    fn list_items(&mut self) -> Option<HashSet<String>> {
	let buff = self.step_comp("inv");
	Self::parse_bullets(&buff, "Items in your inventory:\n")
    }

    fn run(&mut self) {
	// Run one step and get output. Then iterate commands.
	let buff = self.step_comp(&String::new());
	self.update_room_state(buff.as_str());
	self.step_take_items();
	self.list_items();
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

