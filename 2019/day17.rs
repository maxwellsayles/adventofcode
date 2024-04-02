use std::char;
use std::collections::HashMap;
use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

type Point = (i32, i32);

fn part1(cells: &HashMap<Point, char>) {
    let mut res = 0;
    for (&(x, y), c) in cells.iter() {
	if c == &'#' {
	    if cells.get(&(x - 1, y)) == Some(&'#')
		&& cells.get(&(x + 1, y)) == Some(&'#')
		&& cells.get(&(x, y - 1)) == Some(&'#')
		&& cells.get(&(x, y + 1)) == Some(&'#') {
		    res += x * y;
		}
	}
    }

    println!("{}", res);
}

fn main() {
    let code = fs::read_to_string("day17.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut comp = IntcodeComputer::new(vec![], &code);
    comp.run();

    let mut cells: HashMap<Point, char> = HashMap::new();
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    while let Some(c) = comp.remove_output() {
	let ch = char::from_u32(c as u32).unwrap();
	if ch != '\n' {
	    cells.insert((x, y), ch);
	    x += 1;
	} else {
	    x = 0;
	    y += 1;
	}
    }

    part1(&cells);
}
