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

/**
 * Solution for part 2. Rather than solve this in a generic way, I just printed
 * out the map and solved it by hand. Pretty easy really.
 */
fn part2(code: &Vec<i64>) {
    // A = R,8,L,10,R,8
    // B = R,12,R,8,L,8,L,12
    // A = R,8,L,10,R,8
    // C = L,12,L,10,L,8
    // A = R,8,L,10,R,8
    // B = R,12,R,8,L,8,L,12
    // C = L,12,L,10,L,8
    // C = L,12,L,10,L,8
    // A = R,8,L,10,R,8
    // B = R,12,R,8,L,8,L,12

    let fn_main = "A,B,A,C,A,B,C,C,A,B";
    let fn_a = "R,8,L,10,R,8";
    let fn_b = "R,12,R,8,L,8,L,12";
    let fn_c = "L,12,L,10,L,8";
    let mut comp = IntcodeComputer::new(vec![], &code);
    comp.poke_mem(0, 2);

    let mut add_input = |s: &str| {
	for c in s.chars() {
	    comp.add_input(c as i64);
	}
	comp.add_input(10i64);
    };
    add_input(fn_main);
    add_input(fn_a);
    add_input(fn_b);
    add_input(fn_c);
    add_input("n");
    comp.run();
    while comp.output_len() > 1 {
	comp.remove_output();
    }
    let c = comp.remove_output().unwrap();
    println!("{}", c);
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
	print!("{}", ch);
	if ch != '\n' {
	    cells.insert((x, y), ch);
	    x += 1;
	} else {
	    x = 0;
	    y += 1;
	}
    }

    part1(&cells);
    part2(&code);
}
