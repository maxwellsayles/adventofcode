use std::collections::HashSet;

const INPUT: [&str; 5] = [
    "##.#.",
    ".#.##",
    ".#...",
    "#..#.",
    ".##..",
    // "....#",
    // "#..#.",
    // "#..##",
    // "..#..",
    // "#....",
];

#[inline(always)]
fn point_to_mask(x: usize, y: usize) -> u32 {
    1u32 << (x + y * 5)
}

#[inline(always)]
fn is_bug(state: u32, x: usize, y: usize) -> bool {
    if x >= 5 || y >= 5 {
	return false;
    }
    state & point_to_mask(x, y) != 0
}

fn input_to_state(input: &[&str; 5]) -> u32 {
    let mut res = 0u32;
    for (y, s) in input.iter().enumerate() {
	for (x, c) in s.chars().enumerate() {
	    if c == '#' {
		let i = point_to_mask(x, y);
		res |= i;
	    }
	}
    }
    res
}

#[allow(dead_code)]
fn print_state(state: u32) {
    for y in 0..5 {
	for x in 0..5 {
	    print!("{}", if is_bug(state, x, y) { '#' } else { '.' });
	}
	println!();
    }
}

fn step(state: u32) -> u32 {
    let mut res = 0;
    for y in 0..5 {
	for x in 0..5 {
	    let d = is_bug(state, x, y + 1);
	    let u = is_bug(state, x, y - 1);
	    let l = is_bug(state, x - 1, y);
	    let r = is_bug(state, x + 1, y);
	    let c = is_bug(state, x, y);
	    let cnt =
		if d { 1 } else { 0 } +
		if u { 1 } else { 0 } +
		if l { 1 } else { 0 } +
		if r { 1 } else { 0 };
	    let mask = point_to_mask(x, y);
	    if c {
		res |= if cnt == 1 { mask } else { 0 };
	    } else {
		res |= if cnt == 1 || cnt == 2 { mask } else { 0 };
	    }
	}
    }
    res
}

fn part1() {
    let mut state = input_to_state(&INPUT);
    let mut visited = HashSet::new();
    while !visited.contains(&state) {
	visited.insert(state);
	state = step(state);
    }
    println!("{}", state);
}

fn main() {
    part1();
}
