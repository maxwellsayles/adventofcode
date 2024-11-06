//use std::collections::VecDeque;
use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

fn part1(code: &Vec<i64>) {
    let mut comps = Vec::new();
    for i in 0..50 {
	comps.push(IntcodeComputer::new(vec![i], &code));
    }

    let mut idx = 0;
    loop {
	let mut packets = Vec::new();
	{
	    // Process a single computer.
	    let comp = &mut comps[idx];
	    if comp.is_waiting_on_input() {
		comp.add_input(-1);
	    }
	    comp.run();
	    while let Some(out_idx) = comp.remove_output() {
		let x = comp.remove_output().unwrap();
		let y = comp.remove_output().unwrap();
		if out_idx == 255 {
		    println!("{}", y);
		    return;
		}
		packets.push((out_idx, x, y));
	    }
	}
	{
	    // Process packets output from that computer.
	    for (idx, x, y) in packets.iter() {
		let comp = &mut comps[*idx as usize];
		comp.add_input(*x);
		comp.add_input(*y);
	    }
	}
	// Move to the next computer.
	idx = (idx + 1) % 50;
    }
}

fn main() {
    let code = fs::read_to_string("day23.txt")
	.unwrap()
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();
    part1(&code);
}
