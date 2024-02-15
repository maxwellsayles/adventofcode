use std::cmp::{min, max};
use std::collections::HashMap;
use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

type Point = (i32, i32);

struct Robot {
    x: i32,
    y: i32,
    dx: i32,
    dy: i32,
    cells: HashMap<Point, i32>,
}

impl Robot {
    fn new() -> Self {
	Self {
	    x: 0,
	    y: 0,
	    dx: 0,
	    dy: -1,
	    cells: HashMap::new(),
	}
    }

    fn get_cell(&self) -> i32 {
	*self.cells.get(&(self.x, self.y)).unwrap_or(&0)
    }

    fn set_cell(&mut self, val: i32) {
	self.cells.insert((self.x, self.y), val);
    }

    fn rotate_ccw(&mut self) {
	(self.dx, self.dy) = (self.dy, -self.dx);
    }

    fn rotate_cw(&mut self) {
	(self.dx, self.dy) = (-self.dy, self.dx);
    }

    fn move_forward(&mut self) {
	self.x += self.dx;
	self.y += self.dy;
    }

    fn run(&mut self, code: &Vec<i64>) {
	let mut comp = IntcodeComputer::new(vec![], &code);
	comp.run();
	while !comp.is_halted() {
	    // Provide the current cell as input.
	    // Then run the program.
	    // The program will provide two outputs:
	    // 1) The new cell value.
	    // 2) 0 to rotate CCW, 1 to rotate CW.
	    assert!(comp.is_waiting_on_input());
	    comp.add_input(self.get_cell() as i64);
	    comp.run();
	    let val = comp.remove_output().unwrap();
	    self.set_cell(val as i32);
	    let rot = comp.remove_output().unwrap();
	    match rot {
		0 => self.rotate_ccw(),
		1 => self.rotate_cw(),
		_ => panic!("Unexpected rotation value: {}", rot),
	    }
	    self.move_forward();
	}
    }
}

fn part1(code: &Vec<i64>) {
    let mut robot = Robot::new();
    robot.run(&code);
    println!("{}", robot.cells.len());
}


fn part2(code: &Vec<i64>) {
    let mut robot = Robot::new();
    // NOTE: Set the start cell to 1.
    robot.set_cell(1);
    robot.run(&code);

    // Get bounding rectangle of "1" cells.
    let (mut x1, mut y1, mut x2, mut y2) = (0, 0, 0, 0);
    for ((x, y), v) in robot.cells.iter() {
	if *v == 1 {
	    x1 = min(x1, *x);
	    y1 = min(y1, *y);
	    x2 = max(x2, *x);
	    y2 = max(y2, *y);
	}
    }

    for y in y1..=y2 {
	for x in x1..=x2 {
	    let v = *robot.cells.get(&(x, y)).unwrap_or(&0i32);
	    let c = if v == 0i32 { ' ' } else { '*' };
	    print!("{}", c);
	}
	println!();
    }
}

fn main() {
    let contents = fs::read_to_string("day11.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    part1(&code);
    part2(&code);
}
