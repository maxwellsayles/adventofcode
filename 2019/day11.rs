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
}

fn main() {
    let contents = fs::read_to_string("day11.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut robot = Robot::new();
    let mut comp = IntcodeComputer::new(vec![], &code);
    comp.run();
    while !comp.is_halted() {
	// Provide the current cell as input.
	// Then run the program.
	// The program will provide two outputs:
	// 1) The new cell value.
	// 2) 0 to rotate CCW, 1 to rotate CW.
	assert!(comp.is_waiting_on_input());
	comp.add_input(robot.get_cell() as i64);
	comp.run();
	let val = comp.remove_output().unwrap();
	robot.set_cell(val as i32);
	let rot = comp.remove_output().unwrap();
	match rot {
	    0 => robot.rotate_ccw(),
	    1 => robot.rotate_cw(),
	    _ => panic!("Unexpected rotation value: {}", rot),
	}
	robot.move_forward();
    }
    println!("{}", robot.cells.len());
}
