use std::assert;
use std::collections::VecDeque;
use std::fs;
// use itertools::Itertools;

struct IntcodeComputer {
    inputs: VecDeque<i64>,
    code: Vec<i64>,
    ip: usize,
}

impl IntcodeComputer {
    fn new(inputs: VecDeque<i64>, code: Vec<i64>) -> Self {
	Self {
	    inputs: inputs,
	    code: code,
	    ip: 0
	}
    }

    fn lookup(&self, val: i64, mode: i64) -> i64 {
	if mode == 0 {
            self.code[val as usize]
	} else {
            val
	}
    }

    fn step(&mut self) -> bool {
	let instr = self.code[self.ip];
	let mode1 = (instr / 100) % 2;
	let mode2 = (instr / 1000) % 2;
	match instr % 100 {
            1 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = x + y;
		self.ip += 4;
		true
            },
            2 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = x * y;
		self.ip += 4;
		true
            },
            3 => {
		let t = self.code[self.ip + 1] as usize;
		self.code[t] = match self.inputs.pop_front() {
		    Some(i) => i,
		    _ => panic!("Expected more inputs!"),
		};
		self.ip += 2;
		true
            },
            4 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		println!("{}", x);
		self.ip += 2;
		true
            },
            5 => {
		let p = self.lookup(self.code[self.ip + 1], mode1);
		self.ip = if p != 0 {
                    self.lookup(self.code[self.ip + 2], mode2) as usize
		} else {
                    self.ip + 3
		};
		true
            },
            6 => {
		let p = self.lookup(self.code[self.ip + 1], mode1);
		self.ip = if p == 0 {
                    self.lookup(self.code[self.ip + 2], mode2) as usize
		} else {
                    self.ip + 3
		};
		true
            },
            7 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = if x < y { 1 } else { 0 };
		self.ip += 4;
		true
            },
            8 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = if x == y { 1 } else { 0 };
		self.ip += 4;
		true
            },
            99 => false,
            _ => {
		assert!(false, "Unknown opcode {}", instr);
		false
	    },
	}
    }

    fn run(&mut self) {
	self.ip = 0;
	let mut looping = true;
	while looping {
	    looping = self.step();
	}
    }
}

fn main() {
    // let perms = (0..5).permutations(5);
    // for perm in perms {
    // 	println!("{:?}", perm);
    // }
    // println!("Finished!");

    let contents = fs::read_to_string("day05.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut comp = IntcodeComputer::new(VecDeque::from(vec![5]), code);
    comp.run();
}
