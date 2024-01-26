use std::collections::VecDeque;
use std::cmp::max;
use std::fs;
use itertools::Itertools;

struct IntcodeComputer {
    inputs: VecDeque<i64>,
    outputs: VecDeque<i64>,
    code: Vec<i64>,
    ip: usize,
    halted: bool,
}

impl IntcodeComputer {
    fn new(inputs: VecDeque<i64>, code: Vec<i64>) -> Self {
	Self {
	    inputs: inputs,
	    outputs: VecDeque::new(),
	    code: code,
	    ip: 0,
	    halted: false,
	}
    }

    fn lookup(&self, val: i64, mode: i64) -> i64 {
	if mode == 0 {
            self.code[val as usize]
	} else {
            val
	}
    }

    fn step(&mut self) {
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
            },
            2 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = x * y;
		self.ip += 4;
            },
            3 => {
		let t = self.code[self.ip + 1] as usize;
		self.code[t] = match self.inputs.pop_front() {
		    Some(i) => i,
		    _ => panic!("Expected more inputs!"),
		};
		self.ip += 2;
            },
            4 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		self.outputs.push_back(x);
		self.ip += 2;
            },
            5 => {
		let p = self.lookup(self.code[self.ip + 1], mode1);
		self.ip = if p != 0 {
                    self.lookup(self.code[self.ip + 2], mode2) as usize
		} else {
                    self.ip + 3
		};
            },
            6 => {
		let p = self.lookup(self.code[self.ip + 1], mode1);
		self.ip = if p == 0 {
                    self.lookup(self.code[self.ip + 2], mode2) as usize
		} else {
                    self.ip + 3
		};
            },
            7 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = if x < y { 1 } else { 0 };
		self.ip += 4;
            },
            8 => {
		let x = self.lookup(self.code[self.ip + 1], mode1);
		let y = self.lookup(self.code[self.ip + 2], mode2);
		let t = self.code[self.ip + 3] as usize;
		self.code[t] = if x == y { 1 } else { 0 };
		self.ip += 4;
            },
            99 => self.halted = true,
            _ => panic!("Unknown opcode {}", instr),
	}
    }

    fn run(&mut self) {
	while self.outputs.len() == 0 {
	    self.step();
	}
	// while !self.halted {
	//     self.step();
	// }
    }
}

fn run_phase_sequence(code: Vec<i64>, phase_sequence: Vec<i64>) -> i64 {
    let mut output = 0;
    for phase in phase_sequence {
	let inputs = VecDeque::from(vec![phase, output]);
	let mut comp = IntcodeComputer::new(inputs, code.clone());
	comp.run();
	output = match comp.outputs.pop_front() {
	    Some(i) => i,
	    None => panic!("Expected an output!"),
	};
    }
    output
}

fn main() {
    let contents = fs::read_to_string("day07.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    let mut max_output = 0;
    let perms = (0..5).permutations(5);
    for perm in perms {
	let output = run_phase_sequence(code.clone(), perm);
	max_output = max(output, max_output);
    }
    println!("{}", max_output);

    // Expected output: 43210
    // let code = vec![3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0];
    // let phase_sequence = vec![4, 3, 2, 1, 0];

    // Expected output: 54321
    // let code = vec![3,23,3,24,1002,24,10,24,1002,23,-1,23,
    // 		    101,5,23,23,1,24,23,23,4,23,99,0,0];
    // let phase_sequence = vec![0,1,2,3,4];

    // Expected output: 65210
    // let code = vec![3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,
    // 		    1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0];
    // let phase_sequence = vec![1, 0, 4, 3, 2];

    // let output = run_phase_sequence(code.clone(), phase_sequence);
    // println!("{}", output);
}
