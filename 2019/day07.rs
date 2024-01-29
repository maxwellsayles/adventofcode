use std::collections::VecDeque;
use std::cmp::max;
use std::fs;
use itertools::Itertools;

struct IntcodeComputer {
    inputs: VecDeque<i64>,
    outputs: VecDeque<i64>,
    code: Vec<i64>,
    ip: usize,
    running: bool,
}

impl IntcodeComputer {
    fn new(inputs: VecDeque<i64>, code: Vec<i64>) -> Self {
	Self {
	    inputs: inputs,
	    outputs: VecDeque::new(),
	    code: code,
	    ip: 0,
	    running: false,
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
		match self.inputs.pop_front() {
		    Some(i) => {
			let t = self.code[self.ip + 1] as usize;
			self.code[t] = i;
			self.ip += 2;
		    },
		    None => self.running = false,
		}
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
            99 => self.running = false,
            _ => panic!("Unknown opcode {}", instr),
	}
    }

    fn run(&mut self) {
	self.running = true;
	while self.running {
	    self.step();
	}
    }

    fn is_halted(&self) -> bool {
	!self.running && self.code[self.ip] == 99
    }
}

fn run_phase_sequence1(code: Vec<i64>, phase_sequence: Vec<i64>) -> i64 {
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

fn part1(code: Vec<i64>) -> i64 {
    let mut max_output = 0;
    let perms = (0..5).permutations(5);
    for perm in perms {
	let output = run_phase_sequence1(code.clone(), perm);
	max_output = max(output, max_output);
    }
    max_output

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

    // run_phase_sequence1(code.clone(), phase_sequence);
}

fn run_phase_sequence2(code: Vec<i64>, phase_sequence: Vec<i64>) -> i64 {
    let n = phase_sequence.len();

    // Setup initial computers and state.
    let mut comps = Vec::new();
    for phase in phase_sequence {
	let inputs = VecDeque::from(vec![phase]);
	let comp = IntcodeComputer::new(inputs, code.clone());
	comps.push(comp);
    }

    // Push first signal (0) onto first computer.
    comps[0].inputs.push_back(0);

    // Run each computer in sequence until it halts or is blocked on input.
    // Exit when the final computer halts and then return its last output.
    let mut idx = 0;
    let mut final_output = 0;
    while !comps[n - 1].is_halted() {
	comps[idx].run();
	let idx2 = (idx + 1) % n;
	while let Some(output) = comps[idx].outputs.pop_front() {
	    if idx == n - 1 {
		final_output = output;
	    }
	    comps[idx2].inputs.push_back(output);
	}
	idx = idx2;
    }
    final_output
}


fn part2(code: Vec<i64>) -> i64 {
    let mut max_output = 0;
    let perms = (5..10).permutations(5);
    for perm in perms {
	let output = run_phase_sequence2(code.clone(), perm);
	max_output = max(output, max_output);
    }
    max_output


    // Expected output: 139629729
    // let code = vec![3,26,1001,26,-4,26,3,27,1002,27,2,27,1,27,26,
    // 		    27,4,27,1001,28,-1,28,1005,28,6,99,0,0,5];
    // let phase_sequence = vec![9, 8, 7, 6, 5];

    // Expected output: 18216
    // let code = vec![
    // 	3,52,1001,52,-5,52,3,53,1,52,56,54,1007,54,5,55,1005,55,26,1001,54,
    // 	-5,54,1105,1,12,1,53,54,53,1008,54,0,55,1001,55,1,55,2,53,55,53,4,
    // 	53,1001,56,-1,56,1005,56,6,99,0,0,0,0,10];
    // let phase_sequence = vec![9, 7, 8, 5, 6];

    // run_phase_sequence2(code.clone(), phase_sequence)
}

fn main() {
    let contents = fs::read_to_string("day07.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    println!("{}", part1(code.clone()));
    println!("{}", part2(code.clone()));
}
