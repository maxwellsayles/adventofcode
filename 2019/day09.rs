use std::collections::VecDeque;
use std::fs;

const MEM_SIZE: usize = 2048;

struct IntcodeComputer {
    inputs: VecDeque<i64>,
    outputs: VecDeque<i64>,
    mem: [i64; MEM_SIZE],
    ip: usize,
    relative_base: i64,
    running: bool,
}

impl IntcodeComputer {
    fn new(inputs: VecDeque<i64>, code: &Vec<i64>) -> Self {
	let mut mem: [i64; MEM_SIZE] = [0; MEM_SIZE];
	for (i, x) in code.iter().enumerate() {
	    mem[i] = *x;
	}
	Self {
	    inputs: inputs,
	    outputs: VecDeque::new(),
	    mem: mem,
	    ip: 0,
	    relative_base: 0,
	    running: false,
	}
    }

    fn lookup(&self, arg: usize) -> i64 {
	let mode = (self.mem[self.ip] / (10 * 10_i64.pow(arg as u32))) % 10;
	let val = self.mem[self.ip + arg];
	match mode {
	    0 => self.mem[val as usize],
	    1 => val,
	    2 => self.mem[(val + self.relative_base) as usize],
	    _ => panic!("Unknown lookup mode {}", mode),
	}
    }

    fn store(&mut self, arg: usize, val: i64) {
	let mode = (self.mem[self.ip] / (10 * 10_i64.pow(arg as u32))) % 10;
	let loc = self.mem[self.ip + arg];
	match mode {
	    0 => self.mem[loc as usize] = val,
	    2 => self.mem[(loc + self.relative_base) as usize] = val,
	    _ => panic!("Unexpected lookup mode {}", mode),
	}
    }

    fn step(&mut self) {
	let opcode = self.mem[self.ip];
	let instr = opcode % 100;
	match instr {
            1 => {
		self.store(3, self.lookup(1) + self.lookup(2));
		self.ip += 4;
            },
            2 => {
		self.store(3, self.lookup(1) * self.lookup(2));
		self.ip += 4;
            },
            3 => {
		match self.inputs.pop_front() {
		    Some(i) => {
			self.store(1, i);
			self.ip += 2;
		    },
		    None => self.running = false,
		}
            },
            4 => {
		let x = self.lookup(1);
		self.outputs.push_back(x);
		self.ip += 2;
            },
            5 => {
		let p = self.lookup(1);
		self.ip = if p != 0 {
		    self.lookup(2) as usize
		} else {
                    self.ip + 3
		};
            },
            6 => {
		let p = self.lookup(1);
		self.ip = if p == 0 {
                    self.lookup(2) as usize
		} else {
                    self.ip + 3
		};
            },
            7 => {
		let x = self.lookup(1);
		let y = self.lookup(2);
		self.store(3, if x < y { 1 } else { 0 });
		self.ip += 4;
            },
            8 => {
		let x = self.lookup(1);
		let y = self.lookup(2);
		self.store(3, if x == y { 1 } else { 0 });
		self.ip += 4;
            },
	    9 => {
		self.relative_base += self.lookup(1);
		self.ip += 2;
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

    // fn is_halted(&self) -> bool {
    // 	!self.running && self.mem[self.ip] == 99
    // }
}

fn part1(code: &Vec<i64>) -> VecDeque<i64> {
    let inputs = VecDeque::from(vec![1]);
    let mut comp = IntcodeComputer::new(inputs, code);
    comp.run();
    comp.outputs
}

fn part2(code: &Vec<i64>) -> i64 {
    let inputs = VecDeque::from(vec![2]);
    let mut comp = IntcodeComputer::new(inputs, code);
    comp.run();
    match comp.outputs.pop_front() {
	Some(x) => x,
	_ => panic!("WTF!"),
    }
}

fn main() {
    let contents = fs::read_to_string("day09.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    // let code = vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];
    // let code = vec![1102,34915192,34915192,7,4,7,99,0];
    // let code = vec![104,1125899906842624,99];

    let outputs = part1(&code);
    let pretty: Vec<String> = outputs.iter().map(|x| x.to_string()).collect();
    println!("{}", pretty.join(","));

    println!("{}", part2(&code));
}
