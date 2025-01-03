use std::collections::VecDeque;

const MEM_SIZE: usize = 16384;

pub struct IntcodeComputer {
    inputs: VecDeque<i64>,
    outputs: VecDeque<i64>,
    mem: [i64; MEM_SIZE],
    ip: usize,
    relative_base: i64,
    running: bool,
}

impl IntcodeComputer {
    pub fn new(inputs: Vec<i64>, code: &Vec<i64>) -> Self {
	let mut mem: [i64; MEM_SIZE] = [0; MEM_SIZE];
	for (i, x) in code.iter().enumerate() {
	    mem[i] = *x;
	}
	Self {
	    inputs: VecDeque::from(inputs),
	    outputs: VecDeque::new(),
	    mem: mem,
	    ip: 0,
	    relative_base: 0,
	    running: false,
	}
    }

    #[allow(dead_code)]
    pub fn add_input(&mut self, x: i64) {
	self.inputs.push_back(x);
    }

    #[allow(dead_code)]
    pub fn output_len(&self) -> usize {
	self.outputs.len()
    }

    pub fn remove_output(&mut self) -> Option<i64> {
	self.outputs.pop_front()
    }

    #[allow(dead_code)]
    pub fn poke_mem(&mut self, pos: usize, val: i64) {
	self.mem[pos] = val;
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

    fn get_opcode(&self) -> i64 {
	self.mem[self.ip] % 100
    }

    fn step(&mut self) {
	let opcode = self.get_opcode();
	match opcode {
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
            _ => panic!("Unknown opcode {}", opcode),
	}
    }

    pub fn run(&mut self) {
	self.running = true;
	while self.running {
	    self.step();
	}
    }

    #[allow(dead_code)]
    pub fn is_halted(&self) -> bool {
	!self.running && self.get_opcode() == 99
    }

    #[allow(dead_code)]
    pub fn is_waiting_on_input(&self) -> bool {
	!self.running && self.get_opcode() == 3 && self.inputs.is_empty()
    }
}

