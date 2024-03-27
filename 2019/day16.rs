use std::fs;

struct FFTIter {
    idx: usize,
    n: usize,
}

impl FFTIter {
    fn new(n: usize) -> Self {
	Self {
	    idx: 0,
	    n,
	}
    }
}

impl Iterator for FFTIter {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
	const VALS: [i32; 4] = [0, 1, 0, -1];

	self.idx += 1;
	let idx = (self.idx / self.n) % 4;
	Some(VALS[idx])
    }
}

fn main() {
    let contents: Vec<i32> = fs::read_to_string("day16.txt")
	.unwrap()
	.trim()
	.chars()
	.map(|c| c.to_digit(10).unwrap() as i32)
	.collect();

    let fft = FFTIter::new(2);
    for x in fft.take(8) {
	println!("{}", x);
    }
}
