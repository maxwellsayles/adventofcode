use std::fs;

struct FFTValIter {
    idx: usize,
    n: usize,
}

impl FFTValIter {
    fn new(n: usize) -> Self {
	Self {
	    idx: 0,
	    n,
	}
    }
}

impl Iterator for FFTValIter {
    type Item = i32;

    fn next(&mut self) -> Option<Self::Item> {
	const VALS: [i32; 4] = [0, 1, 0, -1];

	self.idx += 1;
	let idx = (self.idx / self.n) % 4;
	Some(VALS[idx])
    }
}

fn dot_product(
    x: impl Iterator<Item=i32>,
    y: impl Iterator<Item=i32>,
) -> i32 {
    x.zip(y).map(|(a, b)| a * b).sum()
}

// Least Significant Digit
fn lsd(x: i32) -> i32 {
    x.abs() % 10
}

fn fft(x: &Vec<i32>) -> Vec<i32> {
    let mut res = Vec::with_capacity(x.len());
    for i in 1..=x.len() {
	let d = dot_product(x.iter().map(|x| *x), FFTValIter::new(i));
	res.push(lsd(d));
    }
    res
}

fn part1(xs: &Vec<i32>) {
    let mut ys = xs.clone();
    for _ in 0..100 {
	ys = fft(&ys);
    }
    let res = ys.iter()
	.take(8)
	.map(|x| x.to_string())
	.collect::<Vec<_>>()
	.join("");
    println!("{}", res);
}

/**
 * Went to Reddit to see how others solved this. Heavily influenced by the
 * solution here: https://github.com/prscoelho/aoc2019/blob/master/src/aoc16/mod.rs
 */
fn part2(xs: &Vec<i32>) {
    let msg_offset =
	xs[0..7].iter().fold(0, |acc, x| acc * 10 + x) as usize;

    // Repeat the input sequence 10000 times, ignoring anything before the
    // message offset.
    let mut cur = xs
	.iter()
	.cycle()
	.take(xs.len() * 10000)
	.skip(msg_offset)
	.map(|x| *x) // Convert &i32 to i32
	.collect::<Vec<i32>>();

    for _ in 0..100 {
        let mut sums = Vec::with_capacity(cur.len());
        let mut total = 0;
	for x in cur.iter() {
	    sums.push(total);
	    total += x;
	}

	let last = sums.last().unwrap();
        for i in 0..cur.len() {
            cur[i] = (last - sums[i]) % 10;
        }
    }

    let res = cur[0..8].iter().fold(0, |acc, x| acc * 10 + x);
    println!("{}", res);
}

fn main() {
    let contents: Vec<i32> = fs::read_to_string("day16.txt")
	.unwrap()
	.trim()
	.chars()
	.map(|c| c.to_digit(10).unwrap() as i32)
	.collect();

    part1(&contents);
    part2(&contents);
}
