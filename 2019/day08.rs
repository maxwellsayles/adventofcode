use std::fs;

const WIDTH: usize = 25;
const HEIGHT: usize = 6;
const AREA: usize = WIDTH * HEIGHT;

fn count_char(input: &str, c: char) -> i64 {
    let mut cnt = 0;
    for x in input.chars() {
	if x == c {
	    cnt += 1;
	}
    }
    cnt
}

fn chunk(input: &str, n: usize) -> Vec<&str> {
    let mut layers = vec![];
    let mut rem = input;
    while rem.chars().count() >= n {
	let (a, b) = rem.split_at(n);
	layers.push(a);
	rem = b;
    }
    layers
}

fn part1(input: &str) -> i64 {
    let layers = chunk(input, AREA);

    // Find layer with least 0s.
    let mut min = std::i64::MAX;
    let mut res = 0;
    for layer in layers {
	let zcnt = count_char(layer, '0');
	if zcnt < min {
	    min = zcnt;
	    res = count_char(layer, '1') * count_char(layer, '2');
	}
    }
    res
}

fn main() {
    let contents = fs::read_to_string("day08.txt")
        .unwrap();
    println!("{}", part1(&contents));
}
