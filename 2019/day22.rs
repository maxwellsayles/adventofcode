use regex::Regex;
use std::fs;

enum Op {
    Rev,
    Cut { n: i32 },
    Inc { n: i32 },
}

fn parse_input(filename: &str) -> Vec<Op> {
    let content = fs::read_to_string(filename)
	.unwrap();
    let re_deal_into_new_stack =
	Regex::new(r"^deal into new stack$").unwrap();
    let re_cut =
	Regex::new(r"^cut (-?\d+)$").unwrap();
    let re_deal_with_increment =
	Regex::new(r"^deal with increment (\d+)$").unwrap();
    let mut res = Vec::new();
    for l in content.lines() {
	if let Some(_) = re_deal_into_new_stack.captures(l) {
	    res.push(Op::Rev);
	} else if let Some(captures) = re_cut.captures(l) {
	    let n = captures.get(1).unwrap().as_str().parse::<i32>().unwrap();
	    res.push(Op::Cut { n });
	} else if let Some(captures) = re_deal_with_increment.captures(l) {
	    let n = captures.get(1).unwrap().as_str().parse::<i32>().unwrap();
	    res.push(Op::Inc { n });
	} else {
	    panic!("Unrecognized instructions: {}", l);
	}
    }
    res
}

fn part1(ops: &Vec<Op>) {
    const P: i32 = 10007;
    let mut i = 2019;
    for op in ops.iter() {
	match op {
	    Op::Rev => {
		i = P - 1 - i;
	    },
	    Op::Cut { n } => {
		i = (i - n) % P;
		if i < 0 {
		    i += P;
		}
	    },
	    Op::Inc { n } => {
		i = (i * n) % P;
	    },
	}
    }
    println!("{}", i);
}

fn main() {
    let ops = parse_input("day22.txt");
    part1(&ops);
}
