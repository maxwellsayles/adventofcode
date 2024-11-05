use num::{BigInt, Integer};
use num::bigint::Sign;
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

fn part2(ops: &Vec<Op>) {
    const P64: i64 = 119315717514047;
    const E: i64 = 101741582076661;
    let p = BigInt::from(P64);
    let zero = BigInt::ZERO;
    let one = BigInt::from(1);

    let norm = |state: &(BigInt, BigInt)| -> (BigInt, BigInt) {
	let res = (&state.0 % &p, &state.1 % &p);
	(
	    &res.0 + if res.0.sign() == Sign::Minus { &p } else { &zero },
	    &res.1 + if res.1.sign() == Sign::Minus { &p } else { &zero },
	)
    };

    let mut state = (BigInt::from(1), BigInt::ZERO);
    for op in ops.iter().rev() {
	match op {
	    Op::Rev => {
		state = (- state.0, &p - &one - &state.1);
	    },
	    Op::Cut { n } => {
		state.1 = &state.1 + *n;
	    },
	    Op::Inc { n } => {
		let mut inv = i64::extended_gcd(&P64, &(*n as i64)).y;
		if inv < 0 {
		    inv += P64;
		}
		state = (&state.0 * inv, &state.1 * inv);
	    }
	}
	state = norm(&state);
    }

    let mut i = BigInt::from(2020);
    let mut exp = E;
    while exp > 0 {
	if exp % 2 == 1 {
            i = (&i * &state.0 + &state.1) % &p;
	    if i.sign() == Sign::Minus {
		i += &p;
	    }
	    exp -= 1;
	} else {
	    state = norm(&(
		&state.0 * &state.0,
		&state.0 * &state.1 + &state.1,
	    ));
            exp /= 2;
	}
    }
    println!("{}", i.to_string());
}

fn main() {
    let ops = parse_input("day22.txt");
    part1(&ops);
    part2(&ops);
}
