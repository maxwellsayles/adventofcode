use regex::Regex;
use std::fs;

fn process_input(filename: &str, p: i32) -> (i32, i32) {
    let content = fs::read_to_string(filename)
	.unwrap();
    let re_deal_into_new_stack =
	Regex::new(r"^deal into new stack$").unwrap();
    let re_cut =
	Regex::new(r"^cut (-?\d+)$").unwrap();
    let re_deal_with_increment =
	Regex::new(r"^deal with increment (\d+)$").unwrap();
    let mut i = 0;
    let mut j = 1;
    for l in content.lines() {
	if let Some(_) = re_deal_into_new_stack.captures(l) {
	    i = p - 1 - i;
	    j = p - 1 - j;
	} else if let Some(captures) = re_cut.captures(l) {
	    let n = captures.get(1).unwrap().as_str().parse::<i32>().unwrap();
	    i = (i - n) % p;
	    if i < 0 {
		i += p;
	    }
	    j = (j - n) % p;
	    if j < 0 {
		j += p;
	    }
	} else if let Some(captures) = re_deal_with_increment.captures(l) {
	    let n = captures.get(1).unwrap().as_str().parse::<i32>().unwrap();
	    i = (i * n) % p;
	    j = (j * n) % p;
	} else {
	    panic!("Unrecognized instructions: {}", l);
	}
    }
    (i, (j - i) % p)
}

fn main() {
    let (i, j) = process_input("day22.txt", 10007);
    let k = ((j * 2019) + i) % 10007;
    println!("{}", k);
}
