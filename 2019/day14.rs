/**
 * Tentative idea is to keep a LHS (what's needed) and RHS (what's produced).
 * In a loop: for any item on the LHS, replace it with the items needed to
 * produce enough of it. Then subtract any matching items from the RHS (already
 * produced) to get the deficit. Continue until the LHS is only ORE.
 */

use std::collections::HashMap;
use std::fs;

type Consume = HashMap<String, i32>;
type Produce = HashMap<String, i32>;

type Reaction = (Consume, Produce);

#[derive(Debug)]
struct Rule {
    qty: i32,
    produce: String,
    consume: Consume,
}

type Rules = HashMap<String, Rule>;

fn parse_rule(txt: &str) -> Rule {
    let lhs_rhs: Vec<_> = txt.split(" => ").collect();
    let (lhs, rhs) = (lhs_rhs[0], lhs_rhs[1]);
    let consume: Consume = lhs
	.split(", ")
	.map(|item| {
	    let xs: Vec<_> = item.split(' ').collect();
	    (String::from(xs[1]), xs[0].parse::<i32>().unwrap())
	})
	.collect();
    let produce_tmp: Vec<_> = rhs.split(' ').collect();

    Rule {
	qty: produce_tmp[0].parse::<i32>().unwrap(),
	produce: String::from(produce_tmp[1]),
	consume,
    }
}

fn main() {
    let contents = fs::read_to_string("day14.txt").unwrap();
    let xs = contents
	.lines()
	.map(|s| parse_rule(s));

    let rules: Rules = xs
	.map(|rule| (rule.produce.clone(), rule))
	.collect();

    for (k, v) in rules {
	println!("{} {:?}", k, v);
    }
}
