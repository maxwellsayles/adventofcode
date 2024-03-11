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

fn scale_reaction(
    r: &Reaction,
    x: i32,
) -> Reaction {
    let consume = r.0.iter().map(|(k, v)| (k.clone(), v * x)).collect();
    let produce = r.1.iter().map(|(k, v)| (k.clone(), v * x)).collect();
    (consume, produce)
}

/**
 * Return a reaction that produces at least the requested amount of an element
 * (may produce more than the requested amount).
 */
fn produce_reaction(
    rules: &Rules,
    to_produce: &String,
    qty: i32,
) -> Reaction {
    let rule = &rules[to_produce];
    let mult = ((qty as f32) / (rule.qty as f32)).ceil() as i32;
    let consume = rule.consume.clone();
    let produce = HashMap::from([(to_produce.clone(), rule.qty)]);
    scale_reaction(&(consume, produce), mult)
}

fn main() {
    let contents = fs::read_to_string("day14.txt").unwrap();
    let xs = contents
	.lines()
	.map(|s| parse_rule(s));

    let rules: Rules = xs
	.map(|rule| (rule.produce.clone(), rule))
	.collect();

    // for (k, v) in rules {
    // 	println!("{} {:?}", k, v);
    // }
    let reaction = produce_reaction(&rules, &String::from("CJTB"), 7);
    println!("{:?}", reaction);
}
