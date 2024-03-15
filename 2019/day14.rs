/**
 * Idea for part 1 is to keep a LHS (what's needed) and RHS (what's produced).
 * In a loop: for any item on the LHS, replace it with the items needed to
 * produce enough of it. Then subtract any matching items from both sides to get
 * the deficit. Continue until the LHS is only ORE.
 */

use std::cmp::max;
use std::collections::HashMap;
use std::fs;

const ONE_TRILLION: i64 = 1000000000000;

type Consume = HashMap<String, i64>;
type Produce = HashMap<String, i64>;

type Reaction = (Consume, Produce);

#[derive(Debug)]
struct Rule {
    qty: i64,
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
	    (String::from(xs[1]), xs[0].parse::<i64>().unwrap())
	})
	.collect();
    let produce_tmp: Vec<_> = rhs.split(' ').collect();

    Rule {
	qty: produce_tmp[0].parse::<i64>().unwrap(),
	produce: String::from(produce_tmp[1]),
	consume,
    }
}

fn scale_reaction(
    r: &Reaction,
    x: i64,
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
    qty: i64,
) -> Reaction {
    let rule = &rules[to_produce];
    let mult = ((qty as f32) / (rule.qty as f32)).ceil() as i64;
    let consume = rule.consume.clone();
    let produce = HashMap::from([(to_produce.clone(), rule.qty)]);
    scale_reaction(&(consume, produce), mult)
}

fn combine_reactions(
    x: &Reaction,
    y: &Reaction,
) -> Reaction {
    fn combine(
	x: &HashMap<String, i64>,
	y: &HashMap<String, i64>,
    ) -> HashMap<String, i64> {
	let mut z = x.clone();
	for (k, v) in y.iter() {
	    let vv = z.get(k).unwrap_or(&0) + v;
	    z.insert(k.clone(), vv);
	}
	z
    }
    (combine(&x.0, &y.0), combine(&x.1, &y.1))
}

fn normalize_reaction(r: &Reaction) -> Reaction {
    let mut cs = HashMap::new();
    for (ck, cv) in r.0.iter() {
	let vv = max(0, cv - r.1.get(ck).unwrap_or(&0));
	if vv > 0 {
	    cs.insert(ck.clone(), vv);
	}
    }

    let mut ps = HashMap::new();
    for (pk, pv) in r.1.iter() {
	let vv = max(0, pv - r.0.get(pk).unwrap_or(&0));
	if vv > 0 {
	    ps.insert(pk.clone(), vv);
	}
    }

    (cs, ps)
}

fn is_consume_just_ore(r: &Reaction) -> bool {
    r.0.len() == 1 && r.0.contains_key(&String::from("ORE"))
}

fn get_random_non_ore_from_consume<'a>(r: &'a Reaction) -> (&'a String, i64) {
    for (k, v) in r.0.iter() {
	if k != "ORE" {
	    return (k, *v);
	}
    }

    panic!("No pair found in {:?}", r);
}

fn step_reaction(rules: &Rules, r: &Reaction) -> Reaction {
    let (k, v) = get_random_non_ore_from_consume(&r);
    let r1 = produce_reaction(&rules, k, v);
    let r2 = combine_reactions(r, &r1);
    normalize_reaction(&r2)
}

fn part1(rules: &Rules) {
    let mut r = produce_reaction(rules, &String::from("FUEL"), 1);
    while !is_consume_just_ore(&r) {
	r = step_reaction(rules, &r);
    }
    println!("{}", r.0["ORE"]);
}

/**
 * Found by manually binary searching for the largest amount of FUEL that
 * produced the largest amount of ORE <= 10^12. This could be computed with an
 * actual binary search, but doing it manually was pretty fast.
 */
fn part2(rules: &Rules) {
    const FUEL: i64 = 998536;

    let mut r = produce_reaction(rules, &String::from("FUEL"), FUEL);
    while !is_consume_just_ore(&r) {
	r = step_reaction(rules, &r);
    }
    let ore = r.0["ORE"];
    println!("{} {} {}", ore, ore <= ONE_TRILLION, FUEL);
}

fn main() {
    let contents = fs::read_to_string("day14.txt").unwrap();
    let xs = contents
	.lines()
	.map(|s| parse_rule(s));

    let rules: Rules = xs
	.map(|rule| (rule.produce.clone(), rule))
	.collect();

    part1(&rules);
    part2(&rules);
}
