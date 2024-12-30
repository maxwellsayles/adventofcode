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

type Consume<'a> = HashMap<&'a str, i64>;
type Produce<'a> = HashMap<&'a str, i64>;

type Reaction<'a> = (Consume<'a>, Produce<'a>);

#[derive(Debug)]
struct Rule<'a> {
    qty: i64,
    produce: &'a str,
    consume: Consume<'a>,
}

type Rules<'a> = HashMap<&'a str, Rule<'a>>;

fn parse_rule<'a>(txt: &'a str) -> Rule<'a> {
    let lhs_rhs: Vec<_> = txt.split(" => ").collect();
    let (lhs, rhs) = (lhs_rhs[0], lhs_rhs[1]);
    let consume: Consume = lhs
	.split(", ")
	.map(|item| {
	    let xs: Vec<_> = item.split(' ').collect();
	    (xs[1], xs[0].parse::<i64>().unwrap())
	})
	.collect();
    let produce_tmp: Vec<_> = rhs.split(' ').collect();

    Rule {
	qty: produce_tmp[0].parse::<i64>().unwrap(),
	produce: produce_tmp[1],
	consume,
    }
}

fn scale_reaction<'a>(
    r: &Reaction<'a>,
    x: i64,
) -> Reaction<'a> {
    let consume = r.0.iter().map(|(k, v)| (*k, v * x)).collect();
    let produce = r.1.iter().map(|(k, v)| (*k, v * x)).collect();
    (consume, produce)
}

/**
 * Return a reaction that produces at least the requested amount of an element
 * (may produce more than the requested amount).
 */
fn produce_reaction<'a>(
    rules: &Rules<'a>,
    to_produce: &'a str,
    qty: i64,
) -> Reaction<'a> {
    let rule = &rules[to_produce];
    let mult = ((qty as f32) / (rule.qty as f32)).ceil() as i64;
    let consume = rule.consume.clone();
    let produce = HashMap::from([(to_produce, rule.qty)]);
    scale_reaction(&(consume, produce), mult)
}

fn combine_reactions<'a>(
    x: &Reaction<'a>,
    y: &Reaction<'a>,
) -> Reaction<'a> {
    fn combine<'a>(
	x: &HashMap<&'a str, i64>,
	y: &HashMap<&'a str, i64>,
    ) -> HashMap<&'a str, i64> {
	let mut z = x.clone();
	for (k, v) in y.iter() {
	    let vv = z.get(k).unwrap_or(&0) + v;
	    z.insert(k, vv);
	}
	z
    }
    (combine(&x.0, &y.0), combine(&x.1, &y.1))
}

fn normalize_reaction<'a>(r: &Reaction<'a>) -> Reaction<'a> {
    let mut cs = HashMap::new();
    for (ck, cv) in r.0.iter() {
	let vv = max(0, cv - r.1.get(ck).unwrap_or(&0));
	if vv > 0 {
	    cs.insert(*ck, vv);
	}
    }

    let mut ps = HashMap::new();
    for (pk, pv) in r.1.iter() {
	let vv = max(0, pv - r.0.get(pk).unwrap_or(&0));
	if vv > 0 {
	    ps.insert(*pk, vv);
	}
    }

    (cs, ps)
}

fn is_consume_just_ore<'a>(r: &Reaction<'a>) -> bool {
    r.0.len() == 1 && r.0.contains_key(&"ORE")
}

fn get_random_non_ore_from_consume<'a>(r: &Reaction<'a>) -> (&'a str, i64) {
    for (k, v) in r.0.iter() {
	if *k != "ORE" {
	    return (k, *v);
	}
    }

    panic!("No pair found in {:?}", r);
}

fn step_reaction<'a>(rules: &Rules<'a>, r: &Reaction<'a>) -> Reaction<'a> {
    let (k, v) = get_random_non_ore_from_consume(r);
    let r1 = produce_reaction(rules, k, v);
    let r2 = combine_reactions(r, &r1);
    normalize_reaction(&r2)
}

fn part1<'a>(rules: &Rules<'a>) {
    let mut r = produce_reaction(rules, "FUEL", 1);
    while !is_consume_just_ore(&r) {
	r = step_reaction(rules, &r);
    }
    println!("{}", r.0["ORE"]);
}

/**
 * Found by binary searching for the largest amount of FUEL that produced the
 * largest amount of ORE <= 10^12.
 */
fn part2<'a>(rules: &Rules<'a>) {
    let eval = |fuel: i64| -> bool {
	let mut r = produce_reaction(rules, "FUEL", fuel);
	while !is_consume_just_ore(&r) {
	    r = step_reaction(rules, &r);
	}
	r.0["ORE"] <= ONE_TRILLION
    };

    let mut a: i64 = 1;
    let mut n: i64 = i32::MAX as i64;
    while n > 1 {
	let m = n / 2;
	let b = a + m;
	if eval(b) {
	    n -= m;
	    a = b;
	} else {
	    n = m;
	}
    }
    println!("{}", a);
}

fn main() {
    let contents = fs::read_to_string("day14.txt").unwrap();
    let xs = contents
	.lines()
	.map(|s| parse_rule(s));

    let rules: Rules = xs
	.map(|rule| (rule.produce, rule))
	.collect();

    part1(&rules);
    part2(&rules);
}
