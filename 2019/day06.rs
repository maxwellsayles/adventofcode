use std::cmp;
use std::collections::HashMap;
use std::fs;

fn depth(orbits: &HashMap<&str, &str>, node: &str) -> i64 {
    let mut d = 0;
    let mut n = node;
    while n != "COM" {
        n = orbits.get(n).unwrap();
        d += 1
    }
    d
}

fn compute_pair_depths<'a>(
    orbits: &HashMap<&'a str, &'a str>,
) -> HashMap<(&'a str, &'a str), i64> {
    let mut res = HashMap::new();
    let srcs = orbits.keys().map(|x| *x);
    for src in srcs {
        let mut d: i64 = 0;
        let mut dst = src;
        while dst != "COM" {
            dst = orbits.get(dst).unwrap();
            d += 1;
            res.insert((src, dst), d);
        }
    }
    res
}

fn distance_between(
    pair_depths: &HashMap<(&str, &str), i64>,
    src: &str,
    dst: &str,
) -> i64 {
    let mut best = std::i64::MAX;
    for ((a, b), dist_src) in pair_depths.iter() {
        if *a == src {
            if let Some(dist_dst) = pair_depths.get(&(dst, *b)) {
                best = cmp::min(best, dist_src + dist_dst - 2);
            }
        }
    }
    best
}       

fn main() {
    let contents = fs::read_to_string("day06.txt")
        .unwrap();
    let input: HashMap<_, _> = contents
        .lines()
        .map(|s| s.split(')').collect())
        .map(|xs: Vec<&str>| (xs[1], xs[0]))
        .collect();

    let solve1: i64 = input.keys()
        .map(|k| depth(&input, k))
        .sum();
    println!("{}", solve1);

    let pair_depths = compute_pair_depths(&input);
    let d = distance_between(&pair_depths, "YOU", "SAN");
    println!("{}", d);
}
