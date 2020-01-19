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

fn compute_depth<'a>(
    orbits: &HashMap<&'a str, &'a str>,
    depths: &mut HashMap<(&'a str, &'a str), i64>,
    src: &'a str,
) {
    let mut d: i64 = 0;
    let mut n = src;
    while n != "COM" {
        n = orbits.get(n).unwrap();
        d += 1;
        depths.insert((src, n), d);
    }
}

fn distance_between(
    orbits: &HashMap<&str, &str>,
    src: &str,
    dst: &str,
) -> i64 {
    let mut depths = HashMap::new();
    compute_depth(&orbits, &mut depths, "YOU");
    compute_depth(&orbits, &mut depths, "SAN");

    let mut best = std::i64::MAX;
    for ((a, b), dist_src) in depths.iter() {
        if *a == src {
            if let Some(dist_dst) = depths.get(&(dst, *b)) {
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

    let d = distance_between(&input, "YOU", "SAN");
    println!("{}", d);
}
