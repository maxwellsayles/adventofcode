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
}
