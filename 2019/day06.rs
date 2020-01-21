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

fn distance_between(
    orbits: &HashMap<&str, &str>,
    src: &str,
    dst: &str,
) -> i64 {
    let mut depths = HashMap::new();
    let mut d: i64 = 0;
    let mut n = dst;
    while n != "COM" {
        n = orbits.get(n).unwrap();
        d += 1;
        depths.insert(n, d);
    }

    n = src;
    d = 0;
    while n != "COM" {
        n = orbits.get(n).unwrap();
        d += 1;
        if let Some(dist) = depths.get(n) {
            return d + dist - 2;
        }
    }
    -1
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
