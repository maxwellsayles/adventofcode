use std::fs;

fn fuel(mass: u32) -> u32 {
    return mass / 3 - 2;
}

fn main() {
    let contents = fs::read_to_string("day01.txt").expect("WTF");
    let lines = contents
        .lines()
        .map(|x| x.parse::<u32>().expect("WTF"));

    let solve1: u32 = lines
        .map(fuel)
        .sum();
    println!("{}", solve1);
}
