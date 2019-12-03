use std::fs;

fn fuel(mass: i32) -> i32 {
    return mass / 3 - 2;
}

fn fuelr(mass: i32, acc: i32) -> i32 {
    let f = mass / 3 - 2;
    if f > 0 {
        return fuelr(f, acc + f);
    } else {
        return acc;
    }
}

fn main() {
    let contents = fs::read_to_string("day01.txt").expect("WTF");
    let lines = contents
        .lines()
        .map(|x| x.parse::<i32>().expect("WTF"));

    let solve1: i32 = lines
        .clone()
        .map(fuel)
        .sum();
    println!("{}", solve1);

    let solve2: i32 = lines
        .clone()
        .map(|x| fuelr(x, 0))
        .sum();
    println!("{}", solve2);
}
