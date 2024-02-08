use std::fs;

use crate::intcode_computer::IntcodeComputer;

mod intcode_computer;

fn part1(code: &Vec<i64>) -> Vec<i64> {
    let mut comp = IntcodeComputer::new(vec![1], code);
    comp.run();
    let mut res = Vec::new();
    while let Some(x) = comp.remove_output() {
	res.push(x);
    }
    res
}

fn part2(code: &Vec<i64>) -> i64 {
    let mut comp = IntcodeComputer::new(vec![2], code);
    comp.run();
    comp.remove_output().unwrap()
}

fn main() {
    let contents = fs::read_to_string("day09.txt")
        .unwrap();
    let code: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    // let code = vec![109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99];
    // let code = vec![1102,34915192,34915192,7,4,7,99,0];
    // let code = vec![104,1125899906842624,99];

    let outputs = part1(&code);
    let pretty: Vec<String> = outputs.iter().map(|x| x.to_string()).collect();
    println!("{}", pretty.join(","));

    println!("{}", part2(&code));
}
