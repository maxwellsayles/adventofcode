use std::assert;
use std::fs;

fn run(mut code: Vec<i32>, ip: usize) -> Vec<i32> {
    let instr = code[ip];
    if instr == 1 {
        let x = code[ip + 1];
        let y = code[ip + 2];
        let t = code[ip + 3] as usize;
        code[t] = x + y;
    } else if instr == 2 {
        let x = code[ip + 1];
        let y = code[ip + 2];
        let t = code[ip + 3] as usize;
        code[t] = x * y;
    } else if instr == 99 {
        return code;
    } else {
        assert!(false, "Unknown opcode");
    }
    run(code, ip + 4)
}

fn solve1(mut code: Vec<i32>) -> i32 {
    code[1] = 12;
    code[2] = 2;
    run(code, 0)[0]
}

fn main() {
    let contents = fs::read_to_string("day02.txt")
        .expect("WTF");

    let input: Vec<i32> = contents
        .split(',')
        .map(|s| s.trim().parse::<i32>().expect("WTF"))
        .collect();

    println!("{}", solve1(input.clone()));
}
