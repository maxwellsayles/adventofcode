use std::assert;
use std::fs;

fn run(mut code: Vec<usize>, ip: usize) -> Vec<usize> {
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
        assert!(false, "Unknown opcode {}", instr);
    }
    run(code, ip + 4)
}

fn solve1(mut code: Vec<usize>) -> usize {
    code[1] = 12;
    code[2] = 2;
    run(code, 0)[0]
}

fn main() {
    let contents = fs::read_to_string("day02.txt")
        .expect("WTF");

    let input: Vec<usize> = contents
        .split(',')
        .map(|s| s.trim().parse::<usize>().expect("WTF"))
        .collect();

    println!("{}", solve1(input.clone()));
}
