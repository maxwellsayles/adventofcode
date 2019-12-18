use std::assert;
use std::fs;

fn run(mut code: Vec<usize>, ip: usize) -> Vec<usize> {
    let instr = code[ip];
    if instr == 1 {
        let x = code[ip + 1];
        let y = code[ip + 2];
        let t = code[ip + 3];
        code[t] = code[x] + code[y];
    } else if instr == 2 {
        let x = code[ip + 1];
        let y = code[ip + 2];
        let t = code[ip + 3];
        code[t] = code[x] * code[y];
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

const TARGET: usize = 19690720;

fn solve2(code: Vec<usize>) -> usize {
    for noun in 0..99 {
        for verb in 0..99 {
            let mut mcode = code.clone();
            mcode[1] = noun;
            mcode[2] = verb;
            if run(mcode, 0)[0] == TARGET {
                return noun * 100 + verb;
            }
        }
    }
    assert!(false, "Failed to find a solution!");
    0
}

fn main() {
    let contents = fs::read_to_string("day02.txt")
        .expect("WTF");

    let input: Vec<usize> = contents
        .split(',')
        .map(|s| s.trim().parse::<usize>().expect("WTF"))
        .collect();

    println!("{}", solve1(input.clone()));
    println!("{}", solve2(input.clone()));
}
