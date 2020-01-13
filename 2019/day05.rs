use std::assert;
use std::fs;

fn run(code: &mut Vec<i64>, ip: usize) {
    let instr = code[ip];
    if instr == 1 {
        let x = code[ip + 1] as usize;
        let y = code[ip + 2] as usize;
        let t = code[ip + 3] as usize;
        code[t] = code[x] + code[y];
    } else if instr == 2 {
        let x = code[ip + 1] as usize;
        let y = code[ip + 2] as usize;
        let t = code[ip + 3] as usize;
        code[t] = code[x] * code[y];
    } else if instr == 99 {
        return;
    } else {
        assert!(false, "Unknown opcode {}", instr);
    }
    run(code, ip + 4)
}

fn main() {
    let contents = fs::read_to_string("day05.txt")
        .unwrap();
    let input: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    println!("{:?}", input);
}
