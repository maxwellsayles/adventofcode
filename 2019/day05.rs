use std::assert;
use std::fs;

fn lookup(code: &Vec<i64>, val: i64, mode: i64) -> i64 {
    if mode == 0 {
        code[val as usize]
    } else {
        val
    }
}

fn read_int() -> i64 {
    let mut buffer = String::new();
    std::io::stdin().read_line(&mut buffer).unwrap();
    buffer.trim().parse::<i64>().unwrap()
}

fn run(code: &mut Vec<i64>, ip: usize) {
    let instr = code[ip];
    let mode1 = (instr / 100) % 2;
    let mode2 = (instr / 1000) % 2;
    match instr % 100 {
        1 => {
            let x = lookup(code, code[ip + 1], mode1);
            let y = lookup(code, code[ip + 2], mode2);
            let t = code[ip + 3] as usize;
            code[t] = x + y;
            run(code, ip + 4);
        },
        2 => {
            let x = lookup(code, code[ip + 1], mode1);
            let y = lookup(code, code[ip + 2], mode2);
            let t = code[ip + 3] as usize;
            code[t] = x * y;
            run(code, ip + 4);
        },
        3 => {
            let t = code[ip + 1] as usize;
            code[t] = read_int();
            run(code, ip + 2);
        },
        4 => {
            let x = lookup(code, code[ip + 1], mode1);
            println!("{}", x);
            run(code, ip + 2);
        },
        99 => return,
        _ => assert!(false, "Unknown opcode {}", instr),
    }
}

fn main() {
    let contents = fs::read_to_string("day05.txt")
        .unwrap();
    let mut input: Vec<i64> = contents
        .split(',')
        .map(|s| s.trim().parse::<i64>().unwrap())
        .collect();

    run(&mut input, 0);
}
